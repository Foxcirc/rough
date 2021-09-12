
//! The main character-matching hapens here.
//! The only place where the `TokenKind` is changed, is inside
//! `lexer.rs` (around line 130) during the token sysnthesis, where
//! some invariants that can occur here are matched.

use crate::lexer::possible::Possible;
use crate::lexer::token::token::*;

/// Implemented for Possible.
/// This trait abstracts the checking if specific combinations of chars
/// are valid for a specific TokenKind.
pub(crate) trait Check {
    /// Update the flags according to chr.
    fn update(&mut self, chr: char, buffer: &String);
    /// Chekcs if this char is a valid terminator for the currently active tokens.
    fn terminator(&self, chr: char, buffer: &String) -> bool;
}

impl Check for Possible {
    fn update(&mut self, chr: char, _buffer: &String) {

        /*
            self[TokenKind::Something] = valid!{ // TODO Make this macro. (or just a function xD)
                valid('a' | 'b' | 'c'); // what chars are valid
                previous('1' | '2'); // what previous chars are valid
                set(TokenKind::Other & TokenKind::What) // what flags have to be already set
            }

            or, also
            self[TokenKind::Keyword(Keyword::While)] = keyword!("while")
        */

        // "This is the start of a new token"
        let start = self.set() == 0;

        self[TokenKind::Newline]                           = matches!(chr, '\n') && start;

        // self[TokenKind::Keyword(Keyword::Package)]

        self[TokenKind::Symbol(Symbol::Plus)]              = chr == '+' && start;
        self[TokenKind::Symbol(Symbol::Minus)]             = chr == '-' && start;
        self[TokenKind::Symbol(Symbol::Star)]              = chr == '*' && start;
        self[TokenKind::Symbol(Symbol::Slash)]             = chr == '/' && start;
        self[TokenKind::Symbol(Symbol::Equal)]             = chr == '=' && start;
        self[TokenKind::Brace(Brace::NormalOpen)]          = chr == '(' && start;
        self[TokenKind::Brace(Brace::NormalClose)]         = chr == ')' && start;

        self[TokenKind::Integer(IntegerBase::Hexadecimal)] = matches!(chr, '0'..='9' | 'a'..='f' | 'A'..='F' | 'x' | '_') && ((self[TokenKind::Integer(IntegerBase::Decimal)] && chr == 'x' /* && prev == '0' */) || self[TokenKind::Integer(IntegerBase::Hexadecimal)]);
        self[TokenKind::Integer(IntegerBase::Binary)]      = matches!(chr, '0' | '1' | '_' | 'b')                         && ((self[TokenKind::Integer(IntegerBase::Decimal)] && chr == 'b' /* && prev == '0' */) || self[TokenKind::Integer(IntegerBase::Binary)]);
        
        self[TokenKind::Float]                             = matches!(chr, '0'..='9' | '_' | '.') && ((start && chr != '_') || self[TokenKind::Integer(IntegerBase::Decimal)] || self[TokenKind::Float]);

        // this needs to be last, because other tokens depend on it (eg. Integer with Integerbase::Hexadecimal)
        self[TokenKind::Integer(IntegerBase::Decimal)]     = matches!(chr, '0'..='9' | '_')       && ((start && chr != '_') || self[TokenKind::Integer(IntegerBase::Decimal)]);
        
        self[TokenKind::Underscore]                        = chr == '_'                           && start;

        self[TokenKind::Identifier]                        = (chr.is_ascii_alphanumeric() || chr == '_') && ((start && !chr.is_ascii_digit()) || self[TokenKind::Identifier])

        // todo add keywords

    }

    fn terminator(&self, chr: char, _buffer: &String) -> bool {

        assert!(self.set() == 1);

        return
            
            if chr == ' ' || chr == '\n' || chr == '\t' { true } // these are considered valid terminators for every token

            else if      self[TokenKind::Empty]   { true }
            else if self[TokenKind::Newline] { true }
            
            else if self[TokenKind::Symbol(Symbol::Plus)]              { true }
            else if self[TokenKind::Symbol(Symbol::Minus)]             { true }
            else if self[TokenKind::Symbol(Symbol::Star)]              { true }
            else if self[TokenKind::Symbol(Symbol::Slash)]             { true }
            else if self[TokenKind::Symbol(Symbol::Equal)]             { true }
            
            else if self[TokenKind::Brace(Brace::NormalOpen)]          { true }
            else if self[TokenKind::Brace(Brace::NormalClose)]         { true }

            else if self[TokenKind::Integer(IntegerBase::Decimal)] ||
                    self[TokenKind::Integer(IntegerBase::Hexadecimal)] ||
                    self[TokenKind::Integer(IntegerBase::Binary)] ||
                    self[TokenKind::Float]                             { matches!(chr, '+' | '-' | '*' | '/' | '(' | ')') }
            
            else if self[TokenKind::Underscore]                        { false }
            
            else if self[TokenKind::Identifier]                        { matches!(chr, '(' | ')') }
            else if self[TokenKind::Keyword(Keyword::Package)]         { true }
            
            else { false };
        
    }

/*     fn peek(&self, chr: char, _prev: char) -> usize {

        let mut count = 0;
        let set = self.set();

        count += (matches!(chr, '\n') && start) as usize;
        count += (matches!(chr, '+') && start) as usize;
        count += (matches!(chr, '-') && start) as usize;
        count += (matches!(chr, '*') && start) as usize;
        count += (matches!(chr, '/') && start) as usize;
        count += (matches!(chr, '=') && start) as usize;
        count += (matches!(chr, '(') && start) as usize;
        count += (matches!(chr, ')') && start) as usize;

        count += (matches!(chr, '0'..='9' | 'a'..='f' | 'A'..='F' | 'x' | '_') && ((self[TokenKind::Integer(IntegerBase::Decimal)] && chr == 'x' /* && prev == '0' */) || self[TokenKind::Integer(IntegerBase::Hexadecimal)])) as usize;
        count += (matches!(chr, '0' | '1' | '_' | 'b') && ((self[TokenKind::Integer(IntegerBase::Decimal)] && chr == 'b' /* && prev == '0' */) || self[TokenKind::Integer(IntegerBase::Binary)])) as usize;
    
        count += (matches!(chr, '0'..='9' | '_' | '.') && (start || self[TokenKind::Integer(IntegerBase::Decimal)] || self[TokenKind::Float])) as usize;
       
        count += (matches!(chr, '0'..='9' | '_')       && (start || self[TokenKind::Integer(IntegerBase::Decimal)])) as usize;

        return count

    } */

}
