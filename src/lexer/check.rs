
//! The main character-matching hapens here.
//! The only place where the `TokenKind` is changed, is inside
//! `lexer.rs` (around line 130) during the token sysnthesis, where
//! some invariants that can occur here are matched.

use crate::lexer::possible::{Possible, Functions};
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

        // "This is the start of a new token"
        let start = self.set() == 0;

        self.update_flag(matches!(chr, '\n') && start, TokenKind::Newline);

        // self[TokenKind::Keyword(Keyword::Package)]

        self.update_flag(chr == '+' && start, TokenKind::SymbolPlus);
        self.update_flag(chr == '-' && start, TokenKind::SymbolMinus);
        self.update_flag(chr == '*' && start, TokenKind::SymbolStar);
        self.update_flag(chr == '/' && start, TokenKind::SymbolSlash);
        self.update_flag(chr == '=' && start, TokenKind::SymbolEqual);
        self.update_flag(chr == '(' && start, TokenKind::BraceNormalOpen);
        self.update_flag(chr == ')' && start, TokenKind::BraceNormalClose);

        self.update_flag(matches!(chr, '0'..='9' | 'a'..='f' | 'A'..='F' | 'x' | '_') && ((self.has(TokenKind::IntegerDecimal) && chr == 'x' /* && prev == '0' */) || self.has(TokenKind::IntegerHexadecimal)), TokenKind::IntegerHexadecimal);
        self.update_flag(matches!(chr, '0' | '1' | '_' | 'b')                         && ((self.has(TokenKind::IntegerDecimal) && chr == 'b' /* && prev == '0' */) || self.has(TokenKind::IntegerBinary)), TokenKind::IntegerBinary);
        
        self.update_flag(matches!(chr, '0'..='9' | '_' | '.') && ((start && chr != '_') || self.has(TokenKind::IntegerDecimal) || self.has(TokenKind::Float)), TokenKind::Float);

        // this needs to be last, because other tokens depend on it (eg. Integer with Integerbase::Hexadecimal)
        self.update_flag(matches!(chr, '0'..='9' | '_')       && ((start && chr != '_') || self.has(TokenKind::IntegerDecimal)), TokenKind::IntegerDecimal);

        self.update_flag(chr == '_'                           && start, TokenKind::Underscore);

        self.update_flag((chr.is_ascii_alphanumeric() || chr == '_') && ((start && !chr.is_ascii_digit()) || self.has(TokenKind::Identifier)), TokenKind::Identifier);

        // todo add keywords

    }

    fn terminator(&self, chr: char, _buffer: &String) -> bool {

        assert!(self.set() == 1);
        
        if chr == ' ' || chr == '\n' || chr == '\t' { return true } // these are considered valid terminators for every token

        return match self.iter().next().unwrap() { // it is done this way so I think about every one of these boiis
            
            TokenKind::Empty   => true,
            TokenKind::Newline => true,
            
            TokenKind::SymbolPlus  => true,
            TokenKind::SymbolMinus => true,
            TokenKind::SymbolStar  => true,
            TokenKind::SymbolSlash => true,
            TokenKind::SymbolEqual => true,
            
            TokenKind::BraceNormalOpen  => true,
            TokenKind::BraceNormalClose => true,

            TokenKind::IntegerDecimal |
                TokenKind::IntegerHexadecimal |
                TokenKind::IntegerBinary |
                TokenKind::Float => matches!(chr, '+' | '-' | '*' | '/' | '(' | ')') ,
            
            TokenKind::Underscore => false,
            
            TokenKind::Identifier => matches!(chr, '(' | ')'),
            TokenKind::KeywordPackage => true,
        }
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
