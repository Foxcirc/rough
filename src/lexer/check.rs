
use crate::lexer::possible::Possible;
use crate::lexer::token::token::*;

/// Implemented for Possible.
/// This trait abstracts the checking if specific combinations of chars
/// are valid for a specific TokenKind.
pub(crate) trait Check {
    /// Update the flags according to chr.
    fn update(&mut self, chr: char, prev: char);
    /// Peek how many flags would be valid after a call to Char::update(chr).
    fn peek(&self, chr: char, prev: char) -> usize;
}

impl Check for Possible {
    fn update(&mut self, chr: char, _prev: char) {

        /*
            self[TokenKind::Something] = valid!{ // TODO Make this macro. (or just a function xD)
                valid('a' | 'b' | 'c'); // what chars are valid
                previous('1' | '2'); // what previous chars are valid
                set(TokenKind::Other & TokenKind::What) // what flags have to be already set
            }

            or, also
            self[TokenKind::Keyword(Keyword::While)] = keyword!("while")
        */

        let set = self.set();

        self[TokenKind::Newline]                           = matches!(chr, '\n') && set == 0;

        self[TokenKind::Symbol(Symbol::Plus)]              = matches!(chr, '+') && set == 0;
        self[TokenKind::Symbol(Symbol::Minus)]             = matches!(chr, '-') && set == 0;
        self[TokenKind::Symbol(Symbol::Star)]              = matches!(chr, '*') && set == 0;
        self[TokenKind::Symbol(Symbol::Slash)]             = matches!(chr, '/') && set == 0;
        self[TokenKind::Symbol(Symbol::Equal)]             = matches!(chr, '=') && set == 0;
        self[TokenKind::Brace(Brace::NormalOpen)]          = matches!(chr, '(') && set == 0;
        self[TokenKind::Brace(Brace::NormalClose)]         = matches!(chr, ')') && set == 0;

        self[TokenKind::Integer(IntegerBase::Hexadecimal)] = matches!(chr, '0'..='9' | 'a'..='f' | 'A'..='F' | 'x' | '_') && ((self[TokenKind::Integer(IntegerBase::Decimal)] && chr == 'x' /* && prev == '0' */) || self[TokenKind::Integer(IntegerBase::Hexadecimal)]);
        self[TokenKind::Integer(IntegerBase::Binary)]      = matches!(chr, '0' | '1' | '_' | 'b') && ((self[TokenKind::Integer(IntegerBase::Decimal)] && chr == 'b' /* && prev == '0' */) || self[TokenKind::Integer(IntegerBase::Binary)]);
        
        self[TokenKind::Float]                             = matches!(chr, '0'..='9' | '_' | '.') && (set == 0 || self[TokenKind::Integer(IntegerBase::Decimal)] || self[TokenKind::Float]);

        // this need to be last, because other tokens depend on it (eg. Integer with Integerbase::Hexadecimal)
        self[TokenKind::Integer(IntegerBase::Decimal)]     = matches!(chr, '0'..='9' | '_')       && (set == 0 || self[TokenKind::Integer(IntegerBase::Decimal)]);
    }
    fn peek(&self, chr: char, _prev: char) -> usize {

        let mut count = 0;
        let set = self.set();

        count += (matches!(chr, '\n') && set == 0) as usize;
        count += (matches!(chr, '+') && set == 0) as usize;
        count += (matches!(chr, '-') && set == 0) as usize;
        count += (matches!(chr, '*') && set == 0) as usize;
        count += (matches!(chr, '/') && set == 0) as usize;
        count += (matches!(chr, '=') && set == 0) as usize;
        count += (matches!(chr, '(') && set == 0) as usize;
        count += (matches!(chr, ')') && set == 0) as usize;

        count += (matches!(chr, '0'..='9' | 'a'..='f' | 'A'..='F' | 'x' | '_') && ((self[TokenKind::Integer(IntegerBase::Decimal)] && chr == 'x' /* && prev == '0' */) || self[TokenKind::Integer(IntegerBase::Hexadecimal)])) as usize;
        count += (matches!(chr, '0' | '1' | '_' | 'b') && ((self[TokenKind::Integer(IntegerBase::Decimal)] && chr == 'b' /* && prev == '0' */) || self[TokenKind::Integer(IntegerBase::Binary)])) as usize;
    
        count += (matches!(chr, '0'..='9' | '_' | '.') && (set == 0 || self[TokenKind::Integer(IntegerBase::Decimal)] || self[TokenKind::Float])) as usize;
       
        count += (matches!(chr, '0'..='9' | '_')       && (set == 0 || self[TokenKind::Integer(IntegerBase::Decimal)])) as usize;

        return count

    }
}
