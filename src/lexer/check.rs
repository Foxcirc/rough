
use crate::lexer::possible::Possible;
use crate::lexer::token::token::*;

/// Implemented for Possible.
/// This trait abstracts the checking if specific combinations of chars
/// are valid for a specific TokenKind.
trait Check {
    /// Update the flags according to chr.
    fn update(&mut self, chr: char, prev: char);
    /// Peek how many flags would be valid after a call to Char::update(chr).
    fn peek(&self, chr: char, prev: char) -> usize;
}

impl Check for Possible {
    fn update(&mut self, chr: char, _prev: char) {

        /*
            self[TokenKind::Something] = valid!{ // TODO Program this macro. 
                valid('a' | 'b' | 'c'); // what chars are valid
                previous('1' | '2'); // what previous chars are valid
                set(TokenKind::Other & TokenKind::What) // what flags have to be already set
            }
        */

        let set = self.set();
        // todo Put !self[TokenKind::Integer] into the float match, since a float isn't a valid integer.
        // todo This would make the places matching is done, more consistent.
        self[TokenKind::Newline]                   = matches!(chr, '\n') && set == 0;

        self[TokenKind::Symbol(Symbol::Plus)]      = matches!(chr, '+') && set == 0;
        self[TokenKind::Symbol(Symbol::Minus)]     = matches!(chr, '-') && set == 0;
        self[TokenKind::Symbol(Symbol::Star)]      = matches!(chr, '*') && set == 0;
        self[TokenKind::Symbol(Symbol::Slash)]     = matches!(chr, '/') && set == 0;
        self[TokenKind::Symbol(Symbol::Equal)]     = matches!(chr, '=') && set == 0;
        self[TokenKind::Brace(Brace::NormalOpen)]  = matches!(chr, '(') && set == 0;
        self[TokenKind::Brace(Brace::NormalClose)] = matches!(chr, ')') && set == 0;
       
        self[TokenKind::Integer]                   = matches!(chr, '0'..='9' | '_')       && (set == 0 || self[TokenKind::Integer]);
        self[TokenKind::Float]                     = matches!(chr, '0'..='9' | '_' | '.') && (set == 0 || self[TokenKind::Integer] || self[TokenKind::Float]);
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
       
        count += (matches!(chr, '0'..='9' | '_')       && (set == 0 || self[TokenKind::Integer])) as usize;
        count += (matches!(chr, '0'..='9' | '_' | '.') && (set == 0 || !self[TokenKind::Float] || self[TokenKind::Integer])) as usize;

        return count

    }
}
