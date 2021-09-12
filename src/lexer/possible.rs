
use std::ops::{Index, IndexMut};
use crate::lexer::token::token::*;

// todo update this
/// Basically Bitflags, but with the whole "bit" thing removed.
/// 
/// Indexing by TokenKind will return the corresponding
/// integer index into the inner buffer.
/// -----------------------  Layout ---------------------------o
/// [0]  => TokenKind::Empty                                   │
/// [1]  => TokenKind::Newline                                 │
/// [2]  => TokenKind::Symbol(Symbol::Plus)                    │
/// [3]  => TokenKind::Symbol(Symbol::Minus)                   │
/// [4]  => TokenKind::Symbol(Symbol::Star)                    │
/// [5]  => TokenKind::Symbol(Symbol::Slash)                   │
/// [6]  => TokenKind::Symbol(Symbol::Equal)                   │
/// [7]  => TokenKind::Brace(Brace::NormalOpen)                │
/// [8]  => TokenKind::Brace(Brace::NormalClose)               │
/// [9]  => TokenKind::Integer(IntegerBase::Decimal)           │
/// [10] => TokenKind::Integer(IntegerBase::Hexadecimal)       │
/// [11] => TokenKind::Integer(IntegerBase::Binary)            │
/// [12] => TokenKind::Float                                   │
/// [13] => TokenKind::Underscore                              │
/// [14] => TokenKind::Identifier                              │
/// [15] => TokenKind::Keyword(Keyword::Package)               │
/// -----------------------------------------------------------o
#[derive(Debug)]
pub(crate) struct Possible([bool; 16]);

impl Default for Possible {
    fn default() -> Self {
        Self([false; 16])
    }
}

impl Possible {
    /// Clears all flags.
    /// This may actually be undefined behaviour
    /// but it works so i don't care
    ///
    /// fuck it doesn't work
    /// 
    /// Ah it writes size_of::<T> * count bytes. But whhyyyy?
    pub(crate) unsafe fn clear(&mut self) {
        std::ptr::write_bytes(self, 0, 1);
    }
    
    /// Returns how many flags are set.
    // todo find a good lighter approach for this
    pub(crate) fn set(&self) -> usize {
        self.0.iter().filter(|i| **i).count()
    }

    /// Returns the kind of Token. Mey only be
    /// called if only one item is set. Otherwise
    /// is UnDeFiNeD bEhAVioUr, just kidding
    /// there's a check.
    pub(crate) fn only(&self) -> TokenKind {
        assert!(self.set() == 1);
        
        return
            
            if      self[TokenKind::Empty]   { TokenKind::Empty }
            else if self[TokenKind::Newline] { TokenKind::Newline }
            
            else if self[TokenKind::Symbol(Symbol::Plus)]              { TokenKind::Symbol(Symbol::Plus) }
            else if self[TokenKind::Symbol(Symbol::Minus)]             { TokenKind::Symbol(Symbol::Minus) }
            else if self[TokenKind::Symbol(Symbol::Star)]              { TokenKind::Symbol(Symbol::Star) }
            else if self[TokenKind::Symbol(Symbol::Slash)]             { TokenKind::Symbol(Symbol::Slash) }
            else if self[TokenKind::Symbol(Symbol::Equal)]             { TokenKind::Symbol(Symbol::Equal) }
            
            else if self[TokenKind::Brace(Brace::NormalOpen)]          { TokenKind::Brace(Brace::NormalOpen) }
            else if self[TokenKind::Brace(Brace::NormalClose)]         { TokenKind::Brace(Brace::NormalClose) }

            else if self[TokenKind::Integer(IntegerBase::Decimal)]     { TokenKind::Integer(IntegerBase::Decimal) }
            else if self[TokenKind::Integer(IntegerBase::Hexadecimal)] { TokenKind::Integer(IntegerBase::Hexadecimal) }
            else if self[TokenKind::Integer(IntegerBase::Binary)]      { TokenKind::Integer(IntegerBase::Binary) }
            else if self[TokenKind::Float]                             { TokenKind::Float }
            
            else if self[TokenKind::Underscore]                        { TokenKind::Underscore }
            
            else if self[TokenKind::Identifier]                        { TokenKind::Identifier }
            else if self[TokenKind::Keyword(Keyword::Package)]         { TokenKind::Keyword(Keyword::Package) }
            
            else { panic!("Forgot to add the new Index here.") };
    }

    /// Check if there is a flag, for a multi-character token (Eg. Ident, Float) set.
    pub(crate) fn multichar(&self) -> bool {
        self[TokenKind::Integer(IntegerBase::Decimal)] ||
            self[TokenKind::Integer(IntegerBase::Hexadecimal)] ||
            self[TokenKind::Integer(IntegerBase::Binary)] ||
            self[TokenKind::Float] ||
            self[TokenKind::Identifier] // Every keyword is also a valid identifier.
        }
}

/// Returns the value of the inner array for this TokenKind as reference.
impl Index<TokenKind> for Possible {
    type Output = bool;
    fn index(&self, index: TokenKind) -> &Self::Output {
        match index {
            TokenKind::Empty                             => &self.0[0],
            TokenKind::Newline                           => &self.0[1],
            
            TokenKind::Symbol(Symbol::Plus)              => &self.0[2],
            TokenKind::Symbol(Symbol::Minus)             => &self.0[3],
            TokenKind::Symbol(Symbol::Star)              => &self.0[4],
            TokenKind::Symbol(Symbol::Slash)             => &self.0[5],
            TokenKind::Symbol(Symbol::Equal)             => &self.0[6],
            TokenKind::Brace(Brace::NormalOpen)          => &self.0[7],
            TokenKind::Brace(Brace::NormalClose)         => &self.0[8],
            
            TokenKind::Integer(IntegerBase::Decimal)     => &self.0[9],
            TokenKind::Integer(IntegerBase::Hexadecimal) => &self.0[10],
            TokenKind::Integer(IntegerBase::Binary)      => &self.0[11],
            TokenKind::Float                             => &self.0[12],
            
            TokenKind::Underscore                        => &self.0[13],
            
            TokenKind::Identifier                        => &self.0[14],
            TokenKind::Keyword(Keyword::Package)         => &self.0[15]
        }
    }
}

/// Returns the value of the inner array for this TokenKind as mutable reference.
impl IndexMut<TokenKind> for Possible {
    fn index_mut(&mut self, index: TokenKind) -> &mut Self::Output {
        match index {
            TokenKind::Empty                             => &mut self.0[0],
            TokenKind::Newline                           => &mut self.0[1],
           
            TokenKind::Symbol(Symbol::Plus)              => &mut self.0[2],
            TokenKind::Symbol(Symbol::Minus)             => &mut self.0[3],
            TokenKind::Symbol(Symbol::Star)              => &mut self.0[4],
            TokenKind::Symbol(Symbol::Slash)             => &mut self.0[5],
            TokenKind::Symbol(Symbol::Equal)             => &mut self.0[6],
            TokenKind::Brace(Brace::NormalOpen)          => &mut self.0[7],
            TokenKind::Brace(Brace::NormalClose)         => &mut self.0[8],
           
            TokenKind::Integer(IntegerBase::Decimal)     => &mut self.0[9],
            TokenKind::Integer(IntegerBase::Hexadecimal) => &mut self.0[10],
            TokenKind::Integer(IntegerBase::Binary)      => &mut self.0[11],
            TokenKind::Float                             => &mut self.0[12],
           
            TokenKind::Underscore                        => &mut self.0[13],
            
            TokenKind::Identifier                        => &mut self.0[14],
            TokenKind::Keyword(Keyword::Package)         => &mut self.0[15]
        }
    }
}
