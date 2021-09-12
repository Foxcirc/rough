
use enumflags2::BitFlags;
use crate::lexer::token::token::*;

pub(crate) type Possible = BitFlags<TokenKind>;

pub(crate) trait Functions {
    unsafe fn clear(&mut self);
    fn has<B: Into<BitFlags<TokenKind>>>(self, other: B) -> bool;
    fn set(&self) -> usize;
    fn only(&self) -> TokenKind;
    fn multichar(&self) -> bool;
    fn update_flag(&mut self, condition: bool, kind: TokenKind);
}

impl Functions for Possible {
    /// Just an alias.
    #[inline(always)]
    fn has<B: Into<BitFlags<TokenKind>>>(self, other: B) -> bool {
        self.contains(other)
    }
    /// Clears all flags.
    /// This may actually be undefined behaviour
    /// but it works so i don't care
    ///
    /// fuck it doesn't work
    /// 
    /// Ah it writes size_of::<T> * count bytes. But whhyyyy?
    unsafe fn clear(&mut self) {
        // this should work since the Bitflags<> struct is transparent
        std::ptr::write_bytes(self, 0, 1);
    }
    
    /// Returns how many flags are set.
    // todo find a lighter approach for this
    fn set(&self) -> usize {
        self.iter().count()
    }

    /// Returns the kind of Token. Mey only be
    /// called if only one item is set. Otherwise
    /// is UnDeFiNeD bEhAVioUr, just kidding
    /// there's a check.
    fn only(&self) -> TokenKind {
        assert!(self.set() == 1);
        self.iter().next().unwrap()
    }

    /// Check if there is a flag, for a multi-character token (Eg. Ident, Float) set.
    fn multichar(&self) -> bool {
        self.has(TokenKind::IntegerDecimal) ||
            self.has(TokenKind::IntegerHexadecimal) ||
            self.has(TokenKind::IntegerBinary) ||
            self.has(TokenKind::Float) ||
            self.has(TokenKind::Identifier) // Every keyword is also a valid identifier.
        }
    fn update_flag(&mut self, cond: bool, kind: TokenKind) { // todo rename etc.
        if cond {
            self.insert(kind);
        } else {
            self.remove(kind)
        }
    }
}

// /// Returns the value of the inner array for this TokenKind as reference.
// impl Index<TokenKind> for Possible {
//     type Output = bool;
//     fn index(&self, index: TokenKind) -> &Self::Output {
//         &self.contains(index)
//     }
// }

// /// Returns the value of the inner array for this TokenKind as mutable reference.
// impl IndexMut<TokenKind> for Possible {
//     fn index_mut(&mut self, index: TokenKind) -> &mut Self::Output {
//         &mut self.contains(index)
//     }
// }
