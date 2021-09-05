
use std::iter::Peekable;
use std::str::Chars;
use crate::lexer::pos::Pos;

/// Increment the `Pos` and return the next element of self.
pub(crate) trait Tick {
    type Output;
    fn tick(&mut self) -> Option<Self::Output>;
}

impl Tick for (Peekable<Chars<'_>>, Pos) {
    
    type Output = char;

    fn tick(&mut self) -> Option<Self::Output> {
        if let Some(chr) = self.0.next() {
            self.1.column += 1;
            self.1.all += 1;
            if chr == '\n' {
                self.1.column = 0;
                self.1.line += 1
            }
            return Some(chr)
        };
        None
    }

}
