
use std::iter::Peekable;
use std::str::Chars;
use crate::lexer::types::Pos;

/// Increment the `Pos` and return the next element of self.
pub(crate) trait Tick {
    type Output;
    fn tick(&mut self, counter: &mut Pos) -> Option<Self::Output>;
}

// Could be "impl Tick for T: Iterator {...}"
impl Tick for Peekable<Chars<'_>> {
    
    type Output = char;

    fn tick(&mut self, counter: &mut Pos) -> Option<Self::Output> {
        if let Some(chr) = self.next() {
            counter.column += 1;
            counter.all += 1;
            if chr == '\n' {
                counter.column = 0;
                counter.line += 1
            }
            return Some(chr)
        };
        None
    }

}
