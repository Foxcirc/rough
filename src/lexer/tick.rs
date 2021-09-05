
use std::iter::Peekable;
use std::str::Chars;
use crate::lexer::types::Pos;

/// Increment the `Pos` and return the next element of self.
pub(crate) trait Tick {

    fn tick(&mut self, counter: &mut Pos) {

    }

}

impl Tick for Peekable<Chars<'_>> {



}
