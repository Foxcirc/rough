
use std::iter::Peekable;
use std::str::Chars;
use crate::lexer::types::Pos;

/// Increment the `Pos` and return the next element.
pub(crate) trait Tick {

    fn tick(&mut self, counter: &mut )

}

impl Tick for Peekable<Chars<'_>> {



}
