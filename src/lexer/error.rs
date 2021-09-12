
use std::fmt::{Display, Formatter, Result as FmtResult};
use crate::lexer::pos::Pos;
use crate::lexer::token::token::TokenKind;

pub(crate) type LexResult<T> = Result<T, LexError>;

#[derive(Debug, Clone, Eq, PartialEq)]
pub(crate) enum LexError {
    InvalidSequence {
        /// The position in the input the error occured.
        pos: Pos,
        /// The kind of Token that should have beed built.
        target: TokenKind,
        /// The sequence of characters the token was build from. (Eg. `valid`)
        seq: String,
        /// The original sequence without anything removed. (Eg. `self.buffer`)
        orig: String
    },
    EndOfInput
}

impl Display for LexError {

    fn fmt(&self, fmt: &mut Formatter<'_>) -> FmtResult {

        match self {
            LexError::InvalidSequence { pos, target, seq, orig } => {
                fmt.write_str(&format!("Invalid sequence at {} for {} build from '{}' wich originates from sequence '{}'", pos, target, seq, orig))
            },
            LexError::EndOfInput => {
                fmt.write_str("End of input.")
            }
        }

    }

}
