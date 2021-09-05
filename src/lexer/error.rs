
use crate::lexer::{constants::Pos, token::token::TokenKind};
use std::fmt::{Display, Formatter, Error as FormatError};
use std::collections::VecDeque;

/// An Error generated by the Lexer
#[derive(Debug, Clone)]
pub(crate) struct Error {
    pub(crate) kind: ErrorKind,
    pub(crate) trace: Traceback
}

impl Display for Error {
    fn fmt(&self, fmt: &mut Formatter<'_>) -> Result<(), FormatError> {
        fmt.write_str("Error: ")?;
        Display::fmt(&self.kind, fmt)?;
        // fmt.write_str("\n");
        Ok(())
    }
}

impl std::error::Error for Error {
}

#[derive(Debug, Clone)]
pub(crate) enum ErrorKind {
    /// Invalid sequence while parsing Eg. an integer.
    InvalidSequence { kind: TokenKind, seq: String, pos: Pos /* [line, colum, char] */ },
    /// An invalid character wich doesn't match to any token.
    InvalidChar { chr: char, pos: Pos /* [line, colum, char] */ },
}

impl Display for ErrorKind {
    fn fmt(&self, fmt: &mut Formatter<'_>) -> Result<(), FormatError> {

        match self {
            Self::InvalidSequence { kind, seq, pos } => {
                fmt.write_str(&format!("Invalid sequence \"{}\" for token of kind {} at {}:{}", seq, kind, pos[0], pos[1]))?;
            },
            Self::InvalidChar { chr, pos } => {
                fmt.write_str(&format!("Invalid character '{}' at {}:{}", chr, pos[0], pos[1]))?;
            }
        };
        Ok(())

    }
}

#[derive(Debug, Clone)]
pub(crate) struct Traceback {
    steps: VecDeque<Log>
}

#[derive(Debug, Clone)]
pub(crate) struct Log {
    
}
