
use std::fmt::{Display, Formatter};

/// Represents a single syntax-token.
#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) enum Token {
    Empty,
    Newline,
    Symbol(Symbol),
    Brace(Brace),
    Integer(isize),
    Float(f32),
}

impl Display for Token {
    fn fmt(&self, fmt: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        
        fmt.write_str(&match self {
            Self::Empty => "Empty Token".to_owned(),
            Self::Newline => "Newline".to_owned(),
            Self::Symbol(v) => format!("Symbol({})", v),
            Self::Brace(v) => format!("Brace({})", v),
            Self::Integer(v) => format!("Integer({})", v),
            Self::Float(v) => format!("Float({})", v),
        })?;
        Ok(())
    }
}

/// Represents a kind of Token.
#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) enum TokenKind {
    Empty,
    Newline,
    Symbol(Symbol),
    Brace(Brace),
    Integer,
    Float,
}

impl Display for TokenKind {
    fn fmt(&self, fmt: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        
        fmt.write_str(&match self {
            Self::Empty => "Empty Token".to_owned(),
            Self::Newline => "Newline".to_owned(),
            Self::Symbol(v) => format!("Symbol({})", v),
            Self::Brace(v) => format!("Brace({})", v),
            Self::Integer => format!("Integer"),
            Self::Float => format!("Float"),
        })?;
        Ok(())
    }
}

/// A symbol like '+', '/', '*=', '&'
#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) enum Symbol {
    Plus, // "+"
    Minus, // "-
    Star, // Asterix "*"
    Slash, // Foreward "/"
    Equal, // "="
}

impl Display for Symbol {
    fn fmt(&self, fmt: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        fmt.write_str(match self {
            Self::Plus => "Plus",
            Self::Minus => "Minus",
            Self::Star => "Star",
            Self::Slash => "Foreward Slash",
            Self::Equal => "Equal",
        })
    }
}

/// A brace / paranthese like '(', '}', '[' ('<' is a Symbol) 
#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) enum Brace {
    NormalOpen, // "("
    NormalClose // ")"
}

impl Display for Brace {
    fn fmt(&self, fmt: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        fmt.write_str(match self {
            Self::NormalOpen => "Normal Opening",
            Self::NormalClose => "Normal Closing"
        })
    }
}

