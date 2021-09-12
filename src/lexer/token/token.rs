
use std::fmt::{Display, Formatter};

/// Represents a single syntax-token.
#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Token {
    Empty,
    Newline,
    Symbol(Symbol),
    Brace(Brace),
    Integer(isize),
    Float(f32),
    Underscore,
    Identifier(String),
    Keyword(Keyword),
}

impl Display for Token {
    fn fmt(&self, fmt: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        
        fmt.write_str(&match self {
            Self::Empty => format!("Empty Token"),
            Self::Newline => format!("Newline"),
            Self::Symbol(v) => format!("Symbol({})", v),
            Self::Brace(v) => format!("Brace({})", v),
            Self::Integer(v) => format!("Integer({})", v),
            Self::Float(v) => format!("Float({})", v),
            Self::Underscore => format!("Underscore"),
            Self::Identifier(v) => format!("Identifier('{}')", v),
            Self::Keyword(v) => format!("Keyword({})", v),
        })?;
        Ok(())
    }
}

/// Represents a kind of Token.
#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub(crate) enum TokenKind {
    Empty,
    Newline,
    Symbol(Symbol),
    Brace(Brace),
    Integer(IntegerBase),
    Float,
    Underscore, // a single underscore
    Identifier,
    Keyword(Keyword)
}

impl Display for TokenKind {
    fn fmt(&self, fmt: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        
        fmt.write_str(&match self {
            Self::Empty => format!("Empty Token"),
            Self::Newline => format!("Newline"),
            Self::Symbol(v) => format!("Symbol({})", v),
            Self::Brace(v) => format!("Brace({})", v),
            Self::Integer(v) => format!("Integer({})", v),
            Self::Float => format!("Float"),
            Self::Underscore => format!("Underscore"),
            Self::Identifier => format!("Identifier"),
            Self::Keyword(v) => format!("Keyword({})", v),
        })?;
        Ok(())
    }
}


#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub(crate) enum Keyword {
    Package
}

impl Display for Keyword {
    fn fmt(&self, fmt: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        fmt.write_str(match self {
            Self::Package => "Package",
        })
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub(crate) enum IntegerBase {
    Decimal,
    Hexadecimal,
    Binary
}

impl Display for IntegerBase {
    fn fmt(&self, fmt: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        fmt.write_str(match self {
            Self::Decimal => "Base(10)",
            Self::Hexadecimal => "Base(16)",
            Self::Binary => "Base(2)",
        })
    }
}

/// A symbol like '+', '/', '*=', '&'
#[derive(Debug, Clone, Copy, Eq, PartialEq)]
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
#[derive(Debug, Clone, Copy, Eq,  PartialEq)]
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

