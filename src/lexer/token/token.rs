
use enumflags2::{bitflags};
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
#[bitflags]
#[repr(u32)]
#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub(crate) enum TokenKind {
    Empty = 1 << 0,
    Newline = 1 << 1,
    
    // Symbols
    SymbolPlus = 1 << 2,
    SymbolMinus = 1 << 3,
    SymbolStar = 1 << 4,
    SymbolSlash = 1 << 5,
    SymbolEqual = 1 << 6,

    // Braces
    BraceNormalOpen = 1 << 7,
    BraceNormalClose = 1 << 8,

    // Integers & Floats
    IntegerDecimal = 1 << 9,
    IntegerHexadecimal = 1 << 10,
    IntegerBinary = 1 << 11,
    Float = 1 << 12,

    Underscore = 1 << 13,
    Identifier = 1 << 14,

    KeywordPackage = 1 << 15
}

impl Display for TokenKind {
    fn fmt(&self, fmt: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        
        fmt.write_str(&match self {
            Self::Empty => format!("Empty Token"),
            Self::Newline => format!("Newline"),
            Self::Float => format!("Float"),
            Self::Underscore => format!("Underscore"),
            Self::Identifier => format!("Identifier"),
            _ => format!("Other") // todo add all variants
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

