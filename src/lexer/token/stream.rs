
use std::fmt::{Display, Formatter};
use crate::lexer::token::token::Token;

/// Represents multiple tokens chained together.
#[repr(transparent)]
#[derive(Debug, Clone, PartialEq)]
pub(crate) struct TokenStream(Vec<Token>);

impl TokenStream {
    pub(crate) fn new() -> Self {
        TokenStream(Vec::new())
    }
    #[inline]
    pub(crate) fn push(&mut self, token: Token) {
        self.0.push(token)
    }
}

impl From<Vec<Token>> for TokenStream {
    fn from(vec: Vec<Token>) -> Self {
        Self(vec)
    }
}

impl Display for TokenStream {
    fn fmt(&self, fmt: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {

        fmt.write_str(&format!("> TokenStream with {} Tokens: <\n", self.0.len()))?;
        for token in self.0.iter() {
            
            if *token == Token::Newline { 
                fmt.write_str("           ")?;
            } else {
                fmt.write_str("         ")?;

            }
            
            Display::fmt(token, fmt)?;
            fmt.write_str("\n")?;
            
        };
        
        Ok(())
    }
}
