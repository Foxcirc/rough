
use std::fmt::{Display, Formatter, Result as FmtResult};

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub(crate) struct Pos {
    /// The current column.
    pub(crate) column: usize,
    /// The current line.
    pub(crate) line: usize,
    /// All "columns"
    pub(crate) all: usize
}

impl Display for Pos {

    fn fmt(&self, fmt: &mut Formatter<'_>) -> FmtResult {

        if cfg!(lexer_err_full_pos) {
            fmt.write_str(&format!("{}:{}:{}", self.line, self.column, self.all))
        }
        else {
            fmt.write_str(&format!("{}:{}", self.line, self.column))
        }
    }


}

impl Pos {
    pub(crate) fn new() -> Self {
        Self { column: 0, line: 0, all: 0 }
    }
}
