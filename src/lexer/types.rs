
pub(crate) struct Pos {
    /// The current column.
    pub(crate) column: usize,
    /// The current line.
    pub(crate) line: usize,
    /// All "columns"
    pub(crate) all: usize
}

impl Pos {
    pub(crate) fn new() -> Self {
        Self { column: 0, line: 0, all: 0 }
    }
}
