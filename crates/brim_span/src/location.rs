use crate::index::{ColumnIndex, LineIndex};

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Location {
    pub line: LineIndex,
    pub column: ColumnIndex,
}

impl Location {
    pub fn new(line: impl Into<LineIndex>, column: impl Into<ColumnIndex>) -> Location {
        Location { line: line.into(), column: column.into() }
    }
}
