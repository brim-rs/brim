use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Position {
    pub line: u32,
    pub column: u32,
    pub index: usize,
}

impl Position {
    pub fn new(line: u32, column: u32, index: usize) -> Self {
        Self {
            line,
            column,
            index,
        }
    }

    pub fn increment_line(&mut self) {
        self.line += 1;
        self.column = 1;
        self.index += 1;
    }

    pub fn increment_column(&mut self) {
        self.column += 1;
        self.index += 1;
    }

    pub fn line(&self) -> u32 {
        self.line
    }

    pub fn column(&self) -> u32 {
        self.column
    }

    pub fn index(&self) -> usize {
        self.index
    }

    pub fn move_right(&self, count: usize) -> Position {
        Position::new(self.line, self.column + count as u32, self.index + count)
    }

    pub fn move_left(&self, start: usize, count: usize) -> Position {
        Position::new(self.line, (start + count) as u32, self.index - count)
    }
}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{} (index: {})", self.line, self.column, self.index)
    }
}

impl Default for Position {
    fn default() -> Self {
        Self::new(1, 1, 0)
    }
}
