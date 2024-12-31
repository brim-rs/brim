use crate::error::position::Position;

#[derive(Clone, PartialEq, Eq)]
pub struct TextSpan {
    pub start: Position,
    pub end: Position,
    pub literal: String,
}

impl TextSpan {
    pub fn new(start: Position, end: Position, literal: String) -> Self {
        Self {
            start,
            end,
            literal,
        }
    }

    pub fn combine(mut spans: Vec<TextSpan>) -> Option<TextSpan> {
        if spans.is_empty() {
            return None;
        }

        spans.sort_by(|a, b| a.start.index.cmp(&b.start.index));

        let start = spans.first().unwrap().start;
        let end = spans.last().unwrap().end;

        Some(TextSpan::new(
            start,
            end,
            spans.into_iter().map(|span| span.literal).collect(),
        ))
    }

    pub fn length(&self) -> usize {
        self.end.index - self.start.index
    }

    pub fn literal<'a>(&self, input: &'a str) -> &'a str {
        &input[self.start.index..self.end.index]
    }

    pub fn move_right(&self, count: usize) -> TextSpan {
        TextSpan::new(
            self.start.move_right(count),
            self.end.move_right(count),
            self.literal.clone(),
        )
    }

    pub fn shorten(&self, length: usize) -> TextSpan {
        TextSpan::new(
            self.start,
            self.end.move_left(self.start.column as usize, length),
            self.literal
                .chars()
                .take(self.literal.len() - length)
                .collect(),
        )
    }
}

impl Default for TextSpan {
    fn default() -> Self {
        Self {
            start: Position::default(),
            end: Position::default(),
            literal: String::new(),
        }
    }
}

impl std::fmt::Debug for TextSpan {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "\"{}\" ({}:{})",
            self.literal, self.start.line, self.start.column
        )
    }
}
