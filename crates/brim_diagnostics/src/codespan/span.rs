#[cfg(feature = "serialization")]
use serde::{Deserialize, Serialize};
use std::fmt;
use std::ops::Range;

use crate::codespan::index::{ByteIndex, RawIndex};

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(feature = "serialization", derive(Deserialize, Serialize))]
pub struct Span {
    start: ByteIndex,
    end: ByteIndex,
}

impl Span {
    pub fn new(start: impl Into<ByteIndex>, end: impl Into<ByteIndex>) -> Span {
        let start = start.into();
        let end = end.into();

        assert!(end >= start);

        Span { start, end }
    }


    pub const fn initial() -> Span {
        Span {
            start: ByteIndex(0),
            end: ByteIndex(0),
        }
    }


    pub fn from_str(s: &str) -> Span {
        Span::new(0, s.len() as u32)
    }


    pub fn merge(self, other: Span) -> Span {
        use std::cmp::{max, min};

        let start = min(self.start, other.start);
        let end = max(self.end, other.end);
        Span::new(start, end)
    }


    pub fn disjoint(self, other: Span) -> bool {
        let (first, last) = if self.end < other.end {
            (self, other)
        } else {
            (other, self)
        };
        first.end <= last.start
    }


    pub fn start(self) -> ByteIndex {
        self.start
    }


    pub fn end(self) -> ByteIndex {
        self.end
    }
}

impl Default for Span {
    fn default() -> Span {
        Span::initial()
    }
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "[{start}, {end})",
            start = self.start(),
            end = self.end(),
        )
    }
}

impl<I> From<Range<I>> for Span
where
    I: Into<ByteIndex>,
{
    fn from(range: Range<I>) -> Span {
        Span::new(range.start, range.end)
    }
}

impl From<Span> for Range<usize> {
    fn from(span: Span) -> Range<usize> {
        span.start.into()..span.end.into()
    }
}

impl From<Span> for Range<RawIndex> {
    fn from(span: Span) -> Range<RawIndex> {
        span.start.0..span.end.0
    }
}
