use crate::index::{ByteIndex, ByteOffset, RawIndex, RawOffset};
use std::{fmt, ops::Range, str::FromStr};

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Span {
    start: ByteIndex,
    end: ByteIndex,
}

impl Span {
    pub const DUMMY: Span = Span { start: ByteIndex(0), end: ByteIndex(0) };

    pub fn new(start: impl Into<ByteIndex>, end: impl Into<ByteIndex>) -> Span {
        let start = start.into();
        let end = end.into();

        assert!(end >= start, "end ({}) must be greater than or equal to start ({})", end, start);

        Span { start, end }
    }

    pub const fn initial() -> Span {
        Span { start: ByteIndex(0), end: ByteIndex(0) }
    }

    pub fn merge(self, other: Span) -> Span {
        use std::cmp::{max, min};

        let start = min(self.start, other.start);
        let end = max(self.end, other.end);
        Span::new(start, end)
    }

    pub fn disjoint(self, other: Span) -> bool {
        let (first, last) = if self.end < other.end { (self, other) } else { (other, self) };
        first.end <= last.start
    }

    pub fn start(self) -> ByteIndex {
        self.start
    }

    pub fn end(self) -> ByteIndex {
        self.end
    }

    pub fn range(self) -> Range<usize> {
        self.into()
    }

    /// Returns new empty span with the same start as the current span.
    pub fn from_start(self) -> Span {
        Span::new(self.start, self.start)
    }

    /// Returns new empty span with the same end as the current span.
    pub fn from_end(self) -> Span {
        Span::new(self.end, self.end)
    }

    pub fn to(self, other: Span) -> Span {
        Span::new(self.start, other.end)
    }

    pub fn from_point(point: impl Into<ByteIndex>) -> Span {
        let point = point.into();
        Span::new(point, point)
    }

    pub fn length(self) -> usize {
        (self.end - self.start).into()
    }

    pub fn move_by(self, offset: usize) -> Span {
        let start = self.start + ByteOffset(offset as RawOffset);
        let end = self.end + ByteOffset(offset as RawOffset);
        Span::new(start, end)
    }
}

impl Default for Span {
    fn default() -> Span {
        Span::initial()
    }
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[{start}, {end})", start = self.start(), end = self.end(),)
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

impl FromStr for Span {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Span::new(0, s.len() as u32))
    }
}
