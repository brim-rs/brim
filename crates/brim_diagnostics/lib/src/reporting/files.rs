use std::ops::Range;

#[derive(Debug)]
#[non_exhaustive]
pub enum Error {
    FileMissing,
    IndexTooLarge { given: usize, max: usize },
    LineTooLarge { given: usize, max: usize },
    ColumnTooLarge { given: usize, max: usize },
    InvalidCharBoundary { given: usize },
    Io(std::io::Error),
}

impl From<std::io::Error> for Error {
    fn from(err: std::io::Error) -> Error {
        Error::Io(err)
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::FileMissing => write!(f, "file missing"),
            Error::IndexTooLarge { given, max } => {
                write!(f, "invalid index {}, maximum index is {}", given, max)
            }
            Error::LineTooLarge { given, max } => {
                write!(f, "invalid line {}, maximum line is {}", given, max)
            }
            Error::ColumnTooLarge { given, max } => {
                write!(f, "invalid column {}, maximum column {}", given, max)
            }
            Error::InvalidCharBoundary { .. } => write!(f, "index is not a code point boundary"),
            Error::Io(err) => write!(f, "{}", err),
        }
    }
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match &self {
            Error::Io(err) => Some(err),
            _ => None,
        }
    }
}

pub trait Files<'a> {
    type FileId: 'a + Copy + PartialEq;
    type Name: 'a + std::fmt::Display;
    type Source: 'a + AsRef<str>;

    fn name(&'a self, id: Self::FileId) -> Result<Self::Name, Error>;

    fn source(&'a self, id: Self::FileId) -> Result<Self::Source, Error>;

    fn line_index(&'a self, id: Self::FileId, byte_index: usize) -> Result<usize, Error>;

    #[allow(unused_variables)]
    fn line_number(&'a self, id: Self::FileId, line_index: usize) -> Result<usize, Error> {
        Ok(line_index + 1)
    }

    fn column_number(
        &'a self,
        id: Self::FileId,
        line_index: usize,
        byte_index: usize,
    ) -> Result<usize, Error> {
        let source = self.source(id)?;
        let line_range = self.line_range(id, line_index)?;
        let column_index = column_index(source.as_ref(), line_range, byte_index);

        Ok(column_index + 1)
    }

    fn location(&'a self, id: Self::FileId, byte_index: usize) -> Result<Location, Error> {
        let line_index = self.line_index(id, byte_index)?;

        Ok(Location {
            line_number: self.line_number(id, line_index)?,
            column_number: self.column_number(id, line_index, byte_index)?,
        })
    }

    fn line_range(&'a self, id: Self::FileId, line_index: usize) -> Result<Range<usize>, Error>;
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Location {
    pub line_number: usize,
    pub column_number: usize,
}

pub fn column_index(source: &str, line_range: Range<usize>, byte_index: usize) -> usize {
    let end_index = std::cmp::min(byte_index, std::cmp::min(line_range.end, source.len()));

    (line_range.start..end_index)
        .filter(|byte_index| source.is_char_boundary(byte_index + 1))
        .count()
}

pub fn line_starts(source: &str) -> impl '_ + Iterator<Item = usize> {
    std::iter::once(0).chain(source.match_indices('\n').map(|(i, _)| i + 1))
}

#[derive(Debug, Clone)]
pub struct SimpleFile<Name, Source> {
    name: Name,
    source: Source,
    line_starts: Vec<usize>,
}

impl<Name, Source> SimpleFile<Name, Source>
where
    Name: std::fmt::Display,
    Source: AsRef<str>,
{
    pub fn new(name: Name, source: Source) -> SimpleFile<Name, Source> {
        SimpleFile {
            name,
            line_starts: line_starts(source.as_ref()).collect(),
            source,
        }
    }

    pub fn name(&self) -> &Name {
        &self.name
    }

    pub fn source(&self) -> &Source {
        &self.source
    }

    fn line_start(&self, line_index: usize) -> Result<usize, Error> {
        use std::cmp::Ordering;

        match line_index.cmp(&self.line_starts.len()) {
            Ordering::Less => Ok(self
                .line_starts
                .get(line_index)
                .cloned()
                .expect("failed despite previous check")),
            Ordering::Equal => Ok(self.source.as_ref().len()),
            Ordering::Greater => Err(Error::LineTooLarge {
                given: line_index,
                max: self.line_starts.len() - 1,
            }),
        }
    }
}

impl<'a, Name, Source> Files<'a> for SimpleFile<Name, Source>
where
    Name: 'a + std::fmt::Display + Clone,
    Source: 'a + AsRef<str>,
{
    type FileId = ();
    type Name = Name;
    type Source = &'a str;

    fn name(&self, (): ()) -> Result<Name, Error> {
        Ok(self.name.clone())
    }

    fn source(&self, (): ()) -> Result<&str, Error> {
        Ok(self.source.as_ref())
    }

    fn line_index(&self, (): (), byte_index: usize) -> Result<usize, Error> {
        Ok(self
            .line_starts
            .binary_search(&byte_index)
            .unwrap_or_else(|next_line| next_line - 1))
    }

    fn line_range(&self, (): (), line_index: usize) -> Result<Range<usize>, Error> {
        let line_start = self.line_start(line_index)?;
        let next_line_start = self.line_start(line_index + 1)?;

        Ok(line_start..next_line_start)
    }
}

#[derive(Debug, Default, Clone)]
pub struct SimpleFiles<Name, Source> {
    files: Vec<SimpleFile<Name, Source>>,
}

impl<Name, Source> SimpleFiles<Name, Source>
where
    Name: std::fmt::Display,
    Source: AsRef<str>,
{
    pub fn new() -> SimpleFiles<Name, Source> {
        SimpleFiles { files: Vec::new() }
    }

    pub fn add(&mut self, name: Name, source: Source) -> usize {
        let file_id = self.files.len();
        self.files.push(SimpleFile::new(name, source));
        file_id
    }

    pub fn get(&self, file_id: usize) -> Result<&SimpleFile<Name, Source>, Error> {
        self.files.get(file_id).ok_or(Error::FileMissing)
    }
    
    pub fn update(&mut self, file_id: usize, name: Name, source: Source) {
        self.files[file_id] = SimpleFile::new(name, source);
    }
}

impl<'a, Name, Source> Files<'a> for SimpleFiles<Name, Source>
where
    Name: 'a + std::fmt::Display + Clone,
    Source: 'a + AsRef<str>,
{
    type FileId = usize;
    type Name = Name;
    type Source = &'a str;

    fn name(&self, file_id: usize) -> Result<Name, Error> {
        Ok(self.get(file_id)?.name().clone())
    }

    fn source(&self, file_id: usize) -> Result<&str, Error> {
        Ok(self.get(file_id)?.source().as_ref())
    }

    fn line_index(&self, file_id: usize, byte_index: usize) -> Result<usize, Error> {
        self.get(file_id)?.line_index((), byte_index)
    }

    fn line_range(&self, file_id: usize, line_index: usize) -> Result<Range<usize>, Error> {
        self.get(file_id)?.line_range((), line_index)
    }
}