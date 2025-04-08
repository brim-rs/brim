use crate::{
    files::Error,
    index::{ByteIndex, ColumnIndex, LineIndex, LineOffset, RawIndex},
    location::Location,
    span::Span,
};
use std::{
    ffi::{OsStr, OsString},
    num::NonZeroU32,
    path::PathBuf,
};

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FileId(NonZeroU32);

impl FileId {
    const OFFSET: u32 = 1;

    fn new(index: usize) -> FileId {
        FileId(NonZeroU32::new(index as u32 + Self::OFFSET).expect("file index cannot be stored"))
    }

    fn get(self) -> usize {
        (self.0.get() - Self::OFFSET) as usize
    }
}

#[derive(Clone, Debug)]
pub struct Files<Source> {
    files: Vec<File<Source>>,
}

impl<Source> Default for Files<Source>
where
    Source: AsRef<str>,
{
    fn default() -> Self {
        Self { files: vec![] }
    }
}

impl<Source> Files<Source>
where
    Source: AsRef<str>,
{
    pub fn new() -> Self {
        Files::<Source>::default()
    }

    pub fn add(&mut self, name: impl Into<OsString>, source: Source) -> FileId {
        let file_id = FileId::new(self.files.len());
        self.files.push(File::new(name.into(), source));
        file_id
    }

    pub fn update(&mut self, file_id: FileId, source: Source) {
        self.get_mut(file_id).update(source)
    }

    fn get(&self, file_id: FileId) -> &File<Source> {
        &self.files[file_id.get()]
    }

    fn get_mut(&mut self, file_id: FileId) -> &mut File<Source> {
        &mut self.files[file_id.get()]
    }

    pub fn name(&self, file_id: FileId) -> &OsStr {
        self.get(file_id).name()
    }

    pub fn line_span(
        &self,
        file_id: FileId,
        line_index: impl Into<LineIndex>,
    ) -> Result<Span, Error> {
        self.get(file_id).line_span(line_index.into())
    }

    pub fn line_index(&self, file_id: FileId, byte_index: impl Into<ByteIndex>) -> LineIndex {
        self.get(file_id).line_index(byte_index.into())
    }

    pub fn location(
        &self,
        file_id: FileId,
        byte_index: impl Into<ByteIndex>,
    ) -> Result<Location, Error> {
        self.get(file_id).location(byte_index.into())
    }

    pub fn source(&self, file_id: FileId) -> &Source {
        self.get(file_id).source()
    }

    pub fn source_span(&self, file_id: FileId) -> Span {
        self.get(file_id).source_span()
    }

    pub fn source_slice(&self, file_id: FileId, span: impl Into<Span>) -> Result<&str, Error> {
        self.get(file_id).source_slice(span.into())
    }
}

impl<'a, Source> crate::files::Files<'a> for Files<Source>
where
    Source: AsRef<str>,
{
    type FileId = FileId;
    type Source = &'a str;

    fn name(&self, id: FileId) -> Result<PathBuf, Error> {
        use std::path::PathBuf;

        Ok(PathBuf::from(self.name(id)))
    }

    fn source(&'a self, id: FileId) -> Result<&'a str, Error> {
        Ok(self.source(id).as_ref())
    }

    fn line_index(&self, id: FileId, byte_index: usize) -> Result<usize, Error> {
        Ok(self.line_index(id, byte_index as u32).to_usize())
    }

    fn line_range(
        &'a self,
        id: FileId,
        line_index: usize,
    ) -> Result<std::ops::Range<usize>, Error> {
        let span = self.line_span(id, line_index as u32)?;

        Ok(span.start().to_usize()..span.end().to_usize())
    }
}

#[derive(Debug, Clone)]
struct File<Source> {
    name: OsString,
    source: Source,
    line_starts: Vec<ByteIndex>,
}

impl<Source> File<Source>
where
    Source: AsRef<str>,
{
    fn new(name: OsString, source: Source) -> Self {
        let line_starts = line_starts(source.as_ref()).map(|i| ByteIndex::from(i as u32)).collect();

        File { name, source, line_starts }
    }

    fn update(&mut self, source: Source) {
        let line_starts = line_starts(source.as_ref()).map(|i| ByteIndex::from(i as u32)).collect();
        self.source = source;
        self.line_starts = line_starts;
    }

    fn name(&self) -> &OsStr {
        &self.name
    }

    fn line_start(&self, line_index: LineIndex) -> Result<ByteIndex, Error> {
        use std::cmp::Ordering;

        match line_index.cmp(&self.last_line_index()) {
            Ordering::Less => Ok(self.line_starts[line_index.to_usize()]),
            Ordering::Equal => Ok(self.source_span().end()),
            Ordering::Greater => Err(Error::LineTooLarge {
                given: line_index.to_usize(),
                max: self.last_line_index().to_usize(),
            }),
        }
    }

    fn last_line_index(&self) -> LineIndex {
        LineIndex::from(self.line_starts.len() as RawIndex)
    }

    fn line_span(&self, line_index: LineIndex) -> Result<Span, Error> {
        let line_start = self.line_start(line_index)?;
        let next_line_start = self.line_start(line_index + LineOffset::from(1))?;

        Ok(Span::new(line_start, next_line_start))
    }

    fn line_index(&self, byte_index: ByteIndex) -> LineIndex {
        match self.line_starts.binary_search(&byte_index) {
            Ok(line) => LineIndex::from(line as u32),
            Err(next_line) => LineIndex::from(next_line as u32 - 1),
        }
    }

    fn location(&self, byte_index: ByteIndex) -> Result<Location, Error> {
        let line_index = self.line_index(byte_index);
        let line_start_index = self.line_start(line_index).map_err(|_| Error::IndexTooLarge {
            given: byte_index.to_usize(),
            max: self.source().as_ref().len() - 1,
        })?;
        let line_src = self
            .source
            .as_ref()
            .get(line_start_index.to_usize()..byte_index.to_usize())
            .ok_or_else(|| {
                let given = byte_index.to_usize();
                let max = self.source().as_ref().len() - 1;
                if given > max {
                    Error::IndexTooLarge { given, max }
                } else {
                    Error::InvalidCharBoundary { given }
                }
            })?;

        Ok(Location {
            line: line_index,
            column: ColumnIndex::from(line_src.chars().count() as u32),
        })
    }

    fn source(&self) -> &Source {
        &self.source
    }

    fn source_span(&self) -> Span {
        Span::from_str(self.source.as_ref())
    }

    fn source_slice(&self, span: Span) -> Result<&str, Error> {
        let start = span.start().to_usize();
        let end = span.end().to_usize();

        self.source.as_ref().get(start..end).ok_or_else(|| {
            let max = self.source().as_ref().len() - 1;
            Error::IndexTooLarge { given: if start > max { start } else { end }, max }
        })
    }
}

fn line_starts(source: &str) -> impl '_ + Iterator<Item = usize> {
    std::iter::once(0).chain(source.match_indices('\n').map(|(i, _)| i + 1))
}
