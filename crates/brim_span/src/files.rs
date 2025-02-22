use once_cell::sync::Lazy;
use std::{ops::Range, path::PathBuf, slice::Iter, sync::Mutex};
use thiserror::Error;

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
    type Source: 'a + AsRef<str>;

    fn name(&'a self, id: Self::FileId) -> Result<PathBuf, Error>;

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
pub struct SimpleFile {
    name: PathBuf,
    source: String,
    line_starts: Vec<usize>,
    id: usize,
}

impl SimpleFile {
    pub fn new(name: PathBuf, source: String) -> SimpleFile {
        SimpleFile {
            name,
            line_starts: line_starts(source.as_ref()).collect(),
            source,
            id: 0,
        }
    }

    pub fn name(&self) -> &PathBuf {
        &self.name
    }

    pub fn source(&self) -> &String {
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
            Ordering::Equal => Ok(self.source.len()),
            Ordering::Greater => Err(Error::LineTooLarge {
                given: line_index,
                max: self.line_starts.len() - 1,
            }),
        }
    }

    pub fn id(&self) -> usize {
        self.id
    }
}

impl<'a> Files<'a> for SimpleFile {
    type FileId = ();
    type Source = &'a str;

    fn name(&self, (): ()) -> Result<PathBuf, Error> {
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

#[derive(Error, Debug, Clone, PartialEq)]
pub enum FileError {
    #[error("File not found")]
    FileMissing,
    #[error("Lock acquisition failed: {0}")]
    LockError(String),
}

#[derive(Debug, Default, Clone)]
pub struct SimpleFiles {
    files: Vec<SimpleFile>,
    next_id: usize, // Track next available ID separately
}

impl SimpleFiles {
    /// Creates a new empty SimpleFiles collection
    pub fn new() -> Self {
        Self {
            files: Vec::new(),
            next_id: 0,
        }
    }

    pub fn from_files(files: Vec<SimpleFile>) -> Self {
        let next_id = files.iter().map(|f| f.id).max().unwrap_or(0) + 1;
        Self { files, next_id }
    }

    /// Adds a new file to the collection
    ///
    /// Returns the ID of the newly added file
    pub fn add(&mut self, name: PathBuf, source: String) -> usize {
        let file_id = self.next_id;
        self.next_id += 1;

        let file = SimpleFile {
            id: file_id,
            name,
            source: source.clone(),
            line_starts: compute_line_starts(&source),
        };

        self.files.push(file);
        file_id
    }

    /// Retrieves a file by its ID
    pub fn get(&self, file_id: usize) -> Result<&SimpleFile, Error> {
        self.files
            .iter()
            .find(|file| file.id == file_id)
            .ok_or(Error::FileMissing)
    }

    /// Retrieves a file by its name
    pub fn get_by_name(&self, name: &PathBuf) -> Result<&SimpleFile, Error> {
        self.files
            .iter()
            .find(|file| file.name == *name)
            .ok_or(Error::FileMissing)
    }

    /// Gets the index of a file by its name
    pub fn get_index_by_name(&self, name: &PathBuf) -> Result<usize, Error> {
        self.files
            .iter()
            .position(|file| file.name == *name)
            .ok_or(Error::FileMissing)
    }

    /// Updates an existing file
    pub fn update(&mut self, file_id: usize, name: PathBuf, source: String) {
        if let Some(file) = self.files.iter_mut().find(|f| f.id == file_id) {
            file.name = name;
            file.source = source;
            file.line_starts = compute_line_starts(&file.source);
        }
    }

    /// Returns the number of files in the collection
    pub fn len(&self) -> usize {
        self.files.len()
    }

    /// Returns true if the collection is empty
    pub fn is_empty(&self) -> bool {
        self.files.is_empty()
    }

    /// Removes a file by ID
    pub fn remove(&mut self, file_id: usize) -> Result<SimpleFile, Error> {
        let position = self
            .files
            .iter()
            .position(|file| file.id == file_id)
            .ok_or(Error::FileMissing)?;
        Ok(self.files.remove(position))
    }
}

impl Iterator for SimpleFiles {
    type Item = SimpleFile;

    fn next(&mut self) -> Option<Self::Item> {
        self.files.pop()
    }
}

impl<'a> IntoIterator for &'a SimpleFiles {
    type IntoIter = Iter<'a, SimpleFile>;
    type Item = &'a SimpleFile;

    fn into_iter(self) -> Self::IntoIter {
        self.files.iter()
    }
}

impl<'a> Files<'a> for SimpleFiles {
    type FileId = usize;
    type Source = &'a str;

    fn name(&self, file_id: usize) -> Result<PathBuf, Error> {
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

/// Helper function to compute line start positions
fn compute_line_starts(source: &str) -> Vec<usize> {
    let mut starts = vec![0];
    starts.extend(source.match_indices('\n').map(|(i, _)| i + 1));
    starts
}

/// Global instance of SimpleFiles protected by a mutex
pub static GLOBAL_FILES: Lazy<Mutex<SimpleFiles>> = Lazy::new(|| Mutex::new(SimpleFiles::new()));

// Global helper functions
pub fn add_file(name: PathBuf, source: String) -> usize {
    GLOBAL_FILES
        .lock()
        .expect("Failed to lock global files")
        .add(name, source)
}

pub fn get_file(file_id: usize) -> Result<SimpleFile, Error> {
    GLOBAL_FILES
        .lock()
        .expect("Failed to lock global files")
        .get(file_id)
        .cloned()
}

pub fn get_file_by_name(name: &PathBuf) -> Result<SimpleFile, Error> {
    GLOBAL_FILES
        .lock()
        .expect("Failed to lock global files")
        .get_by_name(name)
        .cloned()
}

pub fn get_index_by_name(name: &PathBuf) -> Result<usize, Error> {
    GLOBAL_FILES
        .lock()
        .expect("Failed to lock global files")
        .get_index_by_name(name)
}

pub fn update_file(file_id: usize, name: PathBuf, source: String) {
    GLOBAL_FILES
        .lock()
        .expect("Failed to lock global files")
        .update(file_id, name, source)
}

pub fn remove_file(file_id: usize) -> Result<SimpleFile, Error> {
    GLOBAL_FILES
        .lock()
        .expect("Failed to lock global files")
        .remove(file_id)
}

pub fn files() -> Vec<SimpleFile> {
    GLOBAL_FILES
        .lock()
        .expect("Failed to lock global files")
        .files
        .clone()
}

pub fn get_path(file_id: usize) -> Result<PathBuf, Error> {
    GLOBAL_FILES
        .lock()
        .expect("Failed to lock global files")
        .name(file_id)
}
