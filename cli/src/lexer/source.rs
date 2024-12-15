use std::path::PathBuf;
use ropey::Rope;

#[derive(Debug, Clone)]
pub struct Source {
    pub path: PathBuf,
    pub content: Rope,
}

impl Source {
    pub fn new(path: PathBuf, source: Rope) -> Self {
        Self { path, content: source }
    }

    pub fn from_reader(path: PathBuf, reader: impl std::io::Read) -> std::io::Result<Self> {
        Ok(Self {
            path,
            content: Rope::from_reader(reader)?,
        })
    }
    
    pub fn get_between(&self, start: usize, end: usize) -> String {
        self.content.slice(start..end).to_string()
    }
    
    pub fn with_path(&self, path: PathBuf) -> Self {
        Self {
            path,
            content: self.content.clone(),
        }
    }
}