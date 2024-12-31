use std::io;
use std::path::Path;

pub trait FileLoader {
    /// Check for the existence of a file.
    fn file_exists(&self, path: &Path) -> bool;

    fn read_file(&self, path: &Path) -> io::Result<String>;
}

pub struct BrimFileLoader;

impl BrimFileLoader {
    pub const MAX_FILE_SIZE: u32 = u32::MAX - 3;
}

impl FileLoader for BrimFileLoader {
    fn file_exists(&self, path: &Path) -> bool {
        path.exists()
    }

    fn read_file(&self, path: &Path) -> io::Result<String> {
        let metadata = path.metadata()?;

        if metadata.len() > Self::MAX_FILE_SIZE as u64 {
            return Err(io::Error::other(format!("files over {} bytes are not supported", Self::MAX_FILE_SIZE)));
        }
        
        std::fs::read_to_string(path)
    }
}