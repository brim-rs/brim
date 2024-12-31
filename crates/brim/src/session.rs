use std::path::PathBuf;
use brim_span::files::{SimpleFile, SimpleFiles};

#[derive(Debug)]
pub struct Session {
    files: SimpleFiles
}

impl Default for Session {
    fn default() -> Self {
        Self {
            files: SimpleFiles::new()
        }
    }
}

impl Session {
    pub fn add_file(&mut self, name: PathBuf, source: String) -> usize {
        if let Ok(file) = self.files.get_index_by_name(&name) {
            self.files.update(file, name, source);
            
            return file;
        }
        
        self.files.add(name, source)
    }
    
    pub fn get_file(&self, file: usize) -> Option<&SimpleFile> {
        self.files.get(file).ok()
    }
    
    pub fn get_file_by_name(&self, name: &PathBuf) -> Option<&SimpleFile> {
        self.files.get_by_name(name).ok()
    }
}