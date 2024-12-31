use brim_span::files::SimpleFiles;

#[derive(Debug)]
pub struct Session {
    files: SimpleFiles<String, String> 
}

impl Default for Session {
    fn default() -> Self {
        Self {
            files: SimpleFiles::new()
        }
    }
}

impl Session {
    pub fn add_file(&mut self, name: String, source: String) -> usize {
        if let Ok(file) = self.files.get_index_by_name(&name) {
            self.files.update(file, name, source);
            
            return file;
        }
        
        self.files.add(name, source)
    }
}