use std::path::PathBuf;
use brim_config::toml::Config;
use brim_span::files::{SimpleFile, SimpleFiles};

#[derive(Debug)]
pub struct Session {
    files: SimpleFiles,
    config: Config,
    cwd: PathBuf,
}

impl Session {
    pub fn new(cwd: PathBuf, config: Config) -> Self {
        Self {
            files: SimpleFiles::new(),
            config,
            cwd,
        }
    }

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