use std::path::PathBuf;
use std::time::Instant;
use anstream::ColorChoice;
use brim_config::toml::Config;
use brim_shell::Shell;
use brim_span::files::{SimpleFile, SimpleFiles};

#[derive(Debug)]
pub struct Session {
    files: SimpleFiles,
    config: Config,
    cwd: PathBuf,
    shell: Shell,
    color_choice: ColorChoice,
    pub start: Instant
}

impl Session {
    pub fn new(cwd: PathBuf, config: Config, color_choice: ColorChoice) -> Self {
        Self {
            files: SimpleFiles::new(),
            config,
            cwd,
            color_choice,
            shell: Shell::new(color_choice),
            start: Instant::now()
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
    
    pub fn shell(&mut self) -> &mut Shell {
        &mut self.shell
    }
}