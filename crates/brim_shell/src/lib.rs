//! Mostly derived from [cargo](https://github.com/rust-lang/cargo) source code.

mod link;
pub mod styles;

use crate::{
    link::Link,
    styles::{ERROR, HEADER, NOTE, WARN},
};
use anstream::{AutoStream, ColorChoice};
use anstyle::Style;
use anyhow::Result;
use gethostname::gethostname;
use std::{
    fmt,
    io::{Stderr, Stdout, Write},
    path::PathBuf,
};
use terminal_size::{terminal_size, Height, Width};

#[derive(Debug)]
pub struct ShellOutput {
    pub stdout: AutoStream<Stdout>,
    pub stderr: AutoStream<Stderr>,
    pub color: ColorChoice,
}

impl ShellOutput {
    pub fn stdout(&mut self) -> &mut AutoStream<Stdout> {
        &mut self.stdout
    }

    pub fn stderr(&mut self) -> &mut AutoStream<Stderr> {
        &mut self.stderr
    }
}

#[derive(Debug)]
pub struct Shell {
    pub output: ShellOutput,
}

impl Shell {
    pub fn new(color_choice: ColorChoice) -> Self {
        let output = ShellOutput {
            stdout: AutoStream::new(std::io::stdout(), color_choice),
            stderr: AutoStream::new(std::io::stderr(), color_choice),
            color: color_choice,
        };

        Self { output }
    }

    pub fn print(
        &mut self,
        status: &dyn fmt::Display,
        message: Option<&dyn fmt::Display>,
        style: &Style,
        justified: bool,
    ) -> Result<()> {
        let dim = anstyle::Style::new() | anstyle::Effects::DIMMED;

        let mut buffer = Vec::new();
        if justified {
            write!(&mut buffer, "{style}{status:>13}{style:#}")?;
        } else {
            write!(&mut buffer, "{style}{status}{style:#}{dim}:{dim:#}")?;
        }
        match message {
            Some(message) => writeln!(buffer, " {message}")?,
            None => write!(buffer, " ")?,
        }
        self.output.stderr().write_all(&buffer)?;
        Ok(())
    }

    pub fn write(
        &self,
        status: &dyn fmt::Display,
        message: Option<&dyn fmt::Display>,
        style: &Style,
        justified: bool,
    ) -> Result<String> {
        let dim = anstyle::Style::new() | anstyle::Effects::DIMMED;

        let mut buffer = Vec::new();
        if justified {
            write!(&mut buffer, "{style}{status:>13}{style:#}")?;
        } else {
            write!(&mut buffer, "{style}{status}{style:#}{dim}:{dim:#}")?;
        }
        match message {
            Some(message) => writeln!(buffer, " {message}")?,
            None => write!(buffer, " ")?,
        }
        Ok(String::from_utf8(buffer)?)
    }

    pub fn warn<T: fmt::Display>(&mut self, message: T) -> Result<()> {
        self.print(&"warning", Some(&message), &WARN, false)
    }

    pub fn note<T: fmt::Display>(&mut self, message: T) -> Result<()> {
        self.print(&"note", Some(&message), &NOTE, false)
    }

    pub fn error<T: fmt::Display>(&mut self, message: T) -> Result<()> {
        self.print(&"error", Some(&message), &ERROR, false)
    }

    pub fn status<T, U>(&mut self, status: T, message: U) -> Result<()>
    where
        T: fmt::Display,
        U: fmt::Display,
    {
        self.print(&status, Some(&message), &HEADER, true)
    }

    pub fn set_color_choice(&mut self, color_choice: ColorChoice) {
        let (stdout, stderr, color) = (
            &mut self.output.stdout,
            &mut self.output.stderr,
            &mut self.output.color,
        );

        *color = color_choice;
        *stdout = AutoStream::new(std::io::stdout(), color_choice);
        *stderr = AutoStream::new(std::io::stderr(), color_choice);
    }

    pub fn file_link(&mut self, file: PathBuf) -> Result<url::Url> {
        let mut url = url::Url::from_file_path(file).ok().unwrap();

        let hostname = if cfg!(windows) {
            None
        } else {
            gethostname().into_string().ok()
        };
        let _ = url.set_host(hostname.as_deref());
        Ok(url)
    }

    pub fn hyperlink<'a>(&mut self, url: &'a str, text: &'a str) -> Result<Link<'a>> {
        Ok(Link::new(text, url))
    }

    pub fn cond_print<T: fmt::Display>(&mut self, message: T, err: bool) -> Result<()> {
        if err {
            self.output.stderr().write_all(format!("{}", message).as_bytes())?;
        } else {
            self.output.stdout().write_all(format!("{}", message).as_bytes())?;
        }
        Ok(())
    }

    pub fn centered<T: fmt::Display>(
        &mut self,
        title: &str,
        message: T,
        err: bool,
    ) -> Result<()> {
        let title = format!(" {} ", title);
        let stderr = self.output.stderr();
        let size = terminal_size();
        if let Some((Width(w), Height(h))) = size {
            let width = w as usize;

            let message = format!(" {} ", message);
            let message_len = message.len();
            let message = if message_len > width {
                message
            } else {
                let padding = (width - message_len) / 2;
                format!("{:padding$}{}", "", message, padding = padding)
            };

            let dim_title = anstyle::Style::new() | anstyle::Effects::DIMMED;
            let reset = anstyle::Style::new() | anstyle::Effects::new();
            let mut buffer = Vec::new();

            writeln!(buffer, "{dim_title}{:=^1$}{reset:#}", title, width)?;
            writeln!(buffer, "{:#^1$}", message, width)?;

            if err {
                stderr.write_all(&buffer)?;
            } else {
                self.output.stdout().write_all(&buffer)?;
            }

            Ok(())
        } else {
            self.cond_print(title, err)?;
            self.cond_print(message, err)?;

            Ok(())
        }
    }
}
