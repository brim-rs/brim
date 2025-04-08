#![feature(let_chains)]

pub mod codegen;
mod expr;
mod generics;
mod item;
mod stmt;
mod ty;

#[derive(Debug)]
pub struct CodeBuilder {
    code: String,
    indent: usize,
    indent_size: usize,
}

impl CodeBuilder {
    /// Creates a new `CodeBuilder` with the given indentation size
    pub fn new(indent_size: usize) -> Self {
        Self {
            code: String::new(),
            indent: 0,
            indent_size,
        }
    }

    pub fn increase_indent(&mut self) {
        self.indent += self.indent_size;
    }

    pub fn decrease_indent(&mut self) {
        if self.indent >= self.indent_size {
            self.indent -= self.indent_size;
        }
    }

    pub fn add_line(&mut self, line: &str) {
        self.code.push_str(&" ".repeat(self.indent));
        self.add_line_no_indent(line);
    }

    pub fn add_line_no_indent(&mut self, line: &str) {
        self.code.push_str(line);
        self.code.push('\n');
    }

    pub fn add_block<F>(&mut self, header: &str, body: F)
    where
        F: FnOnce(&mut CodeBuilder),
    {
        self.add_line(header);
        self.add_line("{");
        self.increase_indent();
        body(self);
        self.decrease_indent();
        self.add_line("}");
    }

    pub fn build(&self) -> &String {
        &self.code
    }

    pub fn reset(&mut self) {
        self.code.clear();
    }
}
