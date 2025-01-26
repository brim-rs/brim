pub mod codegen;
mod expr;
mod generics;
mod item;
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

    /// Increases the current indentation level
    pub fn increase_indent(&mut self) {
        self.indent += self.indent_size;
    }

    /// Decreases the current indentation level
    pub fn decrease_indent(&mut self) {
        if self.indent >= self.indent_size {
            self.indent -= self.indent_size;
        }
    }

    /// Appends a line with the current indentation
    pub fn add_line(&mut self, line: &str) {
        self.code.push_str(&" ".repeat(self.indent));
        self.code.push_str(line);
        self.code.push('\n');
    }

    /// Adds a block with braces and adjusts indentation
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

    /// Converts the generated code to a string
    pub fn build(self) -> String {
        self.code
    }
}
