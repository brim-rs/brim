use crate::compilation::code_gen::CodeGen;

impl<'a> CodeGen<'a> {
    pub fn write(&mut self, s: impl Into<String>) {
        self.buf.extend_from_slice(s.into().as_bytes());
    }

    pub fn write_line(&mut self, line: impl Into<String>) {
        let indent = "    ".repeat(self.current_indent);
        self.buf.extend_from_slice(indent.as_bytes());

        self.write(line);
        self.buf.push(b'\n');
    }

    // This places a newline before the written string
    pub fn write_before(&mut self, s: impl Into<String> + Clone) {
        let indent = "    ".repeat(self.current_indent);
        self.buf.extend_from_slice(indent.as_bytes());

        self.write(s);
    }

    pub fn push_indent(&mut self) {
        self.current_indent += 1;
    }

    pub fn pop_indent(&mut self) {
        if self.current_indent > 0 {
            self.current_indent -= 1;
        }
    }
}