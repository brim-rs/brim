use crate::compilation::code_gen::CodeGen;

impl<'a> CodeGen<'a> {
    pub fn write_line(&mut self, line: impl Into<String>) {
        let indent = "    ".repeat(self.current_indent);
        self.buf.extend_from_slice(indent.as_bytes());

        self.buf.extend_from_slice(line.into().as_bytes());
        self.buf.push(b'\n');
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