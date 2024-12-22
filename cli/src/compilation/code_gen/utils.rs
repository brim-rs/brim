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

    pub fn inject_imports(&mut self) {
        let mut imports = String::new();

        self.needed_imports.sort();
        self.needed_imports.dedup();
        for import in &self.needed_imports {
            imports.push_str(&format!("#include <{}>\n", import));
        }

        imports.push_str("using namespace std;\n");

        self.buf.splice(0..0, imports.bytes());
    }
}