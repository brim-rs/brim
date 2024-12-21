use crate::ast::item::TopLevelItem;
use crate::compilation::code_gen::CodeGen;
use anyhow::Result;
use crate::ast::statements::{Function, Stmt, StmtKind};

impl<'a> CodeGen<'a> {
    pub fn generate_fn(&mut self, function: Function) -> Result<()> {
        // Start function definition with automatic indentation
        self.write_line(&format!("void {}() {{", function.name.literal()));
        self.push_indent(); // Increase indent for function body

        // Add statements inside the function body
        self.write_line("printf(\"Hello, World!\");");
        self.write_line("for (int i = 0; i < 10; i++) {");
        self.push_indent(); // Indent for the loop body
        self.write_line("printf(\"%d\\n\", i);");
        self.pop_indent(); // Outdent after the loop body
        self.write_line("}");

        self.pop_indent(); // Outdent after the function body
        self.write_line("}");

        // Print the generated C++ code for verification
        println!("{}", String::from_utf8_lossy(&self.buf));

        Ok(())
    }
}