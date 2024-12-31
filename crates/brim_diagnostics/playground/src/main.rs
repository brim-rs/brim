use termcolor::ColorChoice;
use brim_diagnostics::diagnostic::{Diagnostic, Label, LabelStyle};
use anyhow::Result;
use colored::Colorize;
use term::color::Color;
use brim_span::files::SimpleFiles;

fn main() -> Result<()> {
    let mut files = SimpleFiles::new();

    let file_id = files.add(
        "FizzBuzz.fun".into(),
        unindent::unindent(
            r#"
            pub fn div(a: i32, b: i32) -> i32!string {
                if b == 0 {
                    return @err("Division by zero");
                }

                return @ok(a / b);
            };

            pub fn do_action(a: i32, b: i32, c: i32) -> i32!string {
                return @try(div(a, b))
            };

            pub fn main() {
                let h = do_action(10, 0, 3);
            };
        "#,
        ),
    );

    let mut diagnostic = Diagnostic::error()
        .with_message("`case` clauses have incompatible types")
        .with_code("E0308")
        .with_labels(vec![
            Label::error(file_id, 30..40).with_message("function must return the value of the same type"),
            Label::add(file_id, ".clone()", 217..222).with_message("add safe cast to i32"),
        ])
        .with_notes(vec![unindent::unindent(
            "
            expected type `String`
                found type `Nat`
        ",
        )]);

    for label in &mut diagnostic.labels {
        match label.style {
            LabelStyle::Add(to_add) => {
                let range = label.range.start..label.range.start;

                let file = files.get(label.file_id)?;
                let source = file.source().as_str();
                // Insert code to add between the range
                let source = source[..range.start].to_string() + &to_add.bright_green().to_string() + &source[range.end..];

                files.update(label.file_id, file.name().clone(), source);

                // Update the range to include the added code
                label.range = label.range.start..label.range.start + to_add.len() + 1;
            }
            _ => {}
        }
    }

    let writer = termcolor::StandardStream::stderr(ColorChoice::Always);
    let mut config = brim_diagnostics::term::DiagConfig::default();

    brim_diagnostics::term::emit(&mut writer.lock(), &config, &files, &diagnostic)?;

    Ok(())
}
