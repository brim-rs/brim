use anyhow::Result;
use colored::Colorize;
use std::path::PathBuf;

pub fn setup_panic_handler(no_backtrace: bool) {
    std::panic::set_hook(Box::new(move |info| {
        let message = match (
            info.payload().downcast_ref::<&str>(),
            info.payload().downcast_ref::<String>(),
        ) {
            (Some(s), _) => (*s).to_string(),
            (_, Some(s)) => s.to_string(),
            (None, None) => "unknown error".into(),
        };
        let location = match info.location() {
            None => "".into(),
            Some(location) => format!("{}:{}", location.file(), location.line()),
        }
        .replace("\\", "/");

        let text = format!(
            "{}
Please report it at https://github.com/brim-rs/brim \n
Version {}
Os: {} {}
Location: {}

{}
",
            "\nOh no! Something went wrong!\nThis is a bug in Brim, not in your code. \n"
                .bright_red(),
            env!("CARGO_PKG_VERSION"),
            std::env::consts::OS,
            std::env::consts::ARCH,
            location,
            message.bright_red(),
        );

        if no_backtrace {
            eprintln!("{}", text.bold());
            return;
        }

        let mut backtrace = String::new();
        backtrace += &"Backtrace:\n".bright_red().to_string();
        backtrace::trace(|frame| {
            backtrace::resolve_frame(frame, |symbol| {
                let mut new_text = String::new();

                if let Some(name) = symbol.name() {
                    new_text =
                        new_text + "\x1b[96mat\x1b[39m " + &name.to_string().dimmed().to_string();
                } else {
                    new_text = "at <unknown>".dimmed().to_string();
                }

                if let Some(filename) = symbol.filename() {
                    new_text = format!(
                        "{}: ({})",
                        new_text,
                        shorten_path(filename.to_str().unwrap()).unwrap()
                    )
                    .cyan()
                    .to_string();
                }

                backtrace = format!("{}  {}\n", backtrace, new_text);
            });

            true
        });

        eprintln!("{}{}", backtrace, text.bold());
    }))
}

pub fn shorten_path(path: &str) -> Result<String> {
    let path = PathBuf::from(path);

    let shortened = path
        .iter()
        .skip(
            if path.starts_with("/rustc/") || path.starts_with("\\rustc\\") {
                3
            } else {
                0
            },
        )
        .collect::<PathBuf>();
    let shortened = shortened.to_str().unwrap();

    Ok(format!("\\rustc\\{}", shortened))
}
