use anyhow::Result;
use colored::Colorize;
use std::path::PathBuf;
use sysinfo::System;
use terminal_size::{Width, terminal_size};

const SYSTEM_PATH_PATTERNS: &[&str] = &[
    "/std/",
    "\\std\\",
    "/core/",
    "\\core\\",
    "/alloc/",
    "\\alloc\\",
    "\\vcstartup\\",
    "/vcstartup/",
    "backtrace-",
    "\\backtrace\\",
    "/backtrace/",
];

const SYSTEM_FUNCTION_PATTERNS: &[&str] = &[
    "::std::",
    "::core::",
    "::alloc::",
    "std::",
    "core::",
    "alloc::",
    "backtrace::",
    "::backtrace::",
    "::_",
    "BaseThreadInitThunk",
    "RtlUserThreadStart",
    "invoke_main",
    "__scrt_common_main",
    "call_once",
];

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
        let location = info
            .location()
            .map_or_else(String::new, |loc| format!("{}:{}", loc.file(), loc.line()))
            .replace("\\", "/");

        let terminal_width = terminal_size().map_or(80, |(Width(w), _)| w as usize);
        let divider = "━".repeat(terminal_width).bright_red();

        let title = " 💥 Oh no! Something went wrong! 💥 ";
        let centered_title = format!("{:^width$}", title, width = terminal_width)
            .bright_red()
            .bold();

        let report_info = format!(
            "{} {}",
            "🔗 Please report it at",
            "https://github.com/brim-rs/brim".blue().underline()
        );

        let mut sys = System::new_all();
        sys.refresh_all();

        let system_info = format_system_info(location, sys);

        let text = format!(
            "{}
{}
{}

{}

{}
",
            divider,
            centered_title,
            report_info,
            system_info,
            message.bright_red().bold(),
        );

        if no_backtrace {
            eprintln!("{}", text);
            return;
        }

        let mut backtrace = String::new();
        let mut consecutive_system_lines = 0;
        let mut last_shown_line = String::new();
        let mut frames = Vec::new();

        backtrace::trace(|frame| {
            frames.push(frame.clone());
            true
        });

        for frame in frames {
            backtrace::resolve_frame(&frame, |symbol| {
                let mut is_system_code = false;
                let name_str = symbol.name().map_or_else(
                    || "at <unknown>".dimmed().to_string(),
                    |name| {
                        let name_str = name.to_string();

                        is_system_code = SYSTEM_FUNCTION_PATTERNS
                            .iter()
                            .any(|pattern| name_str.contains(pattern));

                        if !is_system_code && name_str == "main" {
                            is_system_code = true;
                        }

                        if name_str.starts_with("core::ops::function") {
                            is_system_code = false;
                        }

                        format!("\x1b[96mat\x1b[39m {}", name_str.dimmed())
                    },
                );

                let mut frame_text = name_str;

                if let Some(filename) = symbol.filename() {
                    let file_path = filename.to_str().unwrap_or("");

                    if !is_system_code {
                        is_system_code = SYSTEM_PATH_PATTERNS
                            .iter()
                            .any(|pattern| file_path.contains(pattern));
                    }

                    if file_path.contains("core/src/ops/function.rs") {
                        is_system_code = false;
                    }

                    let line_number = symbol.lineno().unwrap_or(0);
                    let column_number = symbol.colno().unwrap_or(0);

                    frame_text = format!(
                        "{}: ({}:{}:{})",
                        frame_text,
                        shorten_path(file_path).unwrap_or_else(|_| file_path.to_string()),
                        line_number,
                        column_number
                    )
                    .cyan()
                    .to_string();
                }

                if is_system_code {
                    consecutive_system_lines += 1;
                    last_shown_line = frame_text;
                } else {
                    if consecutive_system_lines > 0 {
                        if consecutive_system_lines == 1 {
                            backtrace = format!("{}  {}\n", backtrace, last_shown_line);
                        } else {
                            let collapse_message = format!(
                                "... collapsed {} lines from system code ...",
                                consecutive_system_lines
                            );
                            backtrace = format!(
                                "{}  {}\n",
                                backtrace,
                                collapse_message.bright_magenta().italic()
                            );
                        }
                        consecutive_system_lines = 0;
                    }

                    backtrace = format!("{}  {} {}\n", backtrace, "→".bright_green(), frame_text);
                }
            });
        }

        if consecutive_system_lines > 0 {
            let collapse_message = format!(
                "... collapsed {} lines from system code ...",
                consecutive_system_lines
            );
            backtrace = format!(
                "{}  {}\n",
                backtrace,
                collapse_message.bright_magenta().italic()
            );
        }

        let footer = "━".repeat(terminal_width).bright_red();
        backtrace = format!("{}{}\n", backtrace, footer);

        eprintln!("{}{}", text, backtrace);
    }))
}

fn format_system_info(location: String, sys: System) -> String {
    let mut info = Vec::new();

    let mut add_info = |key: &str, value: String| {
        info.push(format!("{}: {}", key, value).dimmed().to_string());
    };

    add_info("VERSION", env!("CARGO_PKG_VERSION").to_string());

    add_info(
        "SYSTEM",
        format!(
            "{} {} {}",
            System::name().unwrap_or("unknown".to_string()),
            System::cpu_arch(),
            System::os_version().unwrap_or("unknown".to_string())
        ),
    );

    if let Ok(cwd) = std::env::current_dir() {
        add_info("WORKING DIR", cwd.display().to_string());
    }

    if let Ok(parallelism) = std::thread::available_parallelism() {
        add_info("THREADS", parallelism.to_string());
    }

    let memory_info = get_memory_info(sys);
    if !memory_info.is_empty() {
        add_info("MEMORY", memory_info.dimmed().to_string());
    }

    add_info("LOCATION", location.underline().to_string());

    info.join("\n")
}

fn get_memory_info(sys: System) -> String {
    let available = sys.available_memory();
    let total = sys.total_memory();

    if available > 0 && total > 0 {
        format!(
            "{}/{} MB ({:.1}% free)",
            available / (1024 * 1024),
            total / (1024 * 1024),
            (available as f64 / total as f64) * 100.0
        )
    } else if total > 0 {
        format!("Total {} MB", total / (1024 * 1024))
    } else {
        String::new()
    }
}
pub fn shorten_path(path: &str) -> Result<String> {
    let path = PathBuf::from(path);
    let should_skip = path.starts_with("/rustc/") || path.starts_with("\\rustc\\");

    let shortened = path
        .iter()
        .skip(if should_skip { 3 } else { 0 })
        .collect::<PathBuf>();

    Ok(shortened.to_string_lossy().to_string())
}
