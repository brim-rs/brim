//! Terminal back-end for emitting diagnostics.

use std::io::Write;
use std::str::FromStr;
use anstream::ColorChoice;
use brim_span::files::Files;

mod config;
mod renderer;
mod views;

pub use self::config::{Chars, Config, Styles};
use crate::reporting::{diagnostic::Diagnostic};

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct ColorArg(pub ColorChoice);

impl ColorArg {
    pub const VARIANTS: &'static [&'static str] = &["auto", "always", "ansi", "never"];
}

impl FromStr for ColorArg {
    type Err = &'static str;

    fn from_str(src: &str) -> Result<ColorArg, &'static str> {
        match src {
            _ if src.eq_ignore_ascii_case("auto") => Ok(ColorArg(ColorChoice::Auto)),
            _ if src.eq_ignore_ascii_case("always") => Ok(ColorArg(ColorChoice::Always)),
            _ if src.eq_ignore_ascii_case("ansi") => Ok(ColorArg(ColorChoice::AlwaysAnsi)),
            _ if src.eq_ignore_ascii_case("never") => Ok(ColorArg(ColorChoice::Never)),
            _ => Err("valid values: auto, always, ansi, never"),
        }
    }
}

impl From<ColorArg> for ColorChoice {
    fn from(x: ColorArg) -> Self {
        x.0
    }
}

pub fn emit<'files, F: Files<'files>>(
    writer: &mut dyn Write,
    config: &Config,
    files: &'files F,
    diagnostic: &Diagnostic<F::FileId>,
) -> Result<(), brim_span::files::Error> {
    use self::{
        renderer::Renderer,
        views::{RichDiagnostic},
    };

    let mut renderer = Renderer::new(writer, config);
    RichDiagnostic::new(diagnostic, config).render(files, &mut renderer)
}
