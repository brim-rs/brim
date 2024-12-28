use crate::reporting::diagnostic::{LabelStyle, Severity};
use termcolor::{Color, ColorSpec};

#[derive(Clone, Debug)]
pub struct Config {
    pub display_style: DisplayStyle,
    pub tab_width: usize,
    pub styles: Styles,
    pub chars: Chars,
    pub start_context_lines: usize,
    pub end_context_lines: usize,
    pub before_label_lines: usize,
    pub after_label_lines: usize,
}

impl Default for Config {
    fn default() -> Config {
        Config {
            display_style: DisplayStyle::Rich,
            tab_width: 4,
            styles: Styles::default(),
            chars: Chars::default(),
            start_context_lines: 3,
            end_context_lines: 1,
            before_label_lines: 0,
            after_label_lines: 0,
        }
    }
}

#[derive(Clone, Debug)]
pub enum DisplayStyle {
    Rich,

    Medium,

    Short,
}

#[derive(Clone, Debug)]
pub struct Styles {
    pub header_bug: ColorSpec,

    pub header_error: ColorSpec,

    pub header_warning: ColorSpec,

    pub header_note: ColorSpec,

    pub header_help: ColorSpec,

    pub header_message: ColorSpec,

    pub primary_label_bug: ColorSpec,

    pub primary_label_error: ColorSpec,

    pub primary_label_warning: ColorSpec,

    pub primary_label_note: ColorSpec,

    pub primary_label_help: ColorSpec,

    pub secondary_label: ColorSpec,

    pub line_number: ColorSpec,

    pub source_border: ColorSpec,

    pub note_bullet: ColorSpec,
}

impl Styles {
    pub fn header(&self, severity: Severity) -> &ColorSpec {
        match severity {
            Severity::Bug => &self.header_bug,
            Severity::Error => &self.header_error,
            Severity::Warning => &self.header_warning,
            Severity::Note => &self.header_note,
            Severity::Help => &self.header_help,
        }
    }

    pub fn label(&self, severity: Severity, label_style: LabelStyle) -> &ColorSpec {
        match (label_style, severity) {
            (LabelStyle::Primary, Severity::Bug) => &self.primary_label_bug,
            (LabelStyle::Primary, Severity::Error) => &self.primary_label_error,
            (LabelStyle::Primary, Severity::Warning) => &self.primary_label_warning,
            (LabelStyle::Primary, Severity::Note) => &self.primary_label_note,
            (LabelStyle::Primary, Severity::Help) => &self.primary_label_help,
            _ => todo!("Styles::label"),
        }
    }

    #[doc(hidden)]
    pub fn with_blue(blue: Color) -> Styles {
        let header = ColorSpec::new().set_bold(true).set_intense(true).clone();

        Styles {
            header_bug: header.clone().set_fg(Some(Color::Red)).clone(),
            header_error: header.clone().set_fg(Some(Color::Red)).clone(),
            header_warning: header.clone().set_fg(Some(Color::Yellow)).clone(),
            header_note: header.clone().set_fg(Some(Color::Green)).clone(),
            header_help: header.clone().set_fg(Some(Color::Cyan)).clone(),
            header_message: header,

            primary_label_bug: ColorSpec::new().set_fg(Some(Color::Red)).clone(),
            primary_label_error: ColorSpec::new().set_fg(Some(Color::Red)).clone(),
            primary_label_warning: ColorSpec::new().set_fg(Some(Color::Yellow)).clone(),
            primary_label_note: ColorSpec::new().set_fg(Some(Color::Green)).clone(),
            primary_label_help: ColorSpec::new().set_fg(Some(Color::Cyan)).clone(),
            secondary_label: ColorSpec::new().set_fg(Some(blue)).clone(),

            line_number: ColorSpec::new().set_fg(Some(blue)).clone(),
            source_border: ColorSpec::new().set_fg(Some(blue)).clone(),
            note_bullet: ColorSpec::new().set_fg(Some(blue)).clone(),
        }
    }
}

impl Default for Styles {
    fn default() -> Styles {
        #[cfg(windows)]
        const BLUE: Color = Color::Cyan;
        #[cfg(not(windows))]
        const BLUE: Color = Color::Blue;

        Self::with_blue(BLUE)
    }
}

#[derive(Clone, Debug)]
pub struct Chars {
    pub snippet_start: String,
    pub source_border_left: char,
    pub source_border_left_break: char,
    pub note_bullet: char,
    pub single_primary_caret: char,
    pub single_secondary_caret: char,
    pub multi_primary_caret_start: char,
    pub multi_primary_caret_end: char,
    pub multi_secondary_caret_start: char,
    pub multi_secondary_caret_end: char,
    pub multi_top_left: char,
    pub multi_top: char,
    pub multi_bottom_left: char,
    pub multi_bottom: char,
    pub multi_left: char,
    pub pointer_left: char,
}

impl Default for Chars {
    fn default() -> Chars {
        Chars::box_drawing()
    }
}

impl Chars {
    pub fn box_drawing() -> Chars {
        Chars {
            snippet_start: "┌─".into(),
            source_border_left: '│',
            source_border_left_break: '·',
            note_bullet: '=',
            single_primary_caret: '^',
            single_secondary_caret: '-',
            multi_primary_caret_start: '^',
            multi_primary_caret_end: '^',
            multi_secondary_caret_start: '\'',
            multi_secondary_caret_end: '\'',
            multi_top_left: '╭',
            multi_top: '─',
            multi_bottom_left: '╰',
            multi_bottom: '─',
            multi_left: '│',
            pointer_left: '│',
        }
    }

    pub fn ascii() -> Chars {
        Chars {
            snippet_start: "-->".into(),
            source_border_left: '|',
            source_border_left_break: '.',
            note_bullet: '=',
            single_primary_caret: '^',
            single_secondary_caret: '-',
            multi_primary_caret_start: '^',
            multi_primary_caret_end: '^',
            multi_secondary_caret_start: '\'',
            multi_secondary_caret_end: '\'',
            multi_top_left: '/',
            multi_top: '-',
            multi_bottom_left: '\\',
            multi_bottom: '-',
            multi_left: '|',
            pointer_left: '|',
        }
    }
}
