use crate::reporting::diagnostic::{LabelStyle, Severity};
use anstyle::{AnsiColor, Color, Style};

#[derive(Clone, Debug)]
pub struct DiagConfig {
    pub tab_width: usize,
    pub styles: Styles,
    pub chars: Chars,
    pub start_context_lines: usize,
    pub end_context_lines: usize,
    pub before_label_lines: usize,
    pub after_label_lines: usize,
}

impl Default for DiagConfig {
    fn default() -> DiagConfig {
        DiagConfig {
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
pub struct Styles {
    pub header_bug: Style,
    pub header_error: Style,
    pub header_warning: Style,
    pub header_note: Style,
    pub header_help: Style,
    pub header_message: Style,

    // Primary label
    pub primary_label_bug: Style,
    pub primary_label_error: Style,
    pub primary_label_warning: Style,
    pub primary_label_note: Style,
    pub primary_label_help: Style,
    // Error label
    pub error_label: Style,
    // Warning label
    pub warning_label: Style,
    // Add label
    pub add_label: Style,

    pub line_number: Style,
    pub source_border: Style,
    pub note_bullet: Style,
}

impl Styles {
    pub fn header(&self, severity: Severity) -> &Style {
        match severity {
            Severity::Bug => &self.header_bug,
            Severity::Error => &self.header_error,
            Severity::Warning => &self.header_warning,
            Severity::Note => &self.header_note,
            Severity::Help => &self.header_help,
        }
    }

    pub fn label(&self, severity: Severity, label_style: LabelStyle) -> &Style {
        match (label_style, severity) {
            (LabelStyle::Primary, Severity::Bug) => &self.primary_label_bug,
            (LabelStyle::Primary, Severity::Error) => &self.primary_label_error,
            (LabelStyle::Primary, Severity::Warning) => &self.primary_label_warning,
            (LabelStyle::Primary, Severity::Note) => &self.primary_label_note,
            (LabelStyle::Primary, Severity::Help) => &self.primary_label_help,

            (LabelStyle::Error, _) => &self.error_label,
            (LabelStyle::Warning, _) => &self.warning_label,
            (LabelStyle::Add(_), _) => &self.add_label,
        }
    }

    pub fn new(blue: Color) -> Styles {
        let header = Style::new().bold().effects(anstyle::Effects::BOLD);

        Styles {
            header_bug: header.fg_color(Some(Color::Ansi(AnsiColor::Red))),
            header_error: header.fg_color(Some(Color::Ansi(AnsiColor::Red))),
            header_warning: header.fg_color(Some(Color::Ansi(AnsiColor::Yellow))),
            header_note: header.fg_color(Some(Color::Ansi(AnsiColor::Green))),
            header_help: header.fg_color(Some(Color::Ansi(AnsiColor::Cyan))),
            header_message: header,

            primary_label_bug: Style::new().fg_color(Some(Color::Ansi(AnsiColor::Cyan))),
            primary_label_error: Style::new().fg_color(Some(Color::Ansi(AnsiColor::Cyan))),
            primary_label_warning: Style::new().fg_color(Some(Color::Ansi(AnsiColor::Yellow))),
            primary_label_note: Style::new().fg_color(Some(Color::Ansi(AnsiColor::Green))),
            primary_label_help: Style::new().fg_color(Some(Color::Ansi(AnsiColor::Cyan))),
            error_label: Style::new().fg_color(Some(Color::Ansi(AnsiColor::Red))),
            warning_label: Style::new().fg_color(Some(Color::Ansi(AnsiColor::Yellow))),
            add_label: Style::new().fg_color(Some(Color::Ansi(AnsiColor::BrightGreen))),

            line_number: Style::new().fg_color(Some(blue)),
            source_border: Style::new().fg_color(Some(blue)),
            note_bullet: Style::new().fg_color(Some(blue)),
        }
    }
}

impl Default for Styles {
    fn default() -> Styles {
        // Blue is really difficult to see on the standard windows command line
        #[cfg(windows)]
        const BLUE: Color = Color::Ansi(AnsiColor::Cyan);
        #[cfg(not(windows))]
        const BLUE: Color = Color::Ansi(AnsiColor::Blue);

        Self::new(BLUE)
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
    pub plus: char,
}

impl Default for Chars {
    fn default() -> Chars {
        Chars::box_drawing()
    }
}

impl Chars {
    pub fn box_drawing() -> Chars {
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
            multi_top_left: '╭',
            multi_top: '─',
            multi_bottom_left: '╰',
            multi_bottom: '─',
            multi_left: '│',
            pointer_left: '│',
            plus: '+',
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
            plus: '+',
        }
    }
}
