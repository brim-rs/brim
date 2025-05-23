use crate::reporting::{
    diagnostic::{LabelStyle, Severity},
    term::{Chars, DiagConfig, Styles},
};
use anstyle::Style;
use brim_span::files::{FilesError, Location};
use std::{
    io::{self, Write},
    ops::Range,
};

pub struct Locus {
    pub name: String,
    pub location: Location,
}

pub type SingleLabel = (LabelStyle, Range<usize>, String);

pub enum MultiLabel<'diagnostic> {
    Top(usize),

    Left,

    Bottom(usize, &'diagnostic str),
}

#[derive(Copy, Clone)]
enum VerticalBound {
    Top,
    Bottom,
}

type Underline = (LabelStyle, VerticalBound);

pub struct Renderer<'writer, 'config> {
    writer: &'writer mut dyn Write,
    config: &'config DiagConfig,
}

impl<'writer, 'config> Renderer<'writer, 'config> {
    pub fn new(
        writer: &'writer mut dyn Write,
        config: &'config DiagConfig,
    ) -> Renderer<'writer, 'config> {
        Renderer { writer, config }
    }

    fn chars(&self) -> &'config Chars {
        &self.config.chars
    }

    fn styles(&self) -> &'config Styles {
        &self.config.styles
    }

    pub fn render_header(
        &mut self,
        locus: Option<&Locus>,
        severity: Severity,
        code: Option<&str>,
        message: &str,
    ) -> Result<(), FilesError> {
        if let Some(locus) = locus {
            self.snippet_locus(locus)?;
            write!(self, ": ")?;
        }

        self.set_color(self.styles().header(severity))?;
        match severity {
            Severity::Bug => write!(self, "bug")?,
            Severity::Error => write!(self, "error")?,
            Severity::Warning => write!(self, "warning")?,
            Severity::Help => write!(self, "help")?,
            Severity::Note => write!(self, "note")?,
        }

        if let Some(code) = &code.filter(|code| !code.is_empty()) {
            write!(self, "[{}]", code)?;
        }

        self.reset()?;
        self.set_color(&Style::new().dimmed())?;
        self.write(b": ")?;
        self.reset()?;
        write!(self, "{}", message)?;

        writeln!(self)?;

        Ok(())
    }

    pub fn render_empty(&mut self) -> Result<(), FilesError> {
        writeln!(self)?;
        Ok(())
    }

    pub fn render_snippet_start(
        &mut self,
        outer_padding: usize,
        locus: &Locus,
    ) -> Result<(), FilesError> {
        self.outer_gutter(outer_padding)?;

        self.set_color(&self.styles().source_border)?;
        write!(self, "{}", self.chars().snippet_start)?;
        self.reset()?;

        write!(self, " ")?;
        self.snippet_locus(locus)?;

        writeln!(self)?;

        Ok(())
    }

    pub fn render_snippet_source(
        &mut self,
        outer_padding: usize,
        line_number: usize,
        source: &str,
        severity: Severity,
        single_labels: &[SingleLabel],
        num_multi_labels: usize,
        multi_labels: &[(usize, LabelStyle, MultiLabel<'_>)],
    ) -> Result<(), FilesError> {
        let source = source.trim_end_matches(['\n', '\r', '\0'].as_ref());

        {
            self.outer_gutter_number(line_number, outer_padding)?;
            self.border_left()?;

            let mut multi_labels_iter = multi_labels.iter().peekable();
            for label_column in 0..num_multi_labels {
                match multi_labels_iter.peek() {
                    Some((label_index, label_style, label)) if *label_index == label_column => {
                        match label {
                            MultiLabel::Top(start)
                                if *start <= source.len() - source.trim_start().len() =>
                            {
                                self.label_multi_top_left(severity, label_style.clone())?;
                            }
                            MultiLabel::Top(..) => self.inner_gutter_space()?,
                            MultiLabel::Left | MultiLabel::Bottom(..) => {
                                self.label_multi_left(severity, label_style.clone(), None)?;
                            }
                        }
                        multi_labels_iter.next();
                    }
                    Some((_, _, _)) | None => self.inner_gutter_space()?,
                }
            }

            write!(self, " ")?;
            let mut in_primary = false;
            for (metrics, ch) in self.char_metrics(source.char_indices()) {
                let column_range = metrics.byte_index..(metrics.byte_index + ch.len_utf8());

                let is_primary = single_labels.iter().any(|(ls, range, _)| {
                    ls.clone() == LabelStyle::Primary && is_overlapping(range, &column_range)
                }) || multi_labels.iter().any(|(_, ls, label)| {
                    ls.clone() == LabelStyle::Primary
                        && match label {
                            MultiLabel::Top(start) => column_range.start >= *start,
                            MultiLabel::Left => true,
                            MultiLabel::Bottom(start, _) => column_range.end <= *start,
                        }
                });

                if is_primary && !in_primary {
                    self.set_color(self.styles().label(severity, LabelStyle::Primary))?;
                    in_primary = true;
                } else if !is_primary && in_primary {
                    self.reset()?;
                    in_primary = false;
                }

                match ch {
                    '\t' => (0..metrics.unicode_width).try_for_each(|_| write!(self, " "))?,
                    _ => write!(self, "{}", ch)?,
                }
            }
            if in_primary {
                self.reset()?;
            }
            writeln!(self)?;
        }

        if !single_labels.is_empty() {
            let mut num_messages = 0;
            let mut max_label_start = 0;
            let mut max_label_end = 0;
            let mut trailing_label = None;

            for (label_index, label) in single_labels.iter().enumerate() {
                let (_, range, message) = label;
                if !message.is_empty() {
                    num_messages += 1;
                }
                max_label_start = std::cmp::max(max_label_start, range.start);
                max_label_end = std::cmp::max(max_label_end, range.end);

                if range.end == max_label_end {
                    if message.is_empty() {
                        trailing_label = None;
                    } else {
                        trailing_label = Some((label_index, label));
                    }
                }
            }
            if let Some((trailing_label_index, (_, trailing_range, _))) = trailing_label {
                if single_labels
                    .iter()
                    .enumerate()
                    .filter(|(label_index, _)| *label_index != trailing_label_index)
                    .any(|(_, (_, range, _))| is_overlapping(trailing_range, range))
                {
                    trailing_label = None;
                }
            }

            self.outer_gutter(outer_padding)?;
            self.border_left()?;
            self.inner_gutter(severity, num_multi_labels, multi_labels)?;
            write!(self, " ")?;

            let mut previous_label_style = None;
            let placeholder_metrics = Metrics { byte_index: source.len(), unicode_width: 1 };
            for (metrics, ch) in self
                .char_metrics(source.char_indices())
                .chain(std::iter::once((placeholder_metrics, '\0')))
            {
                let column_range = metrics.byte_index..(metrics.byte_index + ch.len_utf8());
                let current_label_style = single_labels
                    .iter()
                    .filter(|(_, range, _)| is_overlapping(range, &column_range))
                    .map(|(label_style, _, _)| label_style.clone())
                    .max_by_key(label_priority_key);

                if previous_label_style != current_label_style {
                    match current_label_style {
                        None => self.reset()?,
                        Some(ref label_style) => {
                            self.set_color(self.styles().label(severity, label_style.clone()))?;
                        }
                    }
                }

                let caret_ch = match current_label_style {
                    Some(LabelStyle::Primary) => Some(self.chars().single_primary_caret),
                    Some(LabelStyle::Error) => Some(self.chars().single_primary_caret),
                    Some(LabelStyle::Warning) => Some(self.chars().single_primary_caret),
                    Some(LabelStyle::Add(_)) => Some(self.chars().plus),
                    Some(LabelStyle::Note) => Some(self.chars().note_bullet),

                    None if metrics.byte_index < max_label_end => Some(' '),
                    None => None,
                };
                if let Some(caret_ch) = caret_ch {
                    (0..metrics.unicode_width).try_for_each(|_| write!(self, "{}", caret_ch))?;
                }

                previous_label_style = current_label_style;
            }

            if previous_label_style.is_some() {
                self.reset()?;
            }

            if let Some((_, (label_style, _, message))) = trailing_label {
                write!(self, " ")?;
                self.set_color(self.styles().label(severity, label_style.clone()))?;
                write!(self, "{}", message)?;
                self.reset()?;
            }
            writeln!(self)?;

            if num_messages > trailing_label.iter().count() {
                self.outer_gutter(outer_padding)?;
                self.border_left()?;
                self.inner_gutter(severity, num_multi_labels, multi_labels)?;
                write!(self, " ")?;
                self.caret_pointers(
                    severity,
                    max_label_start,
                    single_labels,
                    trailing_label,
                    source.char_indices(),
                )?;
                writeln!(self)?;

                for (label_style, range, message) in
                    hanging_labels(single_labels, trailing_label).rev()
                {
                    self.outer_gutter(outer_padding)?;
                    self.border_left()?;
                    self.inner_gutter(severity, num_multi_labels, multi_labels)?;
                    write!(self, " ")?;
                    self.caret_pointers(
                        severity,
                        max_label_start,
                        single_labels,
                        trailing_label,
                        source
                            .char_indices()
                            .take_while(|(byte_index, _)| *byte_index < range.start),
                    )?;
                    self.set_color(self.styles().label(severity, label_style.clone()))?;
                    write!(self, "{}", message)?;
                    self.reset()?;
                    writeln!(self)?;
                }
            }
        }

        for (multi_label_index, (_, label_style, label)) in multi_labels.iter().enumerate() {
            let (label_style, range, bottom_message) = match label {
                MultiLabel::Left => continue,
                MultiLabel::Top(start) if *start <= source.len() - source.trim_start().len() => {
                    continue;
                }
                MultiLabel::Top(range) => (label_style.clone(), range, None),
                MultiLabel::Bottom(range, message) => (label_style.clone(), range, Some(message)),
            };

            self.outer_gutter(outer_padding)?;
            self.border_left()?;

            let mut underline = None;
            let mut multi_labels_iter = multi_labels.iter().enumerate().peekable();
            for label_column in 0..num_multi_labels {
                match multi_labels_iter.peek() {
                    Some((i, (label_index, ls, label))) if *label_index == label_column => {
                        match label {
                            MultiLabel::Left => {
                                self.label_multi_left(
                                    severity,
                                    ls.clone(),
                                    underline.clone().map(|(s, _)| s),
                                )?;
                            }
                            MultiLabel::Top(..) if multi_label_index > *i => {
                                self.label_multi_left(
                                    severity,
                                    ls.clone(),
                                    underline.clone().map(|(s, _)| s),
                                )?;
                            }
                            MultiLabel::Bottom(..) if multi_label_index < *i => {
                                self.label_multi_left(
                                    severity,
                                    ls.clone(),
                                    underline.clone().map(|(s, _)| s),
                                )?;
                            }
                            MultiLabel::Top(..) if multi_label_index == *i => {
                                underline = Some((ls.clone(), VerticalBound::Top));
                                self.label_multi_top_left(severity, label_style.clone())?
                            }
                            MultiLabel::Bottom(..) if multi_label_index == *i => {
                                underline = Some((ls.clone(), VerticalBound::Bottom));
                                self.label_multi_bottom_left(severity, label_style.clone())?;
                            }
                            MultiLabel::Top(..) | MultiLabel::Bottom(..) => {
                                self.inner_gutter_column(severity, underline.clone())?;
                            }
                        }
                        multi_labels_iter.next();
                    }
                    Some((_, _)) | None => self.inner_gutter_column(severity, underline.clone())?,
                }
            }

            match bottom_message {
                None => self.label_multi_top_caret(severity, label_style, source, *range)?,
                Some(message) => {
                    self.label_multi_bottom_caret(severity, label_style, source, *range, message)?
                }
            }
        }

        Ok(())
    }

    pub fn render_snippet_empty(
        &mut self,
        outer_padding: usize,
        severity: Severity,
        num_multi_labels: usize,
        multi_labels: &[(usize, LabelStyle, MultiLabel<'_>)],
    ) -> Result<(), FilesError> {
        self.outer_gutter(outer_padding)?;
        self.border_left()?;
        self.inner_gutter(severity, num_multi_labels, multi_labels)?;
        writeln!(self)?;
        Ok(())
    }

    pub fn render_snippet_break(
        &mut self,
        outer_padding: usize,
        severity: Severity,
        num_multi_labels: usize,
        multi_labels: &[(usize, LabelStyle, MultiLabel<'_>)],
    ) -> Result<(), FilesError> {
        self.outer_gutter(outer_padding)?;
        self.border_left_break()?;
        self.inner_gutter(severity, num_multi_labels, multi_labels)?;
        writeln!(self)?;
        Ok(())
    }

    pub fn render_snippet_note(
        &mut self,
        outer_padding: usize,
        message: &str,
    ) -> Result<(), FilesError> {
        for (note_line_index, line) in message.lines().enumerate() {
            self.outer_gutter(outer_padding)?;
            match note_line_index {
                0 => {
                    self.set_color(&self.styles().note_bullet)?;
                    write!(self, "{}", self.chars().note_bullet)?;
                    self.reset()?;
                }
                _ => write!(self, " ")?,
            }

            writeln!(self, " {}", line)?;
        }

        Ok(())
    }

    fn char_metrics(
        &self,
        char_indices: impl Iterator<Item = (usize, char)>,
    ) -> impl Iterator<Item = (Metrics, char)> {
        use unicode_width::UnicodeWidthChar;

        let tab_width = self.config.tab_width;
        let mut unicode_column = 0;

        char_indices.map(move |(byte_index, ch)| {
            let metrics = Metrics {
                byte_index,
                unicode_width: match (ch, tab_width) {
                    ('\t', 0) => 0,
                    ('\t', _) => tab_width - (unicode_column % tab_width),
                    (ch, _) => ch.width().unwrap_or(0),
                },
            };
            unicode_column += metrics.unicode_width;

            (metrics, ch)
        })
    }

    fn snippet_locus(&mut self, locus: &Locus) -> Result<(), FilesError> {
        write!(
            self,
            "{name}:{line_number}:{column_number}",
            name = locus.name,
            line_number = locus.location.line_number,
            column_number = locus.location.column_number,
        )?;
        Ok(())
    }

    fn outer_gutter(&mut self, outer_padding: usize) -> Result<(), FilesError> {
        write!(self, "{space: >width$} ", space = "", width = outer_padding)?;
        Ok(())
    }

    fn outer_gutter_number(
        &mut self,
        line_number: usize,
        outer_padding: usize,
    ) -> Result<(), FilesError> {
        self.set_color(&self.styles().line_number)?;
        write!(self, "{line_number: >width$}", line_number = line_number, width = outer_padding,)?;
        self.reset()?;
        write!(self, " ")?;
        Ok(())
    }

    fn border_left(&mut self) -> Result<(), FilesError> {
        self.set_color(&self.styles().source_border)?;
        write!(self, "{}", self.chars().source_border_left)?;
        self.reset()?;
        Ok(())
    }

    fn border_left_break(&mut self) -> Result<(), FilesError> {
        self.set_color(&self.styles().source_border)?;
        write!(self, "{}", self.chars().source_border_left_break)?;
        self.reset()?;
        Ok(())
    }

    fn caret_pointers(
        &mut self,
        severity: Severity,
        max_label_start: usize,
        single_labels: &[SingleLabel],
        trailing_label: Option<(usize, &SingleLabel)>,
        char_indices: impl Iterator<Item = (usize, char)>,
    ) -> Result<(), FilesError> {
        for (metrics, ch) in self.char_metrics(char_indices) {
            let column_range = metrics.byte_index..(metrics.byte_index + ch.len_utf8());
            let label_style = hanging_labels(single_labels, trailing_label)
                .filter(|(_, range, _)| column_range.contains(&range.start))
                .map(|(label_style, _, _)| label_style.clone())
                .max_by_key(label_priority_key);

            let mut spaces = match label_style {
                None => 0..metrics.unicode_width,
                Some(label_style) => {
                    self.set_color(self.styles().label(severity, label_style))?;
                    write!(self, "{}", self.chars().pointer_left)?;
                    self.reset()?;
                    1..metrics.unicode_width
                }
            };

            if metrics.byte_index <= max_label_start {
                spaces.try_for_each(|_| write!(self, " "))?;
            }
        }

        Ok(())
    }

    fn label_multi_left(
        &mut self,
        severity: Severity,
        label_style: LabelStyle,
        underline: Option<LabelStyle>,
    ) -> Result<(), FilesError> {
        match underline {
            None => write!(self, " ")?,

            Some(label_style) => {
                self.set_color(self.styles().label(severity, label_style))?;
                write!(self, "{}", self.chars().multi_top)?;
                self.reset()?;
            }
        }
        self.set_color(self.styles().label(severity, label_style))?;
        write!(self, "{}", self.chars().multi_left)?;
        self.reset()?;
        Ok(())
    }

    fn label_multi_top_left(
        &mut self,
        severity: Severity,
        label_style: LabelStyle,
    ) -> Result<(), FilesError> {
        write!(self, " ")?;
        self.set_color(self.styles().label(severity, label_style))?;
        write!(self, "{}", self.chars().multi_top_left)?;
        self.reset()?;
        Ok(())
    }

    fn label_multi_bottom_left(
        &mut self,
        severity: Severity,
        label_style: LabelStyle,
    ) -> Result<(), FilesError> {
        write!(self, " ")?;
        self.set_color(self.styles().label(severity, label_style))?;
        write!(self, "{}", self.chars().multi_bottom_left)?;
        self.reset()?;
        Ok(())
    }

    fn label_multi_top_caret(
        &mut self,
        severity: Severity,
        label_style: LabelStyle,
        source: &str,
        start: usize,
    ) -> Result<(), FilesError> {
        self.set_color(self.styles().label(severity, label_style.clone()))?;

        for (metrics, _) in self
            .char_metrics(source.char_indices())
            .take_while(|(metrics, _)| metrics.byte_index < start + 1)
        {
            (0..metrics.unicode_width)
                .try_for_each(|_| write!(self, "{}", self.chars().multi_top))?;
        }

        let caret_start = match label_style.clone() {
            LabelStyle::Primary => self.config.chars.multi_primary_caret_start,
            LabelStyle::Error => self.config.chars.single_primary_caret,
            LabelStyle::Warning => self.config.chars.single_primary_caret,
            LabelStyle::Add(_) => self.config.chars.plus,
            LabelStyle::Note => self.config.chars.note_bullet,
        };

        write!(self, "{}", caret_start)?;
        self.reset()?;
        writeln!(self)?;
        Ok(())
    }

    fn label_multi_bottom_caret(
        &mut self,
        severity: Severity,
        label_style: LabelStyle,
        source: &str,
        start: usize,
        message: &str,
    ) -> Result<(), FilesError> {
        self.set_color(self.styles().label(severity, label_style.clone()))?;

        for (metrics, _) in self
            .char_metrics(source.char_indices())
            .take_while(|(metrics, _)| metrics.byte_index < start)
        {
            (0..metrics.unicode_width)
                .try_for_each(|_| write!(self, "{}", self.chars().multi_bottom))?;
        }

        let caret_end = match label_style {
            LabelStyle::Primary => self.config.chars.multi_primary_caret_start,
            LabelStyle::Error => self.config.chars.single_primary_caret,
            LabelStyle::Warning => self.config.chars.single_primary_caret,
            LabelStyle::Add(_) => self.config.chars.plus,
            LabelStyle::Note => self.config.chars.note_bullet,
        };
        write!(self, "{}", caret_end)?;
        if !message.is_empty() {
            write!(self, " {}", message)?;
        }
        self.reset()?;
        writeln!(self)?;
        Ok(())
    }

    fn inner_gutter_column(
        &mut self,
        severity: Severity,
        underline: Option<Underline>,
    ) -> Result<(), FilesError> {
        match underline {
            None => self.inner_gutter_space(),
            Some((label_style, vertical_bound)) => {
                self.set_color(self.styles().label(severity, label_style))?;
                let ch = match vertical_bound {
                    VerticalBound::Top => self.config.chars.multi_top,
                    VerticalBound::Bottom => self.config.chars.multi_bottom,
                };
                write!(self, "{0}{0}", ch)?;
                self.reset()?;
                Ok(())
            }
        }
    }

    fn inner_gutter_space(&mut self) -> Result<(), FilesError> {
        write!(self, "  ")?;
        Ok(())
    }

    fn inner_gutter(
        &mut self,
        severity: Severity,
        num_multi_labels: usize,
        multi_labels: &[(usize, LabelStyle, MultiLabel<'_>)],
    ) -> Result<(), FilesError> {
        let mut multi_labels_iter = multi_labels.iter().peekable();
        for label_column in 0..num_multi_labels {
            match multi_labels_iter.peek() {
                Some((label_index, ls, label)) if *label_index == label_column => match label {
                    MultiLabel::Left | MultiLabel::Bottom(..) => {
                        self.label_multi_left(severity, ls.clone(), None)?;
                        multi_labels_iter.next();
                    }
                    MultiLabel::Top(..) => {
                        self.inner_gutter_space()?;
                        multi_labels_iter.next();
                    }
                },
                Some((_, _, _)) | None => self.inner_gutter_space()?,
            }
        }

        Ok(())
    }
}

impl Write for Renderer<'_, '_> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.writer.write(buf)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.writer.flush()
    }
}

impl Renderer<'_, '_> {
    fn set_color(&mut self, spec: &Style) -> io::Result<()> {
        self.writer.write(spec.to_string().as_bytes())?;

        Ok(())
    }

    fn reset(&mut self) -> io::Result<()> {
        self.writer.write(b"\x1b[0m")?;

        Ok(())
    }
}

struct Metrics {
    byte_index: usize,
    unicode_width: usize,
}

fn is_overlapping(range0: &Range<usize>, range1: &Range<usize>) -> bool {
    let start = std::cmp::max(range0.start, range1.start);
    let end = std::cmp::min(range0.end, range1.end);
    start < end
}

fn label_priority_key(label_style: &LabelStyle) -> u8 {
    match label_style {
        LabelStyle::Add(_) => 1,
        LabelStyle::Warning => 2,
        LabelStyle::Primary => 3,
        LabelStyle::Error => 4,
        LabelStyle::Note => 5,
    }
}

fn hanging_labels<'labels, 'diagnostic>(
    single_labels: &'labels [SingleLabel],
    trailing_label: Option<(usize, &'labels SingleLabel)>,
) -> impl 'labels + DoubleEndedIterator<Item = &'labels SingleLabel> {
    single_labels
        .iter()
        .enumerate()
        .filter(|(_, (_, _, message))| !message.is_empty())
        .filter(move |(i, _)| trailing_label.is_none_or(|(j, _)| *i != j))
        .map(|(_, label)| label)
}
