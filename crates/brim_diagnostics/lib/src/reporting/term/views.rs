use crate::reporting::{
    diagnostic::{Diagnostic, LabelStyle},
    term::{
        DiagConfig,
        renderer::{Locus, MultiLabel, Renderer, SingleLabel},
    },
};
use brim_span::files::{Files, FilesError, Location};
use std::ops::Range;

fn count_digits(n: usize) -> usize {
    (n.saturating_add(1) as f64).log10().ceil() as usize
}

pub struct RichDiagnostic<'diagnostic, 'config, FileId> {
    diagnostic: &'diagnostic Diagnostic<FileId>,
    config: &'config DiagConfig,
}

impl<'diagnostic, 'config, FileId> RichDiagnostic<'diagnostic, 'config, FileId>
where
    FileId: Copy + PartialEq,
{
    pub fn new(
        diagnostic: &'diagnostic Diagnostic<FileId>,
        config: &'config DiagConfig,
    ) -> RichDiagnostic<'diagnostic, 'config, FileId> {
        RichDiagnostic { diagnostic, config }
    }

    pub fn render<'files>(
        &self,
        files: &'files impl Files<'files, FileId = FileId>,
        renderer: &mut Renderer<'_, '_>,
    ) -> Result<(), FilesError>
    where
        FileId: 'files,
    {
        use std::collections::BTreeMap;

        struct LabeledFile<'diagnostic, FileId> {
            file_id: FileId,
            start: usize,
            name: String,
            location: Location,
            num_multi_labels: usize,
            lines: BTreeMap<usize, Line<'diagnostic>>,
            max_label_style: LabelStyle,
        }

        impl<'diagnostic, FileId> LabeledFile<'diagnostic, FileId> {
            fn get_or_insert_line(
                &mut self,
                line_index: usize,
                line_range: Range<usize>,
                line_number: usize,
            ) -> &mut Line<'diagnostic> {
                self.lines.entry(line_index).or_insert_with(|| Line {
                    range: line_range,
                    number: line_number,
                    single_labels: vec![],
                    multi_labels: vec![],
                    must_render: false,
                })
            }
        }

        struct Line<'diagnostic> {
            number: usize,
            range: Range<usize>,
            // TODO: How do we reuse these allocations?
            single_labels: Vec<SingleLabel>,
            multi_labels: Vec<(usize, LabelStyle, MultiLabel<'diagnostic>)>,
            must_render: bool,
        }

        let mut labeled_files = Vec::<LabeledFile<'_, _>>::new();
        let mut outer_padding = 0;

        for label in &self.diagnostic.labels {
            let start_line_index = files.line_index(label.file_id, label.range.start)?;
            let start_line_number = files.line_number(label.file_id, start_line_index)?;
            let start_line_range = files.line_range(label.file_id, start_line_index)?;
            let end_line_index = files.line_index(label.file_id, label.range.end)?;
            let end_line_number = files.line_number(label.file_id, end_line_index)?;
            let end_line_range = files.line_range(label.file_id, end_line_index)?;

            outer_padding = std::cmp::max(outer_padding, count_digits(start_line_number));
            outer_padding = std::cmp::max(outer_padding, count_digits(end_line_number));

            let labeled_file = match labeled_files
                .iter_mut()
                .find(|labeled_file| label.file_id == labeled_file.file_id)
            {
                Some(labeled_file) => {
                    if labeled_file.max_label_style > label.style
                        || (labeled_file.max_label_style == label.style
                            && labeled_file.start > label.range.start)
                    {
                        labeled_file.start = label.range.start;
                        labeled_file.location = files.location(label.file_id, label.range.start)?;
                        labeled_file.max_label_style = label.style.clone();
                    }
                    labeled_file
                }
                None => {
                    labeled_files.push(LabeledFile {
                        file_id: label.file_id,
                        start: label.range.start,
                        name: files.name(label.file_id)?.display().to_string(),
                        location: files.location(label.file_id, label.range.start)?,
                        num_multi_labels: 0,
                        lines: BTreeMap::new(),
                        max_label_style: label.style.clone(),
                    });
                    labeled_files.last_mut().expect("just pushed an element that disappeared")
                }
            };

            for offset in 1..self.config.before_label_lines + 1 {
                let index = if let Some(index) = start_line_index.checked_sub(offset) {
                    index
                } else {
                    break;
                };

                if let Ok(range) = files.line_range(label.file_id, index) {
                    let line =
                        labeled_file.get_or_insert_line(index, range, start_line_number - offset);
                    line.must_render = true;
                } else {
                    break;
                }
            }

            for offset in 1..self.config.after_label_lines + 1 {
                let index = end_line_index.checked_add(offset).expect("line index too big");

                if let Ok(range) = files.line_range(label.file_id, index) {
                    let line =
                        labeled_file.get_or_insert_line(index, range, end_line_number + offset);
                    line.must_render = true;
                } else {
                    break;
                }
            }

            if start_line_index == end_line_index {
                let label_start = label.range.start - start_line_range.start;
                let label_end =
                    usize::max(label.range.end - start_line_range.start, label_start + 1);

                let line = labeled_file.get_or_insert_line(
                    start_line_index,
                    start_line_range,
                    start_line_number,
                );

                let index = match line.single_labels.binary_search_by(|(_, range, _)| {
                    (range.start, range.end).cmp(&(label_start, label_end))
                }) {
                    Ok(index) | Err(index) => index,
                };

                line.single_labels.insert(
                    index,
                    (label.style.clone(), label_start..label_end, label.message.clone()),
                );

                line.must_render = true;
            } else {
                let label_index = labeled_file.num_multi_labels;
                labeled_file.num_multi_labels += 1;

                let label_start = label.range.start - start_line_range.start;

                let start_line = labeled_file.get_or_insert_line(
                    start_line_index,
                    start_line_range.clone(),
                    start_line_number,
                );

                start_line.multi_labels.push((
                    label_index,
                    label.style.clone(),
                    MultiLabel::Top(label_start),
                ));

                start_line.must_render = true;

                for line_index in (start_line_index + 1)..end_line_index {
                    let line_range = files.line_range(label.file_id, line_index)?;
                    let line_number = files.line_number(label.file_id, line_index)?;

                    outer_padding = std::cmp::max(outer_padding, count_digits(line_number));

                    let line = labeled_file.get_or_insert_line(line_index, line_range, line_number);

                    line.multi_labels.push((label_index, label.style.clone(), MultiLabel::Left));

                    line.must_render |= line_index - start_line_index
                        <= self.config.start_context_lines
                        || end_line_index - line_index <= self.config.end_context_lines;
                }

                let label_end = label.range.end - end_line_range.start;

                let end_line = labeled_file.get_or_insert_line(
                    end_line_index,
                    end_line_range,
                    end_line_number,
                );

                end_line.multi_labels.push((
                    label_index,
                    label.style.clone(),
                    MultiLabel::Bottom(label_end, &label.message),
                ));

                end_line.must_render = true;
            }
        }

        renderer.render_header(
            None,
            self.diagnostic.severity,
            self.diagnostic.code.as_deref(),
            self.diagnostic.message.as_str(),
        )?;

        let mut labeled_files = labeled_files.into_iter().peekable();
        while let Some(labeled_file) = labeled_files.next() {
            let source = files.source(labeled_file.file_id)?;
            let source = source.as_ref();

            if !labeled_file.lines.is_empty() {
                renderer.render_snippet_start(outer_padding, &Locus {
                    name: labeled_file.name,
                    location: labeled_file.location,
                })?;
                renderer.render_snippet_empty(
                    outer_padding,
                    self.diagnostic.severity,
                    labeled_file.num_multi_labels,
                    &[],
                )?;
            }

            let mut lines =
                labeled_file.lines.iter().filter(|(_, line)| line.must_render).peekable();

            while let Some((line_index, line)) = lines.next() {
                renderer.render_snippet_source(
                    outer_padding,
                    line.number,
                    &source[line.range.clone()],
                    self.diagnostic.severity,
                    &line.single_labels,
                    labeled_file.num_multi_labels,
                    &line.multi_labels,
                )?;

                if let Some((next_line_index, next_line)) = lines.peek() {
                    match next_line_index.checked_sub(*line_index) {
                        Some(1) => {}
                        Some(2) => {
                            let file_id = labeled_file.file_id;

                            let labels = labeled_file
                                .lines
                                .get(&(line_index + 1))
                                .map_or(&[][..], |line| &line.multi_labels[..]);

                            renderer.render_snippet_source(
                                outer_padding,
                                files.line_number(file_id, line_index + 1)?,
                                &source[files.line_range(file_id, line_index + 1)?],
                                self.diagnostic.severity,
                                &[],
                                labeled_file.num_multi_labels,
                                labels,
                            )?;
                        }
                        Some(_) | None => {
                            renderer.render_snippet_break(
                                outer_padding,
                                self.diagnostic.severity,
                                labeled_file.num_multi_labels,
                                &next_line.multi_labels,
                            )?;
                        }
                    }
                }
            }

            if labeled_files.peek().is_none() && self.diagnostic.notes.is_empty() {
            } else {
                renderer.render_snippet_empty(
                    outer_padding,
                    self.diagnostic.severity,
                    labeled_file.num_multi_labels,
                    &[],
                )?;
            }
        }

        for note in &self.diagnostic.notes {
            renderer.render_snippet_note(outer_padding, note)?;
        }
        renderer.render_empty()
    }
}
