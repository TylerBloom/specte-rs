use std::borrow::Cow;

use ratatui::layout::Rect;
use ratatui::style;
use ratatui::text::Line;
use ratatui::text::Span;
use ratatui::text::Text;
use ratatui::widgets::Paragraph;

use super::PROMPT;

#[derive(Debug, Default)]
pub struct CliHistory {
    // The history that is displayed (contains dups)
    visual_history: Vec<Line<'static>>,
    // The "up arrow" history (removes dups and is ordered by last use)
    pub minimized_history: Vec<String>,
}

impl CliHistory {
    pub fn push_command_output(&mut self, value: String) {
        self.push_visual_history(value.lines().map(ToOwned::to_owned).map(Line::from));
    }

    pub fn push_command(&mut self, value: String) {
        self.visual_history.push(format!("{PROMPT}{value}").into());
        self.minimized_history.retain(|val| val != &value);
        self.minimized_history.push(value);
    }

    pub fn push_visual_history(&mut self, lines: impl Iterator<Item = Line<'static>>) {
        self.visual_history.extend(lines);
    }

    pub fn render(&self, rect: Rect) -> (u16, Paragraph<'_>) {
        if self.visual_history.is_empty() {
            return (0, Paragraph::default());
        }
        // Calcuate the number of lines needed for the visual history, before wrapping.
        let index = self
            .visual_history
            .len()
            .saturating_sub(rect.height as usize);
        let entries = &self.visual_history[index..];

        // Collect the wrapped lines into in a vec to be trimmed
        let width = rect.width as usize;
        let lines = Vec::new();
        for line in entries {
            let Line {
                style,
                alignment,
                spans,
            } = line;
            let mut curr_width = 0;
            let mut buffer = Vec::new();
            for span in spans {
                let (start, end) = split_span(width, span);
                loop {}
            }
        }

        // Use only the lines that will fit into the box
        let index = lines.len().saturating_sub(rect.height as usize - 1);
        let lines = lines[index..].to_vec();

        (lines.len() as u16, Paragraph::new(Text::from(lines)))
    }
}

fn wrap_lines(width: usize, line: &Line<'static>) -> impl Iterator<Item = Line<'static>> {
    let style = line.style;
    let alignment = line.alignment;
    let mut spans = line.spans.clone().into_iter();
    let mut curr_width = 0;
    let mut buffer = Vec::new();
    std::iter::from_fn(move || {
        loop {
            // After the first trim, the remaining span could be longer than width
            if curr_width > width {
                let span = buffer.pop().unwrap();
                let (start, end) = split_span(width, span);
                buffer.push(start);
                let line = Line {
                    style,
                    alignment,
                    spans: std::mem::take(&mut buffer),
                };
                curr_width = end.width();
                buffer.push(end);
                return Some(line);
            }
            let Some(span) = spans.next() else {
                return buffer
                    .is_empty()
                    .then(|| std::mem::take(&mut buffer))
                    .map(|spans| Line {
                        style,
                        alignment,
                        spans,
                    });
            };
            if curr_width + span.width() > width {
                let (start, end) = split_span(width - curr_width, span);
                buffer.push(start);
                let line = Line {
                    style,
                    alignment,
                    spans: std::mem::take(&mut buffer),
                };
                curr_width = end.width();
                buffer.push(end);
                return Some(line);
            }
            curr_width += span.width();
            buffer.push(span);
        }
    })
}

fn split_span(width: usize, span: Span) -> (Span, Span) {
    let mut chars = span.content.chars();
    let start = Span {
        style: span.style,
        content: chars.by_ref().take(width).collect(),
    };
    let end = Span {
        style: span.style,
        content: chars.collect(),
    };
    (start, end)
}
