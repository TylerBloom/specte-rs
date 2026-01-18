use ratatui::layout::Rect;
use ratatui::text::{Line, Span, Text};
use ratatui::widgets::Paragraph;

use super::PROMPT;

#[derive(Debug, Default)]
pub struct CliHistory {
    // The history that is displayed (contains dups)
    visual_history: Vec<String>,
    // The "up arrow" history (removes dups and is ordered by last use)
    pub minimized_history: Vec<String>,
}

impl CliHistory {
    pub fn push_command_output(&mut self, value: String) {
        self.push_visual_history(value);
    }

    pub fn push_command(&mut self, value: String) {
        self.visual_history.push(format!("{PROMPT}{value}"));
        self.minimized_history.retain(|val| val != &value);
        self.minimized_history.push(value);
    }

    pub fn push_visual_history(&mut self, value: String) {
        self.visual_history
            .extend(value.lines().map(ToOwned::to_owned));
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
        let lines: Vec<_> = entries
            .iter()
            .map(String::as_str)
            .flat_map(|s| wrap_lines(width, s))
            .map(Line::from)
            .collect();

        // Use only the lines that will fit into the box
        let index = lines.len().saturating_sub(rect.height as usize - 1);
        let lines = lines[index..].to_vec();

        (lines.len() as u16, Paragraph::new(Text::from(lines)))
    }
}

fn wrap_lines(width: usize, s: &str) -> impl Iterator<Item = Span<'static>> {
    let mut chars = s.chars();
    std::iter::from_fn(move || {
        let digest: Span = chars.by_ref().take(width).collect::<String>().into();
        (!digest.content.is_empty()).then_some(digest)
    })
}
