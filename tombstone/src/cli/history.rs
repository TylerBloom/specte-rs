use ratatui::{layout::Rect, text::Text, widgets::Paragraph};

use crate::cli::PROMPT;

#[derive(Debug, Default)]
pub struct CliHistory {
    // The hsitory that is displayed (contains dups)
    visual_history: Vec<String>,
    // The "up arrow" history (removes dups and is ordered by last use)
    pub minimized_history: Vec<String>,
}

impl CliHistory {
    pub fn push(&mut self, value: String) {
        self.visual_history.push(value.clone());
        self.minimized_history.retain(|val| val != &value);
        self.minimized_history.push(value);
    }

    pub fn push_visual_history(&mut self, value: String) {
        self.visual_history.push(value);
    }

    pub fn render(&self, rect: Rect) -> (u16, Paragraph<'_>) {
        let digest = std::cmp::min(self.visual_history.len() as u16, rect.height - 1);
        let iter = self
            .visual_history
            .iter()
            .rev()
            .take((rect.height - 1) as usize)
            .rev()
            .map(|s| format!("{PROMPT}{s}"));
        (digest, Paragraph::new(Text::from_iter(iter)))
    }
}
