use ratatui::{layout::Rect, text::Text, widgets::Paragraph};

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
        let digest = std::cmp::min(self.visual_history.len() as u16, rect.height - 1);
        let iter = self
            .visual_history
            .iter()
            .rev()
            .take((rect.height - 1) as usize)
            .rev()
            .map(String::as_str);
        (digest, Paragraph::new(Text::from_iter(iter)))
    }
}
