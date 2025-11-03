use ratatui::Frame;
use ratatui::Terminal;
use ratatui::backend::Backend;
use ratatui::layout::Position;
use ratatui::layout::Rect;
use ratatui::text::Text;
use ratatui::widgets::Block;
use ratatui::widgets::Paragraph;

mod commands;

use crate::Command;
use crate::cli::commands::CommandProcessor;

#[derive(Debug, Default)]
pub struct Cli {
    processor: CommandProcessor,
    history: CliHistory,
    /// Tracks where in the CLI history the cursor is at. Used for events like Up and Down.
    /// The indexing does not map one-to-one to the index of CLI history. Rather, an index of 0
    /// means "new user input" while an index of 1 means the first item in the history.
    history_index: usize,
}

impl Cli {
    pub fn new() -> Self {
        Self {
            processor: CommandProcessor::new(),
            history: CliHistory::default(),
            history_index: 0,
        }
    }

    pub fn push_to_history(&mut self, data: String) {
        self.history.push(data);
    }

    /// (Re)Draws the line that users input. This allows the input to be rendered without needing
    /// to rerender the entire TUI.
    pub fn draw_input_line<B: Backend>(&self, term: &mut Terminal<B>) {
        self.processor.draw_input_line(term);
    }

    pub fn next_event(&mut self) -> Option<Command> {
        match self.processor.next_event()? {
            commands::CliEvent::Up => {
                let index =
                    std::cmp::min(self.history_index + 1, self.history.minimized_history.len());
                self.history_index = index;
                if !self.history.minimized_history.is_empty() {
                    // We know that the history is not empty, which means the index is at least 1
                    let len = self.history.minimized_history.len();
                    let s = self.history.minimized_history[len - index].clone();
                    self.processor.set_input(s);
                }
                None
            }
            commands::CliEvent::Down => {
                match (self.history_index, self.history_index.saturating_sub(1)) {
                    // Do nothing, the history cursor has not moved.
                    (0, 0) => {}
                    // We went from selecting a historical command to going back to user input.
                    (_, 0) => {
                        self.processor.set_input(String::new());
                        self.history_index = 0;
                    }
                    // We are still in the history, select that command, and set as input
                    (_, index) => {
                        self.history_index = index;
                        let len = self.history.minimized_history.len();
                        let s = self.history.minimized_history[len - index].clone();
                        self.processor.set_input(s);
                    }
                }
                None
            }
            commands::CliEvent::Cancel(s) => {
                self.history.push_visual_history(s);
                self.history_index = 0;
                Some(Command::Redraw)
            }
            commands::CliEvent::Resize => Some(Command::Redraw),
            commands::CliEvent::Command(s, command) => {
                if !s.is_empty() {
                    self.history.push(s);
                }
                self.history_index = 0;
                Some(command)
            }
        }
    }

    pub fn render(&self, frame: &mut Frame, rect: Rect) {
        let history = &self.history.visual_history;
        let block = Block::bordered()
            .title(" CLI ")
            .title_alignment(ratatui::layout::Alignment::Center);
        let Rect { x, y, height, .. } = block.inner(rect);
        let cursor_y = y + std::cmp::min(history.len(), (height - 1) as usize) as u16;
        let iter = history
            .iter()
            .rev()
            .take((height - 1) as usize)
            .rev()
            .map(|s| s.as_str());
        //  .chain(std::iter::once(user_input));
        let para = Paragraph::new(Text::from_iter(iter)).block(block);
        let pos = Position::new(x, cursor_y);
        frame.set_cursor_position(pos);
        frame.render_widget(para, rect);
        self.processor.set_cursor_position(pos);
    }
}

#[derive(Debug, Default)]
pub struct CliHistory {
    // The hsitory that is displayed (contains dups)
    visual_history: Vec<String>,
    // The "up arrow" history (removes dups and is ordered by last use)
    minimized_history: Vec<String>,
}

impl CliHistory {
    fn push(&mut self, value: String) {
        self.visual_history.push(value.clone());
        self.minimized_history.retain(|val| val != &value);
        self.minimized_history.push(value);
    }

    fn push_visual_history(&mut self, value: String) {
        self.visual_history.push(value);
    }
}
