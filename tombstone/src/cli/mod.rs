use ratatui::Frame;
use ratatui::Terminal;
use ratatui::backend::Backend;
use ratatui::layout::Position;
use ratatui::layout::Rect;
use ratatui::widgets::Block;

mod commands;
mod history;

use crate::Command;
use crate::cli::commands::CommandProcessor;
use crate::cli::history::CliHistory;

/// The text displayed at the start of the shell command input.
static PROMPT: &str = "> ";

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
        self.history.push_command(data);
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
                    self.history.push_command(s);
                }
                self.history_index = 0;
                match command {
                    Ok(cmd) => Some(cmd),
                    Err(err) => {
                        self.history.push_command_output(err);
                        Some(Command::Redraw)
                    }
                }
            }
        }
    }

    pub fn render(&self, frame: &mut Frame, rect: Rect) {
        let block = Block::bordered()
            .title(" CLI ")
            .title_alignment(ratatui::layout::Alignment::Center);
        let inner_rect = block.inner(rect);
        let y = inner_rect.y;
        let (display_length, para) = self.history.render(inner_rect);
        frame.render_widget(para.block(block), rect);
        let pos = Position::new(inner_rect.x, y + display_length);
        frame.set_cursor_position(pos);
        self.processor.set_cursor_position(pos);
    }
}
