use std::cell::Cell;
use std::io::Write as _;
use std::sync::mpsc;
use std::time::Duration;

use clap::Parser;
use crossterm::event::Event;
use crossterm::event::KeyCode;
use crossterm::event::KeyEventKind;
use crossterm::event::KeyModifiers;
use crossterm::event::poll;
use crossterm::event::read;
use ratatui::Terminal;
use ratatui::layout::Position;
use ratatui::prelude::Backend;

use crate::Command;
use crate::repl::PROMPT;
use crate::command::ReplCommand;

/// Create a thread to poll for user inputs and forward them to the main thread.
/// Originally based off of [bottom's implementation](https://github.com/ClementTsang/bottom/blob/master/src/main.rs).
fn process_keyboard_input(sender: mpsc::Sender<Event>) {
    loop {
        // TODO: btm's impl used a cancellation token for a gentle shutdown. The same should
        // be done here... eventually.
        if let Ok(true) = poll(Duration::from_millis(20))
            && let Ok(event) = read()
        {
            // NOTE: We unwrap because the state shouldn't be holding too many
            // messages. Perhaps an unbounded channel would be better...
            sender.send(event).unwrap();
        }
    }
}

#[derive(Debug)]
pub struct CommandProcessor {
    inbound: mpsc::Receiver<Event>,
    index: usize,
    buffer: String,
    /// Tracks where the user input is at. This used to maintain the input data.
    cursor_position: Cell<Position>,
    /// This tracks the longest input string "Enter" or "CTRL+C" was pressed. This is used to
    /// "wipe" the input line of any prior input.
    ///
    /// Ex: If the current input is "foo" and then the intput is set to an empty string (perhaps
    /// because of a down arrow), the text in the input line needs to appear empty. Similarly, if
    /// the input text is "foobar" and gets set to "bar", not "wiping" the line would result in the
    /// input line appearing to contain "barbar".
    longest_last_input: u16,
}

impl CommandProcessor {
    pub fn new() -> Self {
        let (send, recv) = mpsc::channel();
        std::thread::spawn(move || process_keyboard_input(send));
        Self {
            inbound: recv,
            index: 0,
            buffer: String::new(),
            cursor_position: Cell::default(),
            longest_last_input: 0,
        }
    }

    pub fn set_input(&mut self, s: String) {
        self.update_buffer(|buffer| *buffer = s)
    }

    /// (Re)Draws the line that users input. This allows the input to be rendered without needing
    /// to rerender the entire TUI.
    pub fn draw_input_line<B: Backend>(&self, term: &mut Terminal<B>) {
        let mut pos = self.cursor_position.get();
        _ = term.set_cursor_position(pos);
        let mut stdout = std::io::stdout();
        let input = PROMPT
            .chars()
            .chain(
                self.buffer
                    .chars()
                    .chain(std::iter::repeat(' '))
                    .take(self.longest_last_input as usize),
            )
            .collect::<String>();
        write!(stdout, "{input}").unwrap();
        stdout.flush().unwrap();
        pos.x += (PROMPT.len() + self.buffer.len()) as u16;
        _ = term.set_cursor_position(pos);
    }

    pub fn set_cursor_position(&self, pos: Position) {
        self.cursor_position.set(pos);
    }

    fn update_buffer<U, F>(&mut self, f: F) -> U
    where
        U: 'static,
        F: FnOnce(&mut String) -> U,
    {
        let digest = f(&mut self.buffer);
        self.update_longest_input();
        digest
    }

    fn update_longest_input(&mut self) {
        let len = self.buffer.len() as u16;
        self.longest_last_input = std::cmp::max(len, self.longest_last_input);
    }

    pub fn next_event(&mut self) -> Option<CliEvent> {
        match self.inbound.recv().unwrap() {
            Event::Resize(_, _) => Some(CliEvent::Resize),
            Event::Paste(data) => {
                self.update_buffer(|buffer| buffer.push_str(&data));
                None
            }
            Event::Key(key) if matches!(key.kind, KeyEventKind::Press) => match key.code {
                KeyCode::Backspace => {
                    self.buffer.pop();
                    self.update_longest_input();
                    None
                }
                KeyCode::Enter => {
                    let cmd = ReplCommand::try_parse_from(self.buffer.split_whitespace());
                    self.index = 0;
                    let output = self.update_buffer(std::mem::take);
                    Some(CliEvent::Command(
                        output,
                        cmd.map(|cmd| cmd.command).map_err(|err| err.to_string()),
                    ))
                }
                KeyCode::Up => Some(CliEvent::Up),
                KeyCode::Down => Some(CliEvent::Down),
                KeyCode::Char(c) => {
                    if key.modifiers.contains(KeyModifiers::CONTROL) {
                        if c == 'c' {
                            return Some(CliEvent::Cancel(self.update_buffer(std::mem::take)));
                        } else if c == 'd' {
                            return Some(CliEvent::Command(String::new(), Ok(Command::Exit)));
                        }
                    }
                    self.buffer.push(c);
                    self.update_longest_input();
                    None
                }
                // Not used (yet)
                _ => None,
            },
            Event::Key(_) => None,
            Event::Mouse(_) => None,
            Event::FocusGained => None,
            Event::FocusLost => None,
        }
    }
}

pub enum CliEvent {
    Up,
    Down,
    Cancel(String),
    Resize,
    Command(String, Result<Command, String>),
}

impl Default for CommandProcessor {
    fn default() -> Self {
        Self::new()
    }
}
