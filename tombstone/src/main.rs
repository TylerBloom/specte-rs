use std::panic::PanicHookInfo;

use clap::Parser;
use crossterm::execute;
use crossterm::style::Print;
use crossterm::terminal::EnterAlternateScreen;
use crossterm::terminal::LeaveAlternateScreen;
use crossterm::terminal::disable_raw_mode;
use crossterm::terminal::enable_raw_mode;
use ratatui::Terminal;
use ratatui::backend::CrosstermBackend;

pub mod cli;
pub mod command;
pub mod config;
pub mod display_windows;
pub mod pc_state;
pub mod state;

use command::*;
use state::*;

#[derive(Debug, Parser)]
struct Args {
    path: String,
}

fn main() {
    let Args { path } = Args::parse();
    let rom = std::fs::read(path).expect("Unknown file");

    // Setup a clean base for the TUI
    std::panic::set_hook(Box::new(panic_hook));
    execute!(std::io::stdout(), EnterAlternateScreen).unwrap();
    enable_raw_mode().unwrap();
    let backend = CrosstermBackend::new(std::io::stdout());
    let mut term = Terminal::new(backend).unwrap();
    term.clear().unwrap();

    // Construct the emulator state
    let state = AppState::new(rom);

    // Run the TUI's state until it returns
    eprintln!("Running...");
    state.run(term);

    // Wait for the running thread to close, then reset the terminal and return
    execute!(std::io::stdout(), LeaveAlternateScreen).unwrap();
}

/// A panic hook to properly restore the terminal in the case of a panic. Originally based on
/// [spotify-tui's implementation](https://github.com/Rigellute/spotify-tui/blob/master/src/main.rs).
fn panic_hook(panic_info: &PanicHookInfo<'_>) {
    let mut stdout = std::io::stdout();

    let msg = match panic_info.payload().downcast_ref::<&'static str>() {
        Some(s) => *s,
        None => match panic_info.payload().downcast_ref::<String>() {
            Some(s) => &s[..],
            None => "Box<Any>",
        },
    };

    let _ = disable_raw_mode();
    let _ = execute!(stdout, LeaveAlternateScreen);

    // Print stack trace. Must be done after!
    if let Some(panic_info) = panic_info.location() {
        let _ = execute!(
            stdout,
            Print(format!(
                "thread '<unnamed>' panicked at '{msg}', {panic_info}"
            )),
        );
    }
}
