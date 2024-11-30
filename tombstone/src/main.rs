#![allow(dead_code, unused)]

use std::fmt::Write;
use std::panic::PanicHookInfo;
use std::sync::{Arc, Mutex};
use std::time::{Duration, Instant};
use std::{error::Error, io::Write as _};

use crossterm::event::{poll, read, Event};
use crossterm::execute;
use crossterm::style::Print;
use crossterm::terminal::{
    disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen,
};
use ghast::state::Emulator;
use ratatui::layout::Position;
use ratatui::{
    backend::{Backend, CrosstermBackend},
    layout::{Constraint, Direction, Layout, Rect},
    text::Text,
    widgets::{Block, Paragraph},
    Frame, Terminal,
};
use spirit::cpu::CpuState;
use spirit::{cpu::Cpu, lookup::Instruction, mem::MemoryMap, ppu::Ppu, Gameboy, StartUpSequence};

use clap::Parser;

pub mod command;
mod repl;
mod state;
mod window;
use command::*;
use state::*;
use std::sync::mpsc;
use tokio::sync::broadcast;
use window::WindowState;

static TMP_ROM: &[u8] = include_bytes!("../../spirit/tests/roms/acid/which.gb");

#[derive(Debug, Parser)]
struct Args {
    path: String,
}

fn main() {
    let Args { path } = Args::parse();
    let rom = std::fs::read(path).expect("Unknown file");
    std::panic::set_hook(Box::new(panic_hook));
    execute!(std::io::stdout(), EnterAlternateScreen).unwrap();
    enable_raw_mode().unwrap();
    let backend = CrosstermBackend::new(std::io::stdout());
    let mut term = Terminal::new(backend).unwrap();
    term.clear().unwrap();
    let mut gb = Arc::new(Mutex::new(Emulator::new(rom)));
    let (send_cmd, recv_cmd) = broadcast::channel(100);
    let (send_event, recv_event) = mpsc::channel();
    create_input_thread(send_event);
    let mut state = AppState::new(gb.clone(), recv_event, send_cmd);
    state.mem_start = 0x8000;
    let handle = std::thread::spawn(move || state.run(term));
    WindowState::new(gb, recv_cmd).run();
    handle.join();
    execute!(std::io::stdout(), LeaveAlternateScreen).unwrap();
}

/// Create a thread to poll for user inputs and forward them to the main thread.
/// Originally based off of [bottom's implementation](https://github.com/ClementTsang/bottom/blob/master/src/main.rs).
fn create_input_thread(sender: mpsc::Sender<Event>) {
    std::thread::spawn(move || {
        loop {
            // TODO: Bottom's impl used a cancellation token for a gentle shutdown. The same should
            // be done here... eventually.
            if let Ok(true) = poll(Duration::from_millis(20)) {
                if let Ok(event) = read() {
                    // NOTE: We unwrap because the state shouldn't be holding too many
                    // messages. Perhaps an unbounded channel would be better...
                    sender.send(event).unwrap();
                }
            }
        }
    });
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

    // TODO: Might be cleaner in the future to use a cancellation token, but that causes some fun
    // issues with lifetimes; for now if it panics then shut down the main program entirely ASAP.
    std::process::exit(1);
}
