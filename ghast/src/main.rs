use ghast::state::Emulator;

use clap::Parser;
use iced::Task;

#[derive(Debug, Parser)]
struct Args {
    path: String,
}

pub fn main() -> iced::Result {
    let Args { path } = Args::parse();
    let rom = std::fs::read(path).expect("Unknown file");
    iced::application("Specters - Ghast GBC", Emulator::update, Emulator::view)
        .subscription(Emulator::subscription)
        .run_with(|| (Emulator::new(rom), Task::none()))
}
