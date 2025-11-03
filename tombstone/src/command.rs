//! Defines the commands that can be ran in the TUI's CLI.

use std::num::ParseIntError;
use std::path::PathBuf;

use clap::Parser;
use clap::Subcommand;
use indexmap::IndexSet;

use spirit::ppu::Pixel;

#[derive(Parser)]
#[command(multicall = true)]
pub struct ReplCommand {
    #[command(subcommand)]
    pub command: Command,
}

// TODO: Add a commands:
//  - help
//  - something to visualize the surrounding ops
//  - to set verbosity level
// Index should be formatted more nicely, like hexdump
#[derive(Subcommand)]
pub enum Command {
    /// This is a no-op command as it doesn't change the state but it re-renders the TUI. This is
    /// only triggered if crossterm detects that the terminal has changed sized.
    Redraw,
    /// Exits the debugger
    Exit,
    Step {
        count: usize,
    },
    Info,
    /// Prints a section of the MemoryMap.
    #[command(subcommand)]
    Index(IndexOptions),
    /// Runs the emulator until some condition is met.
    #[command(subcommand)]
    Run(RunLength),
    /// Runs the emulator until some condition is met.
    #[command(subcommand)]
    Interrupt(Interrupt),
    #[command(subcommand)]
    Stash(StashOptions),
    Pause,
    Read {
        #[arg(value_parser = parse_int)]
        index: u16,
    },
    #[command(subcommand)]
    View(ViewCommand),
}

#[derive(Subcommand)]
pub enum ViewCommand {
    /// This command controls what the PC area looks like. The `start` argument controls how many
    /// commands are shown before the PC's location.
    PC { start: usize },
}

#[derive(Subcommand, Clone, Copy)]
pub enum IndexOptions {
    Single {
        #[arg(value_parser = parse_int)]
        addr: u16,
    },
    Range {
        start: usize,
        end: usize,
    },
}

#[derive(Subcommand, Clone, Copy)]
pub enum RunLength {
    #[command(subcommand)]
    For(RunFor),
    #[command(subcommand)]
    Until(RunUntil),
}

#[derive(Subcommand, Clone, Copy)]
pub enum RunFor {
    Frames { count: usize },
}

#[derive(Subcommand, Clone, Copy)]
pub enum RunUntil {
    /// Runs the emulator until an infinite loop is detected.
    #[command(subcommand)]
    Loop(LoopKind),
    /// Runs the emulator until just before `ret` is ran
    Return,
    /// Runs the emulator until the next frame is ready
    Frame,
    /// Runs the emulator until it receives a pause command
    Pause,
    /// Run the emulator until an interupt is requested.
    Interupt,
    /// Run the emulator until the whatever handwritten  condition is met.
    Custom,
}

#[derive(Subcommand, Clone, Copy)]
pub enum LoopKind {
    /// Run instructions until repeat CPU + memory states are seen.
    CpuAndMem,
    /// Run instructions until repeat screens are seen.
    Screen,
}

#[derive(Subcommand, Clone)]
pub enum StashOptions {
    /// Save a snapshot of the current state. If no string is provided, it is given a number equal
    /// to the number of saved snapshots.
    Save { file: Option<String> },
    /// Take a snapshot of a saved state and make it the current state.
    Load { file: String },
    /// Save a snapshot to file. If the name of a state is not provided, the current state is
    /// exported with an assumed name.
    Export {
        state: Option<String>,
        file: PathBuf,
    },
    /// Removes a snapshot held in memory.
    Remove { state: Option<String> },
    /// Load a state from a file and make it the current state.
    LoadFrom { file: PathBuf },
}

#[derive(Subcommand, Clone, Copy)]
pub enum Interrupt {
    VBlank,
}

#[derive(Debug, Clone)]
pub enum WindowMessage {
    DuplicateScreens(IndexSet<Vec<Vec<Pixel>>>),
    Frames(usize),
    Run,
    Pause,
    Redraw,
}

fn parse_int(input: &str) -> Result<u16, ParseIntError> {
    if let Some(input) = input.strip_prefix("0x") {
        u16::from_str_radix(input, 16)
    } else if let Some(input) = input.strip_prefix("0b") {
        u16::from_str_radix(input, 2)
    } else {
        input.parse()
    }
}

#[cfg(test)]
mod tests {
    use clap::Parser;

    use std::error::Error;

    use super::ReplCommand;

    #[test]
    fn parse_commands() -> Result<(), Box<dyn Error>> {
        ReplCommand::try_parse_from(std::iter::once("info"))?;
        ReplCommand::try_parse_from("step 10".split_whitespace())?;
        assert!(ReplCommand::try_parse_from(std::iter::once("step")).is_err());
        Ok(())
    }
}
