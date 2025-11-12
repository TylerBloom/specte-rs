//! Defines the commands that can be ran in the TUI's CLI.

use std::num::ParseIntError;
use std::path::PathBuf;

use clap::Parser;
use clap::Subcommand;
use indexmap::IndexSet;

use serde::Deserialize;
use serde::Serialize;
use spirit::ppu::Pixel;

#[derive(Parser)]
#[command(multicall = true)]
pub struct ReplCommand {
    #[command(subcommand)]
    pub command: Command,
}

// TODO: Add a commands:
//  - to set verbosity level
#[derive(Debug, Clone, Subcommand, Serialize, Deserialize)]
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
    #[command(subcommand)]
    Breakpoint(BreakpointCommand),
}

#[derive(Debug, Clone, Copy, Subcommand, Serialize, Deserialize)]
pub enum ViewCommand {
    /// This command controls what the PC area looks like. The `start` argument controls how many
    /// commands are shown before the PC's location.
    PC { start: usize },
}

#[derive(Debug, Clone, Copy, Subcommand, Serialize, Deserialize)]
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

#[derive(Debug, Clone, Copy, Subcommand, Serialize, Deserialize)]
pub enum BreakpointCommand {
    List,
    Add {
        #[arg(value_parser = parse_int)]
        pc: Option<u16>,
    },
    /// Ignores a breakpoint for the rest of the run. This is not save between runs.
    Skip {
        #[arg(value_parser = parse_int)]
        pc: u16,
    },
    Remove {
        #[arg(value_parser = parse_int)]
        pc: Option<u16>,
    },
}

#[derive(Debug, Clone, Copy, Subcommand, Serialize, Deserialize)]
pub enum RunLength {
    #[command(subcommand)]
    For(RunFor),
    #[command(subcommand)]
    Until(RunUntil),
}

#[derive(Debug, Clone, Copy, Subcommand, Serialize, Deserialize)]
pub enum RunFor {
    Frames { count: usize },
}

#[derive(Debug, Clone, Copy, Subcommand, Serialize, Deserialize)]
pub enum RunUntil {
    /// Runs the emulator until an infinite loop is detected.
    #[command(subcommand)]
    Loop(LoopKind),
    /// Runs the emulator until just after a call is returned from
    Return,
    /// Runs the emulator until the next frame is ready
    Frame,
    /// Runs the emulator until it receives a pause command
    Pause,
    /// Run the emulator until an interupt is requested.
    Interupt,
    /// Run the emulator until the whatever handwritten  condition is met.
    Custom,
    /// A "loop" here refers to any (conditional) relative jump instruction where the relative jump
    /// is negative. The "count" argument refers to the number of detected loops that will be
    /// passed. For example, if there are two nested loops, the emulator will be run until
    /// immediately after the outer loop is finished. Similarly for two subsequent loops.
    // FIXME: There are lots of edge cases here. The major ones are any kind of jumps that move us
    // outside of the "loop". This includes literal jumps, returns, and even interrupts.
    PassedLoop { count: Option<usize> },
}

#[derive(Debug, Clone, Copy, Subcommand, Serialize, Deserialize)]
pub enum LoopKind {
    /// Run instructions until repeat CPU + memory states are seen.
    CpuAndMem,
    /// Run instructions until repeat screens are seen.
    Screen,
}

#[derive(Debug, Clone, Subcommand, Serialize, Deserialize)]
pub enum StashOptions {
    /// Save a snapshot of the current state. If no string is provided, it is given a number equal
    /// to the number of saved snapshots.
    Save {
        file: String,
    },
    /// Take a snapshot of a saved state and make it the current state.
    Load {
        file: String,
    },
    Remove {
        file: String,
    },
    /// Save a snapshot to file. If the name of a state is not provided, the current state is
    /// exported with an assumed name.
    Export {
        state: Option<String>,
        file: PathBuf,
    },
    /// Load a state from a file and make it the current state.
    LoadFrom {
        file: PathBuf,
    },
}

#[derive(Debug, Clone, Copy, Subcommand, Serialize, Deserialize)]
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
