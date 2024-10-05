use std::{
    borrow::Cow,
    collections::{hash_map::Entry, HashMap},
    fmt::Write,
    hash::{DefaultHasher, Hash, Hasher},
    num::ParseIntError,
    ops::Range,
    path::PathBuf,
    str::FromStr,
};

use clap::{Args, Parser, Subcommand};

use spirit::lookup::{Instruction, JumpOp};

use crate::AppState;

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
    Run(RunUntil),
    /// Runs the emulator until some condition is met.
    #[command(subcommand)]
    Interrupt(Interrupt),
    #[command(subcommand)]
    Stash(StashOptions),
}

#[derive(Subcommand, Clone, Copy)]
pub enum IndexOptions {
    Single {
        addr: usize,
    },
    Range {
        start: usize,
        end: usize,
    },
}

#[derive(Subcommand, Clone, Copy)]
pub enum RunUntil {
    /// Runs the emulator until an infinite loop is detected.
    Loop,
    /// Runs the emulator until just before `ret` is ran
    Return,
    /// Runs the emulator until just before `ret` is ran
    Frame,
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
