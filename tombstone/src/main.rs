#![allow(dead_code)]
use std::{io::Write, ops::Range, str::FromStr};

use spirit::{Gameboy, StartUpSequence};

static TMP_ROM: &[u8] = include_bytes!("../../spirit/tests/roms/acid/which.gb");

fn main() {
    let mut gb = Gameboy::new(TMP_ROM);
    process_init(gb.start_up());
    loop {
        todo!()
    }
}

fn process_init(mut seq: StartUpSequence<'_>) {
    while !seq.is_complete() {
        print!("init $ ");
        std::io::stdout().flush().unwrap();
        match get_input() {
            Some(Command::Info) => todo!(),
            Some(Command::Step(n)) => {
                for _ in 0..n {
                    seq.step();
                    if seq.is_complete() {
                        break;
                    }
                }
            }
            Some(Command::Index(_)) => todo!(),
            Some(Command::Run(_until)) => {
                todo!()
            }
            None => println!("unknown command!"),
        }
    }
}

enum Command {
    /// Steps the emulator forward N operations.
    Step(usize),
    /// Prints the current state of the CPU.
    Info,
    /// Prints a section of the MemoryMap.
    Index(IndexOptions),
    /// Runs the emulator until some condition is met.
    Run(RunUntil),
}

enum IndexOptions {
    Single(u16),
    Range(Range<u16>),
}

/// Encodes the conditions used by the run command
enum RunUntil {
    /// Runs the emulator until an infinite loop is detected.
    Loop,
    /// Runs the emulator until just before `ret` is ran
    Return,
}

fn get_input() -> Option<Command> {
    let mut input = String::new();
    std::io::stdin().read_line(&mut input).ok()?;
    input.trim().parse().ok()
}

impl FromStr for Command {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        println!("Got input: {s:?}");
        if let Some(s) = s.strip_prefix("step") {
            s.trim().parse().map(Self::Step).map_err(drop)
        } else if s.strip_prefix("info").is_some() {
            Ok(Self::Info)
        } else if let Some(s) = s.strip_prefix("index") {
            s.trim().parse().map(Self::Index)
        } else if let Some(s) = s.strip_prefix("run") {
            s.trim().parse().map(Self::Run)
        } else {
            Err(())
        }
    }
}

impl FromStr for IndexOptions {
    type Err = ();

    fn from_str(_s: &str) -> Result<Self, Self::Err> {
        todo!()
    }
}

impl FromStr for RunUntil {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "loop" => Ok(Self::Loop),
            "return" => Ok(Self::Return),
            _ => Err(()),
        }
    }
}
