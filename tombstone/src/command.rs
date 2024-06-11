use std::{
    borrow::Cow, collections::{hash_map::Entry, HashMap}, fmt::Write, hash::{DefaultHasher, Hash, Hasher}, ops::Range, path::PathBuf, str::FromStr
};

use spirit::lookup::{Instruction, JumpOp};

use crate::{AppState, GameBoyLike};
use untwine::ParserError;

untwine::parser! {
    [error = ParserError]
    ws = #{char::is_ascii_whitespace}+;
    number: <"0x" | "0b" | ""> rest=.* -> usize { todo!() }
    range: start=number ".." end=number -> Range<usize> { start..end }
    step: "step" ws count=number -> Command { Command::Step(count) }
    info: "info" -> Command { Command::Info }
    index: "index" -> Command { Command::Index(todo!()) }
    run: "run" -> Command { Command::Run(todo!()) }
    interrupt: "interrupt" -> Command { Command::Interrupt(todo!()) }
    stash: "stash" -> Command { Command::Stash(todo!()) }
    help: "help" -> Command { todo!() }
    exit: "exit" -> Command { todo!() }
    pub command = (step | info | index | run | interrupt | stash | help | exit) -> Command;
}

pub(crate) fn get_input(state: &mut AppState) -> Result<Command, Cow<'static, str>> {
    let mut input = String::new();
    std::io::stdin().read_line(&mut input).unwrap();
    let cmd = input.trim().parse();
    state.cli_history.push(input);
    cmd
}

pub(crate) enum Command {
    // TODO: Add a commands:
    //  - help
    //  - something to visualize the surrounding ops
    //  - exit
    // Index should be formatted more nicely, like hexdump
    /// Steps the emulator forward N operations.
    Step(usize),
    /// Prints the current state of the CPU.
    Info,
    /// Prints a section of the MemoryMap.
    Index(IndexOptions),
    /// Runs the emulator until some condition is met.
    Run(RunUntil),
    /// Runs the emulator until some condition is met.
    Interrupt(Interrupt),
    /// Commands that work with copies of the current state of the emulator.
    Stash(StashOptions),
}

impl Command {
    pub(crate) fn process<GB: GameBoyLike>(self, gb: &mut GB) -> String {
        match self {
            Command::Info => gb.gb().cpu().to_string(),
            Command::Step(n) => {
                (0..n).any(|_| {
                    gb.step();
                    gb.is_complete()
                });
                String::new()
            }
            Command::Index(opts) => match opts {
                IndexOptions::Single(i) => format!("ADDR=0x{i:0>4X} -> {:0>2X}", gb.gb().mem[i]),
                IndexOptions::Range(mut rng) => {
                    let mut digest = format!("ADDR=0x{:0>4X}..0x{:0>4X}", rng.start, rng.end);
                    digest.push('[');
                    if let Some(i) = rng.next() {
                        write!(digest, "0x{:0>2}", gb.gb().mem[i]).unwrap();
                    }
                    for i in rng {
                        write!(digest, ", 0x{:0>2}", gb.gb().mem[i]);
                    }
                    digest.push(']');
                    digest
                }
            },
            Command::Run(until) => match until {
                RunUntil::Loop => {
                    let mut ops = Vec::new();
                    let mut cpu_map: HashMap<_, HashMap<u64, HashMap<u64, usize>>> = HashMap::new();
                    let mut index = 0;
                    while !gb.is_complete() {
                        let cpu = gb.gb().cpu().clone();
                        let ptr = cpu.pc.0;
                        ops.push((cpu, ptr, gb.next_op()));
                        match cpu_map.entry(gb.gb().cpu().clone()) {
                            Entry::Occupied(mut entry) => {
                                let mut hasher = DefaultHasher::new();
                                gb.gb().ppu.hash(&mut hasher);
                                match entry.get_mut().entry(hasher.finish()) {
                                    Entry::Vacant(entry) => {
                                        entry.insert(HashMap::new());
                                    }
                                    Entry::Occupied(mut entry) => {
                                        let mut hasher = DefaultHasher::new();
                                        gb.gb().mem.hash(&mut hasher);
                                        match entry.get_mut().entry(hasher.finish()) {
                                            Entry::Vacant(entry) => {
                                                entry.insert(ops.len());
                                            },
                                            Entry::Occupied(entry) => {
                                                index = *entry.get();
                                                break;
                                            }
                                        }
                                    }
                                }
                            }
                            Entry::Vacant(entry) => {
                                entry.insert(HashMap::new());
                            }
                        }
                        gb.step()
                    }
                    if !gb.is_complete() {
                        let mut digest = "Infinite loop detected! Here are the instructions and CPU states in the loop:".to_string();
                        for (state, ptr, op) in &ops[index..] {
                            writeln!(digest, "\n{state}");
                            write!(digest, "0x{ptr:0>4X} -> {op}");
                        }
                        digest
                    } else {
                        "No loop detect during start up!".to_owned()
                    }
                }
                RunUntil::Return => {
                    while !matches!(gb.next_op(), Instruction::Jump(JumpOp::Return)) {
                        gb.step()
                    }
                    "End of subroutine. Next operation is `ret`.".to_owned()
                }
            },
            Command::Stash(_stash) => {
                todo!()
            }
            Command::Interrupt(_interrupt) => todo!(),
        }
    }
}

pub(crate) enum StashOptions {
    /// Save a snapshot of the current state. If no string is provided, it is given a number equal
    /// to the number of saved snapshots.
    Save(Option<String>),
    /// Take a snapshot of a saved state and make it the current state.
    Load(String),
    /// Save a snapshot to file. If the name of a state is not provided, the current state is
    /// exported with an assumed name.
    Export(PathBuf, Option<String>),
    /// Removes a snapshot held in memory.
    Remove(String),
    /// Load a state from a file and make it the current state.
    LoadFrom(PathBuf),
}

pub(crate) enum Interrupt {
    VBlank,
}

pub(crate) enum IndexOptions {
    Single(u16),
    Range(Range<u16>),
}

/// Encodes the conditions used by the run command
pub(crate) enum RunUntil {
    /// Runs the emulator until an infinite loop is detected.
    Loop,
    /// Runs the emulator until just before `ret` is ran
    Return,
}

fn parse_int(s: &str) -> Result<u16, String> {
    let err = || format!("Unable to parse {s:?} into an integer.");
    match s.parse() {
        Ok(val) => Ok(val),
        Err(_) => {
            if let Some(s) = s.strip_prefix("0x") {
                u16::from_str_radix(s, 16).map_err(|_| err())
            } else if let Some(s) = s.strip_prefix("0b") {
                u16::from_str_radix(s, 2).map_err(|_| err())
            } else {
                Err(err())
            }
        }
    }
}

impl FromStr for Command {
    type Err = Cow<'static, str>;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if let Some(s) = s.strip_prefix("step") {
            let i = parse_int(s.trim())? as usize;
            Ok(Self::Step(i))
        } else if s.strip_prefix("info").is_some() {
            Ok(Self::Info)
        } else if let Some(s) = s.strip_prefix("index") {
            s.trim().parse().map(Self::Index)
        } else if let Some(s) = s.strip_prefix("run") {
            s.trim().parse().map(Self::Run)
        } else if let Some(s) = s.strip_prefix("interrupt") {
            s.trim().parse().map(Self::Interrupt)
        } else {
            Err("Unknown command!".into())
        }
    }
}

impl FromStr for IndexOptions {
    type Err = Cow<'static, str>;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.split_once("..") {
            Some((start, end)) => {
                let start = parse_int(start)?;
                let end = parse_int(end)?;
                Ok(Self::Range(start..end))
            }
            None => parse_int(s).map(Self::Single).map_err(Into::into),
        }
    }
}

impl FromStr for RunUntil {
    type Err = Cow<'static, str>;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "until loop" => Ok(Self::Loop),
            "until return" => Ok(Self::Return),
            _ => Err(format!("Unknown argument to `run`: {s:?}").into()),
        }
    }
}

impl FromStr for Interrupt {
    type Err = Cow<'static, str>;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.trim() == "vblank" {
            Ok(Interrupt::VBlank)
        } else {
            Err(format!("Unknown argument to `interrupt`: {s:?}").into())
        }
    }
}
