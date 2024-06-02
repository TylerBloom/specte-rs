#![allow(dead_code)]
use std::{
    collections::HashMap,
    hash::{DefaultHasher, Hash, Hasher},
    io::Write,
    ops::Range,
    str::FromStr,
};

use spirit::{
    lookup::{Instruction, JumpOp},
    mbc::MemoryMap,
    Gameboy, StartUpSequence,
};

// TODO: handle ctrl-C and ctrl-d

static TMP_ROM: &[u8] = include_bytes!("../../spirit/tests/roms/acid/which.gb");

fn main() {
    let mut gb = Gameboy::new(TMP_ROM);
    let mut header = Vec::new();
    for i in 0x100..=0x14F {
        header.push(gb.mem[i]);
    }
    gb.mem = MemoryMap::construct();
    let rom = gb.mem.rom_mut();
    for (i, byte) in header.into_iter().enumerate() {
        rom[i + 0x100] = byte;
    }
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
            Some(Command::Info) => println!("{:?}", seq.gb().cpu()),
            Some(Command::Step(n)) => {
                for _ in 0..n {
                    seq.step();
                    if seq.is_complete() {
                        break;
                    }
                }
            }
            Some(Command::Index(opts)) => match opts {
                IndexOptions::Single(i) => println!("ADDR=0x{i:0>4X} -> {:0>2X}", seq.gb().mem[i]),
                IndexOptions::Range(mut rng) => {
                    println!("ADDR=0x{:0>4X}..0x{:0>4X}", rng.start, rng.end);
                    print!("[");
                    if let Some(i) = rng.next() {
                        print!("0x{:0>2}", seq.gb().mem[i]);
                    }
                    for i in rng {
                        print!(", 0x{:0>2}", seq.gb().mem[i]);
                    }
                    println!("]");
                }
            },
            Some(Command::Run(until)) => match until {
                RunUntil::Loop => {
                    let mut ops = Vec::new();
                    let mut cpu_map: HashMap<_, HashMap<u64, usize>> = HashMap::new();
                    let index;
                    loop {
                        match cpu_map.entry(seq.gb().cpu().clone()) {
                            std::collections::hash_map::Entry::Occupied(mut entry) => {
                                let mut hasher = DefaultHasher::new();
                                seq.gb().mem.hash(&mut hasher);
                                match entry.get_mut().entry(hasher.finish()) {
                                    std::collections::hash_map::Entry::Vacant(entry) => {
                                        entry.insert(ops.len());
                                    }
                                    std::collections::hash_map::Entry::Occupied(entry) => {
                                        index = *entry.get();
                                        break;
                                    }
                                }
                            }
                            std::collections::hash_map::Entry::Vacant(entry) => {
                                entry.insert(HashMap::new());
                            }
                        }
                        let cpu = seq.gb().cpu().clone();
                        let ptr = cpu.pc.0;
                        ops.push((cpu, ptr, *seq.next_op()));
                        seq.step()
                    }
                    println!("Infinite loop detected! Here are the instructions and CPU states in the loop:");
                    for (state, ptr, op) in &ops[index..] {
                        println!("{state}");
                        println!("0x{ptr:0>4X} -> {op}");
                    }
                }
                RunUntil::Return => {
                    while !matches!(seq.next_op(), Instruction::Jump(JumpOp::Return)) {
                        seq.step()
                    }
                    println!("End of subroutine. Next operation is `ret`.");
                }
            },
            None => println!("unknown command!"),
        }
    }
}

enum Command {
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

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        fn parse_int(s: &str) -> Result<u16, ()> {
            match s.parse() {
                Ok(val) => Ok(val),
                Err(_) => {
                    if let Some(s) = s.strip_prefix("0x") {
                        u16::from_str_radix(s, 16).map_err(drop)
                    } else if let Some(s) = s.strip_prefix("0b") {
                        u16::from_str_radix(s, 2).map_err(drop)
                    } else {
                        Err(())
                    }
                }
            }
        }
        match s.split_once("..") {
            Some((start, end)) => {
                let start = parse_int(start)?;
                let end = parse_int(end)?;
                Ok(Self::Range(start..end))
            }
            None => parse_int(s).map(Self::Single),
        }
    }
}

impl FromStr for RunUntil {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "until loop" => Ok(Self::Loop),
            "until return" => Ok(Self::Return),
            _ => Err(()),
        }
    }
}
