#![allow(dead_code)]
use std::io::Write;

use spirit::{lookup::Instruction, Gameboy, StartUpSequence};

mod command;
use command::*;

// TODO: handle ctrl-C and ctrl-d

static TMP_ROM: &[u8] = include_bytes!("../../spirit/tests/roms/acid/which.gb");

fn main() {
    let mut gb = Gameboy::new(TMP_ROM);
    run_until_complete(gb.start_up());
    println!("Startup sequence complete!");
    run_until_complete(gb)
}

/// This function abstracts over running the startup sequence and the gameboy itself.
fn run_until_complete<GB>(mut gb: GB)
where
    GB: GameBoyLike,
{
    while !gb.is_complete() {
        print!("{} $ ", GB::PROMPT);
        std::io::stdout().flush().unwrap();
        match get_input() {
            Ok(cmd) => cmd.process(&mut gb),
            Err(e) => println!("{e}"),
        }
    }
}

trait GameBoyLike {
    const PROMPT: &'static str;

    fn gb(&self) -> &Gameboy;

    fn next_op(&self) -> Instruction;

    fn step(&mut self);

    fn is_complete(&self) -> bool;
}

impl GameBoyLike for Gameboy {
    const PROMPT: &'static str = "running";

    fn gb(&self) -> &Gameboy {
        self
    }

    fn next_op(&self) -> Instruction {
        self.cpu().read_op(&self.mem)
    }

    fn step(&mut self) {
        self.step().complete()
    }

    fn is_complete(&self) -> bool {
        self.cpu().done
    }
}

impl<'a> GameBoyLike for StartUpSequence<'a> {
    const PROMPT: &'static str = "init";

    fn gb(&self) -> &Gameboy {
        self.gb()
    }

    fn next_op(&self) -> Instruction {
        *self.next_op()
    }

    fn step(&mut self) {
        self.step()
    }

    fn is_complete(&self) -> bool {
        self.is_complete()
    }
}
