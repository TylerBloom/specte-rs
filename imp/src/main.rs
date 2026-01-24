use std::path::PathBuf;
use std::str::FromStr;

use clap::Parser;
use rboy::cpu::Action;
use rboy::device::Device;
use spirit::{instruction::Instruction, lookup, Gameboy};

#[derive(Debug, Parser)]
#[command(version, about)]
struct Config {
    /// The path to the ROM to run.
    rom: String,
}

fn main() {
    let args = Config::parse();
    let path = PathBuf::from_str(&args.rom).unwrap();
    let rom = std::fs::read(&path).unwrap();
    let mut gb = Gameboy::load_cartridge(rom.clone()).complete();
    println!("[spirit] Constructed GB");
    let mut other_gb = Device::new_cgb_from_buffer(rom, false, None).unwrap();
    println!("[  rboy] Constructed GB");
    while !gb.is_stopped() {
        let op = gb.read_op();
        gb.step();
        print!("[spirit] ");
        match op {
            Instruction::Stall => println!("Halted"),
            Instruction::Stopped => println!("Stopped"),
            Instruction::Prefixed => println!("Executed prefixed op"),
            Instruction::Unused => todo!(),
            Instruction::Transfer => {}
            op => println!(
                "Executing op: 0x{:0>2X}",
                lookup::OP_LOOKUP
                    .iter()
                    .enumerate()
                    .find_map(|(i, o)| (&op == o).then_some(i))
                    .unwrap()
            ),
        }
        let (_, action) = other_gb.do_cycle();
        print!("[  rboy] ");
        match action {
            Action::ExecutedOp(opcode) => print!("Executed op: 0x{opcode:0>2X}"),
            Action::ExecutedPrefixedOp(opcode) => {
                print!("Executed prefixed op: 0x{opcode:0>2X}")
            }
            Action::VBlank => print!("VBLank interrupt"),
            Action::Lcd => print!("LDC interrupt"),
            Action::Timer => print!("Timer interrupt"),
            Action::Serial => print!("Serial interrupt"),
            Action::Joypad => print!("Joypad interrupt"),
            Action::Halted => print!("Halted"),
            Action::Stopped => print!("Stopped"),
        }
    }
    println!("Spirit stopped");
}
