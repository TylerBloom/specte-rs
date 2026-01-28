use std::fmt::Display;
use std::str::FromStr;
use std::{collections::HashMap, path::PathBuf};

use clap::Parser;
use rboy::cpu::Action;
use rboy::device::Device;
use rboy::mmu::MMU;
use rboy::register::Registers;
use spirit::cpu::Cpu;
use spirit::lookup::{parse_instruction, parse_prefixed_instruction};
use spirit::mem::MemoryMap;
use spirit::{
    instruction::{Instruction, InterruptOp},
    lookup,
    mem::MemoryLike,
    Gameboy,
};

#[derive(Debug, Parser)]
#[command(version, about)]
struct Config {
    /// The path to the ROM to run.
    rom: String,
}

#[derive(Debug, Hash)]
enum Either<L, R> {
    Left(L),
    Right(R),
}

impl<L: Display, R: Display> Display for Either<L, R> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Either::Left(value) => value.fmt(f),
            Either::Right(value) => value.fmt(f),
        }
    }
}

fn main() {
    let args = Config::parse();
    let path = PathBuf::from_str(&args.rom).unwrap();
    let rom = std::fs::read(&path).unwrap();
    let mut gb = Gameboy::load_cartridge(rom.clone()).complete();
    gb.cpu.b.0 = 0;
    println!("[spirit] Constructed GB");
    let mut other_gb = Device::new_cgb_from_buffer(rom, false, None).unwrap();
    other_gb.cpu.mmu.wram = [0; 0x8000];
    println!("[  rboy] Constructed GB");
    if !sweep_mem(&gb.mem, &mut other_gb.cpu.mmu) {
        panic!("Memory maps did not match");
    }

    let init_gb = gb.clone();
    let init_other = other_gb.clone();
    const STEP_SIZE: usize = 10_000;
    let mut counter = 0;
    'outer: loop {
        for i in 0..STEP_SIZE {
            if !step_and_compare(&mut gb, &mut other_gb) {
                counter += bisect_failure(init_gb, init_other, i);
                break 'outer;
            }
        }
        if !extensive_compare(&gb, &mut other_gb) {
            counter += bisect_failure(init_gb, init_other, STEP_SIZE);
            break 'outer;
        }
        counter += STEP_SIZE;
    }
    println!("Error occurred after {counter} steps");
    let mut counter = 0;
    let mut program = HashMap::new();
    while !gb.is_stopped() {
        counter += 1;
        let i = other_gb.cpu.reg.pc;
        // for _ in 0..10 {
        let op = gb.read_op();
        print!("[spirit] 0x{i:0>4X} ");
        let action = match op {
            Instruction::Stall => {
                println!("Halted");
                Action::Halted
            }
            Instruction::Stopped => {
                println!("Stopped");
                Action::Stopped
            }
            Instruction::Prefixed => {
                let pc = gb.cpu().pc.0;
                println!("Looking for prefixed op code @ 0x{pc:0>4X}");
                let op_code = gb.mem.read_byte(pc);
                println!("Executed prefixed op: 0x{op_code:0>2X}");
                Action::ExecutedPrefixedOp(op_code)
            }
            Instruction::Unused => todo!(),
            Instruction::Transfer => todo!(),
            Instruction::Interrupt(InterruptOp::LCD) => {
                println!("VBLank interrupt");
                Action::VBlank
            }
            Instruction::Interrupt(InterruptOp::VBlank) => {
                println!("LDC interrupt");
                Action::VBlank
            }
            Instruction::Interrupt(InterruptOp::Timer) => {
                println!("Timer interrupt");
                Action::Timer
            }
            Instruction::Interrupt(InterruptOp::Serial) => {
                println!("Serial interrupt");
                Action::Serial
            }
            Instruction::Interrupt(InterruptOp::Joypad) => {
                println!("Joypad interrupt");
                Action::Joypad
            }

            op => {
                let opcode = lookup::OP_LOOKUP
                    .iter()
                    .enumerate()
                    .find_map(|(i, o)| (&op == o).then_some(i))
                    .unwrap();
                println!("Executing op: 0x{opcode:0>2X}",);
                Action::ExecutedOp(opcode as u8)
            }
        };
        gb.step();
        let (_, rboy_action) = other_gb.do_cycle();
        print!("[  rboy] 0x{i:0>4X} ");
        match rboy_action {
            Action::ExecutedOp(op_code) => {
                program.insert(i, Either::Left(parse_instruction(op_code)));
                println!("Executed op: 0x{op_code:0>2X}")
            }
            Action::ExecutedPrefixedOp(op_code) => {
                program.insert(i, Either::Right(parse_prefixed_instruction(op_code)));
                println!("Executed prefixed op: 0x{op_code:0>2X}")
            }
            Action::VBlank => println!("VBLank interrupt"),
            Action::Lcd => println!("LDC interrupt"),
            Action::Timer => println!("Timer interrupt"),
            Action::Serial => println!("Serial interrupt"),
            Action::Joypad => println!("Joypad interrupt"),
            Action::Halted => println!("Halted"),
            Action::Stopped => println!("Stopped"),
        }
        if rboy_action != action {
            break;
        }
        if !check_cpu(gb.cpu(), &other_gb.cpu.reg) {
            println!("Cpu mismatch");
            println!("[spirit] {}", gb.cpu());
            print_reg(&other_gb.cpu.reg);
            break;
        }
    }
    let mut program: Vec<_> = program.into_iter().collect();
    program.sort_by_key(|(i, _)| *i);
    program.reverse();
    for (i, op) in program {
        println!("0x{i:0>4X} @ {op}");
    }
    println!("Spirit stopped after {counter} instructions");
}

/// Takes two emulators that are in states known to be matching (i.e. pass an extensive compare),
/// but that are misaligned after the given number of steps, `steps`. Returns the number of steps
/// needed for the first point in time for the misalignment to occur and returns that number.
fn bisect_failure(mut gb: Gameboy, mut other_gb: Device, steps: usize) -> usize {
    let gb_copy = gb.clone();
    let other_gb_copy = other_gb.clone();
    let to = steps / 2;
    for i in 0..to {
        // If step_and_compare fails, the light compare has failed. This indicates an earlier,
        // deeper failure happened. We can take the number of steps that have occurred thus far as
        // the upper bound and bisect from here.
        if !step_and_compare(&mut gb, &mut other_gb) {
            return bisect_failure(gb_copy, other_gb_copy, i);
        }
    }

    if extensive_compare(&gb, &mut other_gb) {
        // They match
        // FIXME: Consider case where steps was odd
        bisect_failure(gb, other_gb, steps / 2)
    } else {
        // They mismatch
        bisect_failure(gb_copy, other_gb_copy, steps / 2)
    }
}

/// Steps both emulators forward a single tick then does a light comparision between the two.
fn step_and_compare(gb: &mut Gameboy, other_gb: &mut Device) -> bool {
    light_compare(gb, other_gb)
}

/// Compares CPU, PPU, and key memory registers. Meant to be used after every step.
fn light_compare(gb: &Gameboy, other_gb: &mut Device) -> bool {
    todo!()
}

/// Compares performs the light compare and a full memory sweep.
fn extensive_compare(gb: &Gameboy, other_gb: &mut Device) -> bool {
    light_compare(gb, other_gb) && sweep_mem(&gb.mem, &mut other_gb.cpu.mmu)
}

fn check_cpu(cpu: &Cpu, reg: &Registers) -> bool {
    cpu.a.0 == reg.a
        && cpu.f.as_byte() == reg.f
        && cpu.b.0 == reg.b
        && cpu.c.0 == reg.c
        && cpu.d.0 == reg.d
        && cpu.e.0 == reg.e
        && cpu.h.0 == reg.h
        && cpu.l.0 == reg.l
        && cpu.sp.0 == reg.sp
        && cpu.pc.0 == reg.pc + 1
}

fn print_reg(reg: &Registers) {
    print!("[ rboy] Reg {{");
    print!(" A=0x{:0>2X}", reg.a);
    print!(" F=0x{:0>2X}", reg.f);
    print!(" B=0x{:0>2X}", reg.b);
    print!(" C=0x{:0>2X}", reg.c);
    print!(" D=0x{:0>2X}", reg.d);
    print!(" E=0x{:0>2X}", reg.e);
    print!(" H=0x{:0>2X}", reg.h);
    print!(" L=0x{:0>2X}", reg.l);
    print!(" SP=0x{:0>4X}", reg.sp);
    print!(" PC=0x{:0>4X} }}", reg.pc);
    println!();
}

fn sweep_mem(mem: &MemoryMap, mmu: &mut MMU) -> bool {
    let mut digest = 0;
    for i in 0u16..=u16::MAX {
        if (0xFF10..=0xFF3F).contains(&i) {
            continue;
        }
        let a = mem.read_byte(i);
        let b = mmu.rb(i);
        if a != b {
            digest += 1;
            println!("Memory mismatch @ 0x{i:0>4X}, spirit=0x{a:0>2X}, rboy=0x{b:0>2X}");
        }
    }
    println!("Total memory mismatches: {digest}");
    digest == 0
}
