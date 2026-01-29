#![allow(dead_code, unused_variables)]
use std::fmt::Display;
use std::path::PathBuf;
use std::str::FromStr;

use clap::Parser;
use rboy::cpu::Action;
use rboy::device::Device;
use rboy::gpu::GPU;
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
use tracing::level_filters::LevelFilter;
use tracing_subscriber::fmt::format::FmtSpan;

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
    tracing::warn!("[spirit] Constructed GB");
    let mut other_gb = Device::new_cgb_from_buffer(rom, false, None).unwrap();
    other_gb.cpu.mmu.wram = [0; 0x8000];
    tracing::warn!("[  rboy] Constructed GB");
    if !sweep_mem(&gb.mem, &mut other_gb.cpu.mmu) {
        panic!("Memory maps did not match");
    }

    let mut init_gb = gb.clone();
    let mut init_other_gb = other_gb.clone();

    const STEP_SIZE: usize = 10_000;
    let mut counter = 0;
    'outer: loop {
        let init_gb = gb.clone();
        let init_other = other_gb.clone();
        for i in 0..STEP_SIZE {
            if !step_and_compare(&mut gb, &mut other_gb) {
                counter += bisect_failure(init_gb, init_other, i);
                break 'outer;
            }
        }
        if !extensive_compare(&gb, &mut other_gb) {
            tracing::warn!(
                "Failed extensive compare after {} steps",
                counter + STEP_SIZE
            );
            counter += bisect_failure(init_gb, init_other, STEP_SIZE);
            break 'outer;
        }
        tracing::warn!("Passed extensive compare after {counter} steps");
        counter += STEP_SIZE;
    }

    tracing_subscriber::fmt()
        .without_time()
        .compact()
        .with_max_level(LevelFilter::WARN)
        .init();

    tracing::warn!("[spirit] {:?}", init_gb.ppu.inner);
    print_gpu(&init_other_gb.cpu.mmu.gpu);
    for _ in 0..(counter) {
        step_and_compare(&mut init_gb, &mut init_other_gb);
        tracing::warn!("[spirit] {:?}", init_gb.ppu.inner);
        print_gpu(&init_other_gb.cpu.mmu.gpu);
    }
    step_and_compare(&mut init_gb, &mut init_other_gb);
    extensive_compare(&init_gb, &mut init_other_gb);
    tracing::warn!("[spirit] {:?}", init_gb.ppu.inner);
    print_gpu(&init_other_gb.cpu.mmu.gpu);

    tracing::warn!("Error occurred after {counter} steps");

    /*
    let mut program: Vec<_> = program.into_iter().collect();
    program.sort_by_key(|(i, _)| *i);
    program.reverse();
    for (i, op) in program {
        println!("0x{i:0>4X} @ {op}");
    }
    println!("Spirit stopped after {counter} instructions");
    */
}

/// Takes two emulators that are in states known to be matching (i.e. pass an extensive compare),
/// but that are misaligned after the given number of steps, `steps`. Returns the number of steps
/// needed for the first point in time for the misalignment to occur and returns that number.
fn bisect_failure(mut gb: Gameboy, mut other_gb: Device, steps: usize) -> usize {
    if steps == 0 {
        return 0;
    }
    let gb_copy = gb.clone();
    let other_gb_copy = other_gb.clone();
    let to = steps / 2;
    for i in 0..to {
        // If step_and_compare fails, the light compare has failed. This indicates an earlier,
        // deeper failure happened. We can take the number of steps that have occurred thus far as
        // the upper bound and bisect from here.
        if !step_and_compare(&mut gb, &mut other_gb) {
            tracing::warn!("Step and compare failed after {i} steps");
            return bisect_failure(gb_copy, other_gb_copy, i);
        }
    }

    if extensive_compare(&gb, &mut other_gb) {
        tracing::warn!("Passed extensive compare after {to} steps");
        // They match
        // FIXME: Consider case where steps was odd
        to + bisect_failure(gb, other_gb, steps / 2)
    } else {
        tracing::warn!("Failed extensive compare after {to} steps");
        // They mismatch
        bisect_failure(gb_copy, other_gb_copy, steps / 2)
    }
}

/// Steps both emulators forward a single tick then does a light comparision between the two.
fn step_and_compare(gb: &mut Gameboy, other_gb: &mut Device) -> bool {
    let i = gb.cpu.pc.0;
    let op = gb.read_op();
    let span = tracing::warn_span!("[spirit]");
    let guard = span.enter();
    let action = match op {
        Instruction::Stall => {
            tracing::warn!("Halted");
            Action::Halted
        }
        Instruction::Stopped => {
            tracing::warn!("Stopped");
            Action::Stopped
        }
        Instruction::Prefixed => {
            let pc = gb.cpu().pc.0;
            tracing::warn!("Looking for prefixed op code @ 0x{pc:0>4X}");
            let op_code = gb.mem.read_byte(pc);
            tracing::warn!(
                "Executed prefixed op: {} (0x{op_code:0>2X})",
                parse_prefixed_instruction(op_code)
            );
            Action::ExecutedPrefixedOp(op_code)
        }
        Instruction::Unused => todo!(),
        Instruction::Transfer => todo!(),
        Instruction::Interrupt(InterruptOp::LCD) => {
            tracing::warn!("VBLank interrupt");
            Action::Lcd
        }
        Instruction::Interrupt(InterruptOp::VBlank) => {
            tracing::warn!("LDC interrupt");
            Action::VBlank
        }
        Instruction::Interrupt(InterruptOp::Timer) => {
            tracing::warn!("Timer interrupt");
            Action::Timer
        }
        Instruction::Interrupt(InterruptOp::Serial) => {
            tracing::warn!("Serial interrupt");
            Action::Serial
        }
        Instruction::Interrupt(InterruptOp::Joypad) => {
            tracing::warn!("Joypad interrupt");
            Action::Joypad
        }

        op => {
            let opcode = lookup::OP_LOOKUP
                .iter()
                .enumerate()
                .find_map(|(i, o)| (&op == o).then_some(i))
                .unwrap();
            tracing::warn!("Executing op: {op} (0x{opcode:0>2X})",);
            Action::ExecutedOp(opcode as u8)
        }
    };
    drop(guard);
    let i = other_gb.cpu.reg.pc;
    gb.step();
    let (_, rboy_action) = other_gb.do_cycle();
    let span = tracing::warn_span!("[  rboy]");
    let guard = span.enter();
    match rboy_action {
        Action::ExecutedOp(op_code) => {
            // program.insert(i, Either::Left(parse_instruction(op_code)));
            tracing::warn!(
                "Executed op: {} (0x{op_code:0>2X})",
                parse_instruction(op_code)
            )
        }
        Action::ExecutedPrefixedOp(op_code) => {
            // program.insert(i, Either::Right(parse_prefixed_instruction(op_code)));
            tracing::warn!(
                "Executed prefixed op: {} (0x{op_code:0>2X})",
                parse_prefixed_instruction(op_code)
            )
        }
        Action::VBlank => tracing::warn!("VBLank interrupt"),
        Action::Lcd => tracing::warn!("LDC interrupt"),
        Action::Timer => tracing::warn!("Timer interrupt"),
        Action::Serial => tracing::warn!("Serial interrupt"),
        Action::Joypad => tracing::warn!("Joypad interrupt"),
        Action::Halted => tracing::warn!("Halted"),
        Action::Stopped => tracing::warn!("Stopped"),
    }
    drop(guard);
    if rboy_action != action {
        tracing::warn!("Mismatched actions");
    }
    light_compare(gb, other_gb)
}

/// Compares CPU, PPU, and key memory registers. Meant to be used after every step.
fn light_compare(gb: &Gameboy, other_gb: &mut Device) -> bool {
    let cpu_check = check_cpu(gb.cpu(), &other_gb.cpu.reg);
    if !cpu_check {
        tracing::warn!("CPU mismatch");
        tracing::warn!("[spirit] {cpu}", cpu = gb.cpu());
        print_reg(&other_gb.cpu.reg);
    }
    let ppu_check = check_ppu(gb, other_gb);
    if !ppu_check {
        tracing::warn!("PPU mismatch");
    }
    let keys_check = check_key_registers(gb, other_gb);
    if !keys_check {
        tracing::warn!("Key register mismatch");
    }
    cpu_check && ppu_check && keys_check
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

fn check_ppu(gb: &Gameboy, other_gb: &mut Device) -> bool {
    true
}

fn check_key_registers(gb: &Gameboy, other_gb: &mut Device) -> bool {
    true
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
            tracing::warn!("Memory mismatch @ 0x{i:0>4X}, spirit=0x{a:0>2X}, rboy=0x{b:0>2X}");
        }
    }
    tracing::warn!("Total memory mismatches: {digest}");
    digest == 0
}

fn print_reg(reg: &Registers) {
    tracing::warn!("[ rboy] Reg {{ A=0x{:0>2X}, F=0x{:0>2X}, B=0x{:0>2X} , C=0x{:0>2X} , D=0x{:0>2X} , E=0x{:0>2X} , H=0x{:0>2X} , L=0x{:0>2X} , SP=0x{:0>4X}, PC=0x{:0>4X} }}"
        , reg.a
, reg.f
, reg.b
, reg.c
, reg.d
, reg.e
, reg.h
, reg.l
, reg.sp
, reg.pc);
}

fn print_gpu(gpu: &GPU) {
    tracing::warn!(
        "[  rboy] GPU {{ mode: {}, modeclock: {} }}",
        gpu.mode,
        gpu.modeclock
    )
}
