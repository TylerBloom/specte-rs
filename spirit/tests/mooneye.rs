use std::{path::PathBuf};

use spirit::{
    Gameboy,
    lookup::{HalfRegister, Instruction, LoadOp, RegOrPointer},
    utils::Wrapping,
};

const STOP_OP: Instruction = Instruction::Load(LoadOp::Basic {
    dest: RegOrPointer::Reg(HalfRegister::B),
    src: RegOrPointer::Reg(HalfRegister::B),
});

const GOAL_B_REG: Wrapping<u8> = Wrapping(3u8);
const GOAL_C_REG: Wrapping<u8> = Wrapping(5u8);
const GOAL_D_REG: Wrapping<u8> = Wrapping(8u8);
const GOAL_E_REG: Wrapping<u8> = Wrapping(13u8);
const GOAL_H_REG: Wrapping<u8> = Wrapping(21u8);
const GOAL_L_REG: Wrapping<u8> = Wrapping(34u8);

fn mooneye_test(cart: Vec<u8>) -> bool {
    let mut gb = Gameboy::new(cart).complete();
    while !matches!(gb.read_op(), STOP_OP) {
        gb.step();
    }
    let cpu = gb.cpu();
    (cpu.b == GOAL_B_REG)
        && (cpu.c == GOAL_C_REG)
        && (cpu.d == GOAL_D_REG)
        && (cpu.e == GOAL_E_REG)
        && (cpu.h == GOAL_H_REG)
        && (cpu.l == GOAL_L_REG)
}

#[test]
fn add_sp_e_timing() {
    let mut gb = Gameboy::new(include_bytes!("roms/mooneye/acceptance/add_sp_e_timing.gb").into())
        .complete();
    while !matches!(gb.read_op(), STOP_OP) {
        gb.step();
    }
    panic!()
}

#[test]
fn div_timing() {
    let mut gb =
        Gameboy::new(include_bytes!("roms/mooneye/acceptance/div_timing.gb").into()).complete();
    for _ in 0..10 {
        while !matches!(gb.read_op(), STOP_OP) {
            gb.step();
        }
    }
    let cpu = gb.cpu();
    assert_eq!(cpu.b, GOAL_B_REG);
    assert_eq!(cpu.c, GOAL_C_REG);
    assert_eq!(cpu.d, GOAL_D_REG);
    assert_eq!(cpu.e, GOAL_E_REG);
    assert_eq!(cpu.h, GOAL_H_REG);
    assert_eq!(cpu.l, GOAL_L_REG);
}

#[test]
fn timer_div_write() {
    let mut gb = Gameboy::new(include_bytes!("roms/mooneye/acceptance/timer/div_write.gb").into())
        .complete();
    while !matches!(gb.read_op(), STOP_OP) {
        gb.step();
    }
    let cpu = gb.cpu();
    assert_eq!(cpu.b, GOAL_B_REG);
    assert_eq!(cpu.c, GOAL_C_REG);
    assert_eq!(cpu.d, GOAL_D_REG);
    assert_eq!(cpu.e, GOAL_E_REG);
    assert_eq!(cpu.h, GOAL_H_REG);
    assert_eq!(cpu.l, GOAL_L_REG);
}

#[test]
fn timer_tim_00() {
    let mut gb = Gameboy::new(include_bytes!("roms/mooneye/acceptance/timer/tim00.gb").into())
        .complete();
    while !matches!(gb.read_op(), STOP_OP) {
        gb.step();
    }
    let cpu = gb.cpu();
    assert_eq!(cpu.b, GOAL_B_REG);
    assert_eq!(cpu.c, GOAL_C_REG);
    assert_eq!(cpu.d, GOAL_D_REG);
    assert_eq!(cpu.e, GOAL_E_REG);
    assert_eq!(cpu.h, GOAL_H_REG);
    assert_eq!(cpu.l, GOAL_L_REG);
}

#[test]
fn timer_tim01() {
    let mut gb = Gameboy::new(include_bytes!("roms/mooneye/acceptance/timer/tim01.gb").into())
        .complete();
    while !matches!(gb.read_op(), STOP_OP) {
        gb.step();
    }
    let cpu = gb.cpu();
    assert_eq!(cpu.b, GOAL_B_REG);
    assert_eq!(cpu.c, GOAL_C_REG);
    assert_eq!(cpu.d, GOAL_D_REG);
    assert_eq!(cpu.e, GOAL_E_REG);
    assert_eq!(cpu.h, GOAL_H_REG);
    assert_eq!(cpu.l, GOAL_L_REG);
}

#[test]
fn timer_tim10() {
    let mut gb = Gameboy::new(include_bytes!("roms/mooneye/acceptance/timer/tim10.gb").into())
        .complete();
    while !matches!(gb.read_op(), STOP_OP) {
        gb.step();
    }
    let cpu = gb.cpu();
    assert_eq!(cpu.b, GOAL_B_REG);
    assert_eq!(cpu.c, GOAL_C_REG);
    assert_eq!(cpu.d, GOAL_D_REG);
    assert_eq!(cpu.e, GOAL_E_REG);
    assert_eq!(cpu.h, GOAL_H_REG);
    assert_eq!(cpu.l, GOAL_L_REG);
}

#[test]
fn timer_tim11() {
    let mut gb = Gameboy::new(include_bytes!("roms/mooneye/acceptance/timer/tim11.gb").into())
        .complete();
    while !matches!(gb.read_op(), STOP_OP) {
        gb.step();
    }
    let cpu = gb.cpu();
    assert_eq!(cpu.b, GOAL_B_REG);
    assert_eq!(cpu.c, GOAL_C_REG);
    assert_eq!(cpu.d, GOAL_D_REG);
    assert_eq!(cpu.e, GOAL_E_REG);
    assert_eq!(cpu.h, GOAL_H_REG);
    assert_eq!(cpu.l, GOAL_L_REG);
}

#[test]
fn test_all() {
    let mut failures = Vec::new();
    let mut successes = Vec::new();
    let mut root: PathBuf = env!("CARGO_MANIFEST_DIR").parse().unwrap();
    root.push("tests/roms/mooneye/acceptance/timer");
    for file in std::fs::read_dir(root).unwrap() {
        let file = file.unwrap();
        if !file
            .path()
            .extension()
            .unwrap()
            .to_str()
            .unwrap()
            .contains("gb")
        {
            continue;
        }
        println!("{:?}", file.path());
        if mooneye_test(std::fs::read(file.path()).unwrap()) {
            successes.push(
                file.path()
                    .file_name()
                    .unwrap()
                    .to_str()
                    .unwrap()
                    .to_owned(),
            )
        } else {
            failures.push(
                file.path()
                    .file_name()
                    .unwrap()
                    .to_str()
                    .unwrap()
                    .to_owned(),
            )
        }
    }
    println!("Failures:");
    for f in &failures {
        println!(" - {f:?}");
    }
    println!();
    println!("Successes:");
    for s in successes {
        println!(" - {s:?}");
    }
    if !failures.is_empty() {
        panic!()
    }
}
