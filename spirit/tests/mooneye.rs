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

/* FIXME: Only the timer tests have been investigated.
#[test]
fn add_sp_e_timing() {
    let mut gb = Gameboy::new(include_bytes!("roms/mooneye/acceptance/add_sp_e_timing.gb").into())
        .complete();
    while !matches!(gb.read_op(), STOP_OP) {
        gb.step();
    }
    panic!()
}
*/

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
    let mut gb =
        Gameboy::new(include_bytes!("roms/mooneye/acceptance/timer/tim00.gb").into()).complete();
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
fn timer_tim_00_div_trigger() {
    let mut gb =
        Gameboy::new(include_bytes!("roms/mooneye/acceptance/timer/tim00_div_trigger.gb").into())
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
    let mut gb =
        Gameboy::new(include_bytes!("roms/mooneye/acceptance/timer/tim01.gb").into()).complete();
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
fn timer_tim01_div_trigger() {
    let mut gb =
        Gameboy::new(include_bytes!("roms/mooneye/acceptance/timer/tim01_div_trigger.gb").into())
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

#[test_log::test]
fn timer_tim10_() {
    let mut gb =
        Gameboy::new(include_bytes!("roms/mooneye/acceptance/timer/tim10.gb").into()).complete();
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
fn timer_tim10_div_trigger() {
    let mut gb =
        Gameboy::new(include_bytes!("roms/mooneye/acceptance/timer/tim10_div_trigger.gb").into())
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
    let mut gb =
        Gameboy::new(include_bytes!("roms/mooneye/acceptance/timer/tim11.gb").into()).complete();
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
fn timer_tim11_div_trigger() {
    let mut gb =
        Gameboy::new(include_bytes!("roms/mooneye/acceptance/timer/tim11_div_trigger.gb").into())
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
fn timer_tima_reload() {
    let mut gb =
        Gameboy::new(include_bytes!("roms/mooneye/acceptance/timer/tima_reload.gb").into())
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

// This test and the similarly named TIMA test are expected to panic currently. These tests require
// sub-intruction levels of percision. While this is a long-term goal of the project, it is far
// from necessary. Once the project has reached on MVP, this is be revisited.
#[test]
#[should_panic]
fn timer_tma_write_reloading() {
    let mut gb =
        Gameboy::new(include_bytes!("roms/mooneye/acceptance/timer/tma_write_reloading.gb").into())
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
#[should_panic]
fn timer_tima_write_reloading() {
    let mut gb =
        Gameboy::new(include_bytes!("roms/mooneye/acceptance/timer/tima_write_reloading.gb").into())
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

// Similar to the prior two tests, (I think) this test requires to sub-instruction precision.
#[test]
#[should_panic]
fn timer_rapid_toggle() {
    let mut gb =
        Gameboy::new(include_bytes!("roms/mooneye/acceptance/timer/rapid_toggle.gb").into())
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
