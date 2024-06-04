//! Spirit is the core crate for the specters project. Contained here is the all of the logic for
//! creating a gameboy emulator and running it with a game ROM. This crate aims to be platform
//! agnostic and free of the UI-specifics. Other projects will wrap this logic in their own ways.
//! For example, the handheld device will have different IO than a desktop. Moreover, the syncing
//! webservice will not need a UI per se.
//!
//! # Notes
//! The Z80 CPU is big endian.

#![allow(
    dead_code,
    unused,
    private_interfaces,
    clippy::diverging_sub_expression,
    clippy::all
)]

use std::borrow::Cow;

use cpu::{check_bit_const, Cpu};
use lookup::Instruction;
use mbc::{MemoryBankController, MemoryMap, StartUpHeaders};

pub mod cpu;
pub mod lookup;
pub mod mbc;
pub mod rom;

/// Represents a Gameboy color with a cartridge inserted.
#[derive(Debug, Hash)]
pub struct Gameboy {
    pub mem: MemoryMap,
    cpu: Cpu,
}

impl Gameboy {
    /// Takes data that represents the data stored on a game cartridge and uses it to construct the
    pub fn new<'a, C: Into<Cow<'a, [u8]>>>(cart: C) -> Self {
        Self {
            mem: MemoryMap::new(cart),
            cpu: Cpu::new(),
        }
    }

    pub fn cpu(&self) -> &Cpu {
        &self.cpu
    }

    /// Runs the power-up sequence for the gameboy. During this time, the Gameboy remaps part of
    /// the memory to read from the start up sequence.
    pub fn start_up(&mut self) -> StartUpSequence<'_> {
        StartUpSequence::new(self)
    }

    /// This returns a state that will track the number of required clock ticks it takes to
    /// complete the next operation. If the state is dropped before the required number of ticks,
    /// the intruction is automatically ran.
    ///
    /// This is done to prevent saving the gameboy in odd states. While the `StepProcess` exists,
    /// the Gameboy can not be otherwise accessed.
    pub fn step(&mut self) -> StepProcess<'_> {
        StepProcess::new(self)
    }

    fn read_op(&self) -> Instruction {
        self.cpu.read_op(&self.mem)
    }

    fn apply_op(&mut self, op: Instruction) {
        self.cpu.execute(op, &mut self.mem)
    }

    fn start_up_apply_op(&mut self, op: Instruction) -> Option<Instruction> {
        self.cpu.start_up_execute(op, &mut self.mem)
    }
}

#[repr(u8)]
pub enum JoypadInput {
    Right = 0b0001_0001u8,
    Left  = 0b0001_0010u8,
    Up    = 0b0001_0100u8,
    Down  = 0b0001_1000u8,
}

#[repr(u8)]
pub enum ButtonInput {
    A      = 0b0010_0001u8,
    B      = 0b0010_0010u8,
    Select = 0b0010_0100u8,
    Start  = 0b0010_1000u8,
}

#[must_use = "Stepping returns a counter that must be ticked to perform the next operation. Dropping this object instantly completes the operation."]
pub struct StepProcess<'a> {
    gb: &'a mut Gameboy,
    op: Instruction,
    counter: u8,
}

impl<'a> StepProcess<'a> {
    fn new(gb: &'a mut Gameboy) -> Self {
        let op = gb.read_op();
        Self { gb, op, counter: 0 }
    }

    pub fn tick(&mut self) {
        self.counter += 1;
        if self.is_complete() {
            self.gb.apply_op(self.op);
        }
    }

    pub fn press_joypad_button(&mut self, button: JoypadInput) {
        let reg = &mut self.gb.mem[0xFF00];
        if check_bit_const::<4>(*reg) {
            *reg = button as u8;
        }
    }

    pub fn press_button(&mut self, button: ButtonInput) {
        let reg = &mut self.gb.mem[0xFF00];
        if check_bit_const::<5>(*reg) {
            *reg = button as u8;
        }
    }

    pub fn is_complete(&self) -> bool {
        self.counter >= self.op.length(&self.gb.cpu)
    }

    /// Consumes the step processor, dropping it immediately.
    pub fn complete(self) {}
}

impl Drop for StepProcess<'_> {
    fn drop(&mut self) {
        self.gb.apply_op(self.op);
    }
}

#[must_use = "Start up sequence is lazy and should be ticked. Dropping this object to complete the startup sequence instantly."]
pub struct StartUpSequence<'a> {
    gb: &'a mut Gameboy,
    op: Instruction,
    counter: u8,
    done: bool,
    /// The memory that the ROM contains that get mapped over during the startup process.
    remap_mem: StartUpHeaders,
}

impl<'a> StartUpSequence<'a> {
    fn new(gb: &'a mut Gameboy) -> Self {
        let remap_mem = gb.mem.start_up_remap();
        let op = gb.read_op();
        Self {
            gb,
            op,
            counter: 0,
            done: false,
            remap_mem,
        }
    }

    pub fn next_op(&self) -> &Instruction {
        &self.op
    }

    pub fn gb(&self) -> &Gameboy {
        &self.gb
    }


    pub fn set_vblank(&mut self) {
        self.gb.mem.set_vblank_req()
    }

    pub fn tick(&mut self) {
        if !self.is_complete() {
            self.counter += 1;
            if self.is_step_complete() {
                self.step()
            }
        }
    }

    pub fn step(&mut self) {
        self.counter = 0;
        if let Some(op) = self.gb.start_up_apply_op(self.op) {
            self.op = op;
        } else {
            self.done = true
        }
    }

    pub fn is_complete(&self) -> bool {
        self.done
    }

    pub fn is_step_complete(&self) -> bool {
        self.counter >= self.op.length(&self.gb.cpu)
    }

    /// Consumes the start-up processor, dropping it immediately.
    pub fn complete(self) {}
}

impl Drop for StartUpSequence<'_> {
    fn drop(&mut self) {
        while !self.is_complete() {
            self.step()
        }
        self.gb.mem.start_up_unmap(self.remap_mem)
    }
}

#[cfg(test)]
mod tests {
    use std::mem::size_of;

    use crate::lookup::Instruction;

    #[test]
    fn size() {
        assert!(size_of::<Instruction>() <= size_of::<usize>())
    }
}
