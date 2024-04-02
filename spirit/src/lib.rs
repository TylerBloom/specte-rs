//! Spirit is the core crate for the specters project. Contained here is the all of the logic for
//! creating a gameboy emulator and running it with a game ROM. This crate aims to be platform
//! agnostic and free of the UI-specifics. Other projects will wrap this logic in their own ways.
//! For example, the handheld device will have different IO than a desktop. Moreover, the syncing
//! webservice will not need a UI per se.
//!
//! # Notes
//! The Z80 CPU is big endian.

#![allow(dead_code, unused)]

use std::borrow::Cow;

use cpu::Cpu;
use lookup::Instruction;
use mbc::MemoryBankController;

mod cpu;
mod decoder;
mod emulator;
mod gameboy;
mod mbc;
mod rom;
mod lookup;

/// Represents a Gameboy color with a cartridge inserted.
pub struct Gameboy {
    mbc: MemoryBankController,
    cpu: Cpu,
}

impl Gameboy {
    /// Takes data that represents the data stored on a game cartridge and uses it to construct the
    pub fn new<'a, C: Into<Cow<'a, [u8]>>>(cart: C) -> Self {
        Self {
            mbc: MemoryBankController::new(cart),
            cpu: Cpu::new(),
        }
    }

    /// Runs the power-up sequence for the gameboy. During this time, the Gameboy remaps part of
    /// the memory to read from the start up sequence.
    // TODO: It might be a good idea to have this function return an state type that can ticked and
    // read from.
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
        self.cpu.read_op(&self.mbc)
    }

    fn start_up_read_op(&self) -> Instruction {
        self.cpu.start_up_read_op(&self.mbc)
    }

    fn apply_op(&mut self, op: Instruction) {
        self.cpu.execute(op, &mut self.mbc)
    }

    fn start_up_apply_op(&mut self, op: Instruction) -> Option<Instruction> {
        self.cpu.start_up_execute(op, &mut self.mbc)
    }
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
}

impl<'a> StartUpSequence<'a> {
    fn new(gb: &'a mut Gameboy) -> Self {
        println!("-- Start up Gameboy");
        let op = gb.start_up_read_op();
        println!("First start up op: {op:?}");
        Self { gb, op, counter: 0 }
    }

    pub fn tick(&mut self) {
        self.counter += 1;
        if self.is_complete() {
            if let Some(op) = self.gb.start_up_apply_op(self.op) {
                self.op = op;
                self.counter = 0;
            }
        }
    }

    pub fn is_complete(&self) -> bool {
        self.counter >= self.op.length(&self.gb.cpu)
    }

    /// Consumes the start-up processor, dropping it immediately.
    pub fn complete(self) {}
}

impl Drop for StartUpSequence<'_> {
    fn drop(&mut self) {
        while let Some(op) = self.gb.start_up_apply_op(self.op) {
            self.op = op;
        }
    }
}
