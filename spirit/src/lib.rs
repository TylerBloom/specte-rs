//! Spirit is the core crate for the specters project. Contained here is the all of the logic for
//! creating a gameboy emulator and running it with a game ROM. This crate aims to be platform
//! agnostic and free of the UI-specifics. Other projects will wrap this logic in their own ways.
//! For example, the handheld device will have different IO than a desktop. Moreover, the syncing
//! webservice will not need a UI per se.

// TODO: The `tick` methods on most of the processes is incorrect, but the step methods are fine.

#![allow(
    dead_code,
    unused,
    private_interfaces,
    clippy::diverging_sub_expression,
    clippy::all
)]

use std::{
    borrow::Cow,
    ops::{Deref, DerefMut},
};

use serde::{Deserialize, Serialize};

use cpu::{Cpu, CpuState, check_bit_const};
use lookup::Instruction;
use mem::{MemoryBankController, MemoryMap, StartUpHeaders, vram::PpuMode};
use ppu::Ppu;

use crate::lookup::JumpOp;

pub mod cpu;
pub mod lookup;
pub mod mem;
pub mod ppu;
pub mod rom;
#[doc(hidden)]
pub mod utils;

/// Represents a Gameboy color with a cartridge inserted.
#[derive(Debug, Hash, Serialize, Deserialize)]
pub struct Gameboy {
    pub mem: MemoryMap,
    pub ppu: Ppu,
    cpu: Cpu,
}

#[must_use = "Start up sequence is lazy and should be ticked. Dropping this object to complete the startup sequence instantly."]
pub struct StartUpSequence {
    gb: Gameboy,
    op: Instruction,
    counter: u8,
    done: bool,
    /// The memory that the ROM contains that get mapped over during the startup process.
    remap_mem: Box<StartUpHeaders>,
}

impl Gameboy {
    /// Takes data that represents the data stored on a game cartridge and uses it to construct the
    pub fn new<'a, C: Into<Cow<'a, [u8]>>>(cart: C) -> StartUpSequence {
        StartUpSequence::new(Self {
            mem: MemoryMap::new(cart),
            cpu: Cpu::new(),
            ppu: Ppu::new(),
        })
    }

    pub fn next_frame(&mut self) -> FrameSequence<'_> {
        FrameSequence::new(self)
    }

    pub fn scanline_step(&mut self) {
        match self.ppu.state() {
            PpuMode::VBlank => {
                let mut counter = 0;
                // Process ops until we see OamScan or have processed a number of ticks equal to
                // one scan line.
                loop {
                    let step = self.step();
                    counter += step.op_len() as usize;
                    step.complete();
                    if counter >= 456 || matches!(self.ppu.state(), PpuMode::OamScan) {
                        return;
                    }
                }
            }
            _ => {
                // process ops until we see OamScan or VBlank
                loop {
                    self.step().complete();
                    if matches!(self.ppu.state(), PpuMode::OamScan | PpuMode::VBlank) {
                        return;
                    }
                }
            }
        }
    }

    pub fn cpu(&self) -> &Cpu {
        &self.cpu
    }

    /// Registers a button press.
    pub fn button_press(&mut self, input: ButtonInput) {
        self.mem.request_button_int(input);
    }

    /// This method returns an iterator over the current and upcoming instructions, based on the
    /// location of the PC. This does not do any clever predictions, like trying to determine if a
    /// conditional jump will execute. Rather, it is to be used for debugging by providing a window
    /// into region around the PC.
    pub fn op_iter<'b, 'a: 'b>(&'a self) -> impl 'b + Iterator<Item = (u16, Instruction)> {
        let mut pc = self.cpu.pc.0;
        std::iter::repeat(()).map(move |()| {
            let op = self.mem.read_op(pc, self.cpu.ime);
            let old_pc = pc;
            // TODO: Either commit to this all of the way or don't. The core issue here is that
            // some data might be read and is not meant to be an instruction. Panic catching is
            // also an option here.
            if let Instruction::Jump(JumpOp::Absolute(dest)) = op {
                pc = dest;
            } else {
                pc += op.size() as u16;
            }
            (old_pc, op)
        })
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

    /// This method is only called by the step sequence when it gets ticked. This represents a
    /// clock cycle (not a machine cycle). This involves ticking the memory and PPU.
    fn tick(&mut self) {
        self.mem.tick();
        /*
        let mask = {
            let bit = self.cpu().ime as u8;
            (bit << 0) | (bit << 1) | (bit << 2) | (bit << 3) | (bit << 4)
        };
        self.mem.io_mut().interrupt_flags &= mask;
        */
        self.ppu.tick(&mut self.mem);
    }

    pub fn read_op(&self) -> Instruction {
        self.cpu.read_op(&self.mem)
    }

    fn apply_op(&mut self, op: Instruction) {
        self.cpu.execute(op, &mut self.mem)
    }

    fn start_up_apply_op(&mut self, op: Instruction) -> Option<Instruction> {
        self.cpu.start_up_execute(op, &mut self.mem)
    }

    pub fn is_running(&self) -> bool {
        self.cpu.state == CpuState::Running
    }

    pub fn is_halted(&self) -> bool {
        self.cpu.state == CpuState::Halted
    }

    pub fn is_stopped(&self) -> bool {
        self.cpu.state == CpuState::Stopped
    }
}

pub enum ButtonInput {
    Joypad(JoypadInput),
    Ssab(SsabInput),
}

#[repr(u8)]
pub enum JoypadInput {
    Right = 0x1,
    Left = 0x2,
    Up = 0x4,
    Down = 0x8,
}

#[repr(u8)]
pub enum SsabInput {
    A = 0x1,
    B = 0x2,
    Select = 0x4,
    Start = 0x8,
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

    /// Processes the current instructions, fetches the next instruction, and undates this process
    /// in-place.
    fn step_and_next(&mut self) {
        (self.counter..self.op.length(&self.gb.cpu)).for_each(|_| self.gb.tick());
        self.gb.apply_op(self.op);
        self.op = self.gb.read_op();
        self.counter = 0;
    }

    pub fn tick(&mut self) {
        self.counter += 1;
        if self.is_complete() {
            self.gb.apply_op(self.op);
        } else {
            self.gb.tick();
        }
    }

    pub fn press_joypad_button(&mut self, input: ButtonInput) {
        self.gb.button_press(input)
    }

    pub fn is_complete(&self) -> bool {
        self.counter >= self.op.length(&self.gb.cpu)
    }

    /// Consumes the step processor, dropping it immediately.
    pub fn complete(self) {}

    pub fn op_len(&self) -> u8 {
        self.op.length(&self.gb.cpu)
    }
}

impl Drop for StepProcess<'_> {
    fn drop(&mut self) {
        (self.counter..self.op.length(&self.gb.cpu)).for_each(|_| self.gb.tick());
        self.gb.apply_op(self.op);
    }
}

impl StartUpSequence {
    pub fn frame_step(&mut self) -> FrameSequence<'_> {
        FrameSequence::new(self)
    }

    pub fn scanline_step(&mut self) {
        self.gb.scanline_step()
    }
}

impl Deref for StartUpSequence {
    type Target = Gameboy;

    fn deref(&self) -> &Self::Target {
        self.gb()
    }
}

impl DerefMut for StartUpSequence {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.gb
    }
}

impl StartUpSequence {
    fn new(mut gb: Gameboy) -> Self {
        let remap_mem = Box::new(gb.mem.start_up_remap());
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
        self.gb.mem.request_vblank_int()
    }

    pub fn tick(&mut self) {
        if !self.is_complete() {
            self.counter += 1;
            self.gb.tick();
            if self.is_step_complete() {
                self.step()
            }
        }
    }

    pub fn step(&mut self) {
        (self.counter..self.op.length(&self.gb.cpu)).for_each(|_| self.gb.tick());
        self.counter = 0;
        self.load_next_op();
    }

    fn load_next_op(&mut self) {
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
    pub fn complete(mut self) -> Gameboy {
        while !self.is_complete() {
            self.step()
        }
        self.gb.mem.start_up_unmap(*self.remap_mem);
        self.gb
    }
}

pub struct FrameSequence<'a> {
    next: StepProcess<'a>,
    mode: PpuMode,
}

impl<'a> FrameSequence<'a> {
    fn new(gb: &'a mut Gameboy) -> Self {
        let mode = gb.ppu.state();
        Self {
            mode,
            next: StepProcess::new(gb),
        }
    }

    fn tick(&mut self) {
        if !self.is_complete() {
            self.mode = self.next.gb.ppu.state();
            self.next.tick();
        }
    }

    fn is_complete(&self) -> bool {
        matches!(
            (self.mode, self.next.gb.ppu.state()),
            (PpuMode::VBlank, PpuMode::OamScan)
        )
    }

    pub fn complete(self) {}

    fn step(&mut self) {
        if !self.is_complete() {
            self.mode = self.next.gb.ppu.state();
            self.next.step_and_next();
        }
    }
}

impl<'a> Drop for FrameSequence<'a> {
    fn drop(&mut self) {
        while !self.is_complete() {
            self.step()
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::Gameboy;

    #[test]
    fn start_up_completion() {
        Gameboy::new(include_bytes!("../tests/roms/acid/which.gb")).complete();
    }
}
