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

use std::borrow::Cow;

use cpu::{check_bit_const, Cpu};
use lookup::Instruction;
use mem::{vram::PpuMode, MemoryBankController, MemoryMap, StartUpHeaders};
use ppu::Ppu;

use crate::lookup::JumpOp;

pub mod cpu;
pub mod lookup;
pub mod mem;
pub mod ppu;
pub mod rom;

/// Represents a Gameboy color with a cartridge inserted.
#[derive(Debug, Hash)]
pub struct Gameboy {
    pub mem: MemoryMap,
    pub ppu: Ppu,
    cpu: Cpu,
}

impl Gameboy {
    /// Takes data that represents the data stored on a game cartridge and uses it to construct the
    pub fn new<'a, C: Into<Cow<'a, [u8]>>>(cart: C) -> Self {
        Self {
            mem: MemoryMap::new(cart),
            cpu: Cpu::new(),
            ppu: Ppu::new(),
        }
    }

    pub fn next_frame(&mut self) -> FrameSequence<'_> {
        FrameSequence::new(self)
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

    /// This method is only called by the step sequence when it gets ticked. This represents a
    /// clock cycle (not a machine cycle). This involves ticking the memory and PPU.
    fn tick(&mut self) {
        self.mem.tick();
        self.ppu.tick(&mut self.mem);
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

    pub fn is_complete(&self) -> bool { self.counter >= self.op.length(&self.gb.cpu)
    }

    /// Consumes the step processor, dropping it immediately.
    pub fn complete(self) {}
}

impl Drop for StepProcess<'_> {
    fn drop(&mut self) {
        (self.counter..self.op.length(&self.gb.cpu)).for_each(|_| self.tick());
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

    pub fn frame_step(&'a mut self) -> StartUpFrame<'a> {
        StartUpFrame::new(self)
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
        (self.counter..self.op.length(&self.gb.cpu)).for_each(|_| self.tick());
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
            self.tick()
        }
        self.gb.mem.start_up_unmap(self.remap_mem)
    }
}

pub struct StartUpFrame<'a> {
    seq: &'a mut StartUpSequence<'a>,
    mode: PpuMode,
}

impl<'a> StartUpFrame<'a> {
    fn new(seq: &'a mut StartUpSequence<'a>) -> Self {
        let mode = seq.gb().ppu.state();
        Self { seq, mode }
    }

    fn tick(&mut self) {
        if !self.is_complete() {
            self.seq.step();
            self.mode = self.seq.gb.ppu.state();
        }
    }

    fn is_complete(&self) -> bool {
        self.mode == PpuMode::VBlank && self.seq.gb().ppu.state() == PpuMode::OamScan
    }

    fn complete(self) {}

    fn step(&mut self) {
        if !self.is_complete() {
            self.seq.step();
            self.mode = self.seq.gb().ppu.state();
        }
    }
}

impl<'a> Drop for StartUpFrame<'a> {
    fn drop(&mut self) {
        while !self.is_complete() {
            self.step()
        }
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
        self.next.tick();
        if !self.is_complete() {
            self.mode = self.next.gb.ppu.state();
        }
    }

    fn is_complete(&self) -> bool {
        self.mode == PpuMode::VBlank && self.next.gb.ppu.state() == PpuMode::OamScan
    }

    fn complete(self) {}

    fn step(&mut self) {
        if !self.is_complete() {
            self.next.step_and_next();
            self.mode = self.next.gb.ppu.state();
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
    use std::mem::size_of;

    use crate::lookup::Instruction;

    #[test]
    fn size() {
        assert!(size_of::<Instruction>() <= size_of::<usize>())
    }
}
