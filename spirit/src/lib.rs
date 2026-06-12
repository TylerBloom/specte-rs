//! Spirit is the core crate for the specters project. Contained here is the all of the logic for
//! creating a gameboy emulator and running it with a game ROM. This crate aims to be platform
//! agnostic and free of the UI-specifics. Other projects will wrap this logic in their own ways.
//! For example, the handheld device will have different IO than a desktop. Moreover, the syncing
//! webservice will not need a UI per se.

// TODO: The `tick` methods on most of the processes is incorrect, but the step methods are fine.

use std::ops::Deref;
use std::ops::DerefMut;

use serde::Deserialize;
use serde::Serialize;

use cpu::Cpu;
use cpu::CpuState;
use instruction::*;
use mem::MemoryLike;
use mem::MemoryMap;
use mem::StartUpHeaders;
use mem::vram::PpuMode;
use ppu::Ppu;
// use tracing::info;

pub mod apu;
pub mod cpu;
pub mod instruction;
pub mod mem;
pub mod ppu;
pub mod rom;
#[doc(hidden)]
pub mod utils;

pub use instruction::lookup;

/// Represents a Gameboy color with a cartridge inserted.
#[derive(Debug, Clone, Hash, Serialize, Deserialize)]
pub struct Gameboy {
    pub mem: MemoryMap,
    pub ppu: Ppu,
    cpu: Cpu,
}

impl Gameboy {
    /// Takes data that represents the data stored on a game cartridge and uses it to construct the
    pub fn load_cartridge(cart: Vec<u8>) -> StartUpSequence {
        StartUpSequence::new(Self {
            mem: MemoryMap::new(cart),
            cpu: Cpu::new(),
            ppu: Ppu::new(),
        })
    }

    pub fn next_frame(&mut self) {
        let mut mode = self.ppu.state();
        while !matches!(
            (mode, self.ppu.state()),
            (PpuMode::VBlank, PpuMode::OamScan)
        ) {
            mode = self.ppu.state();
            self.step();
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
    pub fn op_iter(&self) -> impl Iterator<Item = (u16, Instruction)> {
        let loaded_pc = self.cpu.pc.0.saturating_sub(1);
        let loaded_op = self.read_op();

        let ime = self.cpu.ime.current;
        let mut pc = loaded_pc + loaded_op.size() as u16;

        let mut stop = false;
        std::iter::once((loaded_pc, loaded_op)).chain(std::iter::from_fn(move || {
            if stop {
                return None;
            }
            let op = self.mem.read_op(pc, ime);
            let ret_pc = pc;
            match op {
                // TODO: Either commit to this all of the way or don't. The core issue here is that
                // some data might be read and is not meant to be an instruction. Panic catching is
                // also an option here.
                Instruction::Jump(JumpOp::Absolute) => {
                    let hi = self.mem.read_byte(pc + 1);
                    let lo = self.mem.read_byte(pc + 2);
                    pc = u16::from_be_bytes([hi, lo]);
                }
                Instruction::Stopped | Instruction::Unused => stop = true,
                _ => {}
            }
            pc += op.size() as u16;
            Some((ret_pc, op))
        }))
    }

    /// This returns a state that will track the number of required clock ticks it takes to
    /// complete the next operation. If the state is dropped before the required number of ticks,
    /// the intruction is automatically ran.
    pub fn step(&mut self) {
        let op = self.read_op();
        let state = GameboyState {
            mem: &mut self.mem,
            ppu: &mut self.ppu,
            cpu: &mut self.cpu,
        };
        op.execute(state);
    }

    pub fn read_op(&self) -> Instruction {
        match (
            self.mem
                .vram_dma
                .get_op(self.mem.io.lcd_y, self.mem.vram.status),
            self.mem.check_interrupt(),
        ) {
            (Some(op), _) => op,
            (None, Some(op)) if self.cpu.ime.current => Instruction::Interrupt(op),
            _ => self.cpu.read_op(),
        }
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

#[must_use = "Start up sequence is lazy and should be ticked. Dropping this object to complete the startup sequence instantly."]
pub struct StartUpSequence {
    gb: Gameboy,
    /// The memory that the ROM contains that get mapped over during the startup process.
    remap_mem: Option<Box<StartUpHeaders>>,
}

impl StartUpSequence {
    fn new(mut gb: Gameboy) -> Self {
        let remap_mem = Some(Box::new(gb.mem.start_up_remap()));
        Self { gb, remap_mem }
    }

    /// Consumes the start-up processor, dropping it immediately.
    pub fn complete(mut self) -> Gameboy {
        while !self.is_complete() {
            self.step();
        }
        self.unmap();
        self.gb
    }

    pub fn next_frame(&mut self) {
        let mut mode = self.ppu.state();
        while !matches!(
            (mode, self.ppu.state()),
            (PpuMode::VBlank, PpuMode::OamScan)
        ) {
            if self.is_complete() {
                self.unmap();
            }
            mode = self.ppu.state();
            self.step();
        }
    }

    fn unmap(&mut self) {
        if let Some(mem) = self.remap_mem.take() {
            self.gb.mem.start_up_unmap(*mem);
        }
    }

    pub fn is_complete(&self) -> bool {
        self.mem.io().boot_status == 0x11
    }
}

impl Deref for StartUpSequence {
    type Target = Gameboy;

    fn deref(&self) -> &Self::Target {
        &self.gb
    }
}

impl DerefMut for StartUpSequence {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.gb
    }
}

pub(crate) struct GameboyState<'a, M = MemoryMap>
where
    M: MemoryLike,
{
    pub(crate) mem: &'a mut M,
    pub(crate) ppu: &'a mut Ppu,
    pub(crate) cpu: &'a mut Cpu,
}

impl<M: MemoryLike> GameboyState<'_, M> {
    pub(crate) fn tick(&mut self, cycle: MCycle) {
        self.cpu.execute(cycle, self.mem);
        self.mem.tick(self.ppu);
    }

    /// In the CGB, there were two speed modes. The double speed mode is triggered by writing to
    /// the KEY1 register and running a STOP instruction. This switch take a total of 2050 machine
    /// cycles.
    ///
    /// Once in double speed mode, CPU operations, Timer/Divider register ticks, Serial port
    /// updates, and OAM DMA transfers take half as much time to perform. However, the LCD video
    /// controller (including the PPU), HDMA VRAM transfers, and all sound effects are unaffected.
    ///
    /// To model this, the unaffected components will be ticked twice rather than four times while
    /// in this mode.
    pub fn switch_speeds(&mut self) {
        // TODO: Does the PPU get advanced during this time?? Yes?
        self.mem.switch_speeds();
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum ButtonInput {
    Joypad(JoypadInput),
    Ssab(SsabInput),
}

#[cfg_attr(test, derive(strum_macros::EnumIter))]
#[derive(Debug, PartialEq, Clone, Copy)]
#[repr(u8)]
pub enum JoypadInput {
    Right = 0x1,
    Left = 0x2,
    Up = 0x4,
    Down = 0x8,
}

#[cfg_attr(test, derive(strum_macros::EnumIter))]
#[derive(Debug, PartialEq, Clone, Copy)]
#[repr(u8)]
pub enum SsabInput {
    A = 0x1,
    B = 0x2,
    Select = 0x4,
    Start = 0x8,
}

// TODO: Write tests for
//  - Double speed mode

#[cfg(test)]
mod tests {
    use crate::Gameboy;

    #[test]
    fn start_up_completion() {
        Gameboy::load_cartridge(include_bytes!("../tests/roms/acid/which.gb").into()).complete();
    }
}
