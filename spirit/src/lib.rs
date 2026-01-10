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
use mem::MemoryLikeExt;
use mem::MemoryMap;
use mem::StartUpHeaders;
use mem::vram::PpuMode;
use ppu::Ppu;
// use tracing::info;

pub mod apu;
pub mod cpu;
pub mod instruction;
pub mod lookup;
pub mod mem;
pub mod ppu;
pub mod rom;
#[doc(hidden)]
pub mod utils;

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
        let mut pc = self.cpu.pc.0.saturating_sub(1);
        let orig_pc = pc;
        let ime = self.cpu.ime;
        let op = self.mem.read_op(pc, ime);
        pc += op.size() as u16;
        let mut stop = false;
        std::iter::once((orig_pc, op)).chain(std::iter::from_fn(move || {
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
            cycle_count: 0,
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
            (None, Some(op)) if self.cpu.ime => op,
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
    M: MemoryLikeExt,
{
    pub(crate) mem: &'a mut M,
    pub(crate) ppu: &'a mut Ppu,
    pub(crate) cpu: &'a mut Cpu,
    #[cfg(debug_assertions)]
    /// Used during testing to ensure the number of cycles executed per instruction is correct
    pub(crate) cycle_count: usize,
}

impl<M: MemoryLikeExt> GameboyState<'_, M> {
    pub(crate) fn tick(&mut self, cycle: MCycle) {
        self.cpu.execute(cycle, self.mem);
        self.mem.tick(self.ppu);
        #[cfg(debug_assertions)]
        {
            self.cycle_count += 1;
        }
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

#[cfg(test)]
mod tests {
    use crate::Gameboy;

    #[test]
    fn start_up_completion() {
        Gameboy::load_cartridge(include_bytes!("../tests/roms/acid/which.gb").into()).complete();
    }
}
