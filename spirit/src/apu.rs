#![allow(unused)]
// FIXME: Unused code is being allowed here only because this is *very* much under construction.

use crate::cpu::check_bit;
use crate::cpu::check_bit_const;
use crate::mem::MemoryMap;
use crate::mem::SpeedMode;
use crate::mem::io::apu::AudioRegisters;

pub struct Apu {
    /// A counter that is ticked every time the DIV register's 4th (or 5th in double speed) bit goes
    /// from 1 to 0.
    apu_div: u8,
    /// The state of the DIV register during the last tick.
    last_div: u8,
    mixer: AudioMixer,
    amp: AudioAmplifier,
}

impl Apu {
    pub(crate) fn new() -> Self {
        todo!()
    }

    pub(crate) fn tick(&mut self, mem: &MemoryMap) {
        let bit = match mem.speed_mode {
            SpeedMode::Standard => 4,
            SpeedMode::Double => 5,
        };
        let new_div = mem.io().tac.divider_reg.0;
        if check_bit(bit, self.last_div) && !check_bit(bit, new_div) {
            self.apu_div += 1;
        }
        self.last_div = new_div;
        if self.apu_div.is_multiple_of(2) {
            self.sound_length();
            if self.apu_div.is_multiple_of(4) {
                self.channel_one_sweep();
                if self.apu_div.is_multiple_of(8) {
                    self.envelope_sweep();
                    self.apu_div = 0;
                }
            }
        }
        todo!()
    }

    fn sound_length(&mut self) {}

    fn channel_one_sweep(&mut self) {}

    fn envelope_sweep(&mut self) {}
}

impl Default for Apu {
    fn default() -> Self {
        Self::new()
    }
}

struct AudioMixer {}

struct AudioAmplifier {}

pub struct Envelope {}

pub enum PulseWidthSetting {}

pub struct PulseWidthChannel {}

impl PulseWidthChannel {
    fn trigger(&mut self) {
        todo!()
    }
}

pub struct WaveChannel {
    env: Envelope,
}

impl WaveChannel {
    /// The channel can be enabled to automatically turn off. When it does, a counter is ticked up
    /// from a starting value (set by a register) until it reaches this value.
    const LENGTH: usize = 64;

    fn trigger(&mut self) {
        todo!()
    }
}

pub struct NoiseChannel {}

impl NoiseChannel {
    /// The channel can be enabled to automatically turn off. When it does, a counter is ticked up
    /// from a starting value (set by a register) until it reaches this value.
    const LENGTH: usize = 256;

    fn trigger(&mut self) {
        todo!()
    }
}

pub struct Mixer {}

pub struct Amplifier {}

// TODO: Will probably be a trait
pub struct Output {}
