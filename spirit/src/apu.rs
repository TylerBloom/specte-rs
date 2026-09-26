#![allow(unused)]
// FIXME: Unused code is being allowed here only because this is *very* much under construction.

use crate::mem::io::apu::AudioRegisters;

pub struct Apu {
    mixer: AudioMixer,
    amp: AudioAmplifier,
}

impl Apu {
    pub(crate) fn new() -> Self {
        todo!()
    }

    pub(crate) fn tick(&mut self, reg: &AudioRegisters) {
        todo!()
    }
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
