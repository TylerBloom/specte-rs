pub struct Apu {}

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
