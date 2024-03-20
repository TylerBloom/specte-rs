// TODO:
// Does it make sense to implement `Iterator`, `Stream`, or even `Future` for the gameboy?
// Conceptually, it maps fairly well... TBD

use crate::{cpu::Cpu, mbc::MemoryBankController};

/// This is the core emulation primative. It contains the entire state machine of the emulated
/// handheld and is agnostic to usecase and how it is rendered (if at all). Notably, the `Gameboy`
/// does not provide a `run` or analogous method. It must be ticked forward.
///
/// This allows managing tick rate, processing IO, and more to be done externally.
pub struct Gameboy {
    cpu: Cpu,
    mem: MemoryBankController,
}
