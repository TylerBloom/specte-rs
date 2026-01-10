use crate::GameboyState;
use crate::cpu::Cpu;
use crate::cpu::CpuState;
use crate::cpu::FullRegister;
use crate::lookup::parse_prefixed_instruction;
use crate::mem::MemoryLikeExt;

use derive_more::From;
use derive_more::IsVariant;

mod arithmetic;
mod bit;
mod bit_shift;
mod control;
mod interrupt;
mod jump;
mod load;
mod prefixed;

pub use arithmetic::*;
pub use bit::*;
pub use bit_shift::*;
pub use control::*;
pub use interrupt::*;
pub use jump::*;
pub use load::*;
pub use prefixed::*;
use tracing::info;

// Refactor plan:
// Implement the instruction execution via a series of "M Cycle" sub-instructions. This needs to
// ignore pre-fetching in the last cycle of each instruction.
//  - Ensure all of the CPU JSON tests continue to pass.
//  - Sure the test ROM tests continue to pass.
// Note that the goal here is identical behavior from before the refactor.
//
// When these changes are complete, they should be merged in with two follow up PRs.
// First, instructions should be constructable from a single byte (the op code). The instruction
// should then communicate via MCycles how to get any literals it uses. (And similarly for prefixed
// instructions).
// Using the cycles data from the JSON tests to further verify that this is working correctly.
//
// The ultimate goal is the follow flow:
//  - CPU reads the current state of the IR and constructs an operations
//  - The operation then constructs ticks and passes them into the "Gameboy state" to be processed

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, derive_more::Display)]
#[display("{_variant}")]
pub enum Instruction {
    #[display("{_0}")]
    Load(LoadOp),
    #[display("{_0}")]
    ControlOp(ControlOp),
    #[display("{_0}")]
    Jump(JumpOp),
    #[display("{_0}")]
    Arithmetic(ArithmeticOp),
    #[display("Interrupt {_0}")]
    Interrupt(InterruptOp),
    #[display("DAA")]
    Daa,
    /// Set Carry.
    #[display("SCF")]
    Scf,
    /// ComPLement accumulator.
    #[display("CPL")]
    Cpl,
    /// CompLement carry flag.
    #[display("CCF")]
    Ccf,
    /// Disable interupts
    #[display("DI")]
    Di,
    /// Enable interupts
    #[display("EI")]
    Ei,
    /// The RLA, RLCA, RRA, RRCA are, in a sense, bit shift operations. However, they are the only
    /// shifting ops that are not prefixed, so they are classified as misc.
    #[display("RLA")]
    Rla,
    #[display("RLCA")]
    Rlca,
    #[display("RRA")]
    Rra,
    #[display("RRCA")]
    Rrca,
    /// This is not a real instruction, but its used to communicate that the VRAM DMA is
    /// transferring data. One of these operations represents 16 bytes being transferred.
    Transfer,
    /// This is not a real operation. Rather is executed while halted, waiting for an interrupt.
    Stall,
    /// This is not a real operation. Rather is executed when the system is stopped. If executed,
    /// this is the only instruction that will be executed until the system resets.
    Stopped,
    /// Load the next byte as an op code for a prefixed instruction
    Prefixed,
    /// Used for the handful of unused op codes
    Unused,
}

// TODO: Misc operations (messing with the IME) will also need to be implemented
// Alternatively, the operation can call some method directly. This prevents a check during
// every cycle.
// TODO: This is very premature optimization, but this type could be split into four types to
// prevent the unnecessary checks for IDU and ALU operations during every tick.
#[derive(Debug)]
pub struct MCycle {
    /// Signals which address will now live on the address bus.
    pub addr_bus: PointerReg,
    /// Signals what to do with the address that now lives on the address bus.
    pub action: AddrAction,
    /// Contains an optional signal to inc or dec the address on the address bus and then where to
    /// put the resulting value.
    pub idu: Option<(IduSignal, FullRegister)>,
    /// Contains an optional signal to the ALU about where to read data for a binary op and where
    /// to put the resulting data.
    pub alu: Option<AluSignal>,
}

impl MCycle {
    /// The end of every instruction is very similar. The PC is pushed onto the address bus, read
    /// from, and then incremented
    pub const fn final_cycle() -> Self {
        Self {
            addr_bus: PointerReg::PC,
            action: AddrAction::Read(ReadLocation::InstrRegister),
            idu: Some((IduSignal::Inc, FullRegister::PC)),
            alu: None,
        }
    }

    /// Used for cycles that are hand rolled. Note that this is *not* the cycle used by the NOOP
    /// instruction.
    pub const fn noop() -> Self {
        Self {
            // This is not actually used
            addr_bus: PointerReg::PC,
            action: AddrAction::Noop,
            idu: None,
            alu: None,
        }
    }

    /// Like the final cycle but with a concurrent ALU process.
    pub const fn final_with_alu(signal: AluSignal) -> Self {
        let mut cycle = Self::final_cycle();
        cycle.alu = Some(signal);
        cycle
    }

    pub const fn load_pointer() -> Self {
        Self {
            addr_bus: PointerReg::HL,
            action: AddrAction::Read(ReadLocation::Other(DataLocation::Bus)),
            idu: None,
            alu: None,
        }
    }

    pub const fn write_pointer() -> Self {
        Self {
            addr_bus: PointerReg::HL,
            action: AddrAction::Write(DataLocation::Bus),
            idu: None,
            alu: None,
        }
    }

    /// Reads byte pointed to by PC into the data bus and inc PC.
    pub const fn load_pc() -> Self {
        MCycle {
            addr_bus: PointerReg::PC,
            action: AddrAction::Read(ReadLocation::Other(DataLocation::Bus)),
            idu: Some((IduSignal::Inc, FullRegister::PC)),
            alu: None,
        }
    }
}

/// Communicates which 16 bit address is moved from the CPU onto the address bus, which register is
/// being moved needs to be communicated.
#[derive(Debug)]
pub enum PointerReg {
    PC,
    SP,
    HL,
    BC,
    DE,
    /// Use the "ghost registers" WZ as a pointer
    Ghost,
}

/// When the new address is put onto the address bus, either data is read from memory or writen
/// onto the data bus or a register. This communicates that.
#[derive(Debug)]
pub enum AddrAction {
    Read(ReadLocation),
    Write(DataLocation),
    Noop,
}

// FIXME: Fix these names... The instr register is also a data location but the ALU can't use it...

/// When data is read from memory, it can move to several locations. This signals where to put the
/// data.
///
/// NOTE: "Registers" Z and W are what are referred to (in this crate) as "ghost registers". For
/// single-byte operations that read data in, mess with it, and move it to an 8-bit register, this
/// can just be thought of as a byte living on the data bus. This idea breaks down when moving a
/// 2-byte datum around (e.g. loading a 16 bit literal into a wide register). For this, the "ghost"
/// wide register WZ is needed.
#[derive(Debug, derive_more::From)]
pub enum ReadLocation {
    InstrRegister,
    RegisterZ,
    RegisterW,
    Other(DataLocation),
}

/// When data is moved from the ALU (or from memory in many cases), there needs to be a single for
/// where it can go. This communicates those locations.
#[derive(Debug, Clone, Copy, derive_more::From)]
pub enum DataLocation {
    Bus,
    Register(HalfRegister),
    // TODO: This is a bit of a work around for cases like where SP is writen to/read from.
    Literal(u8),
}

/// The IDU (increment/decrement unit) can, well, either increment or decrement the address on the
/// address bus (independently from the ALU) and then put the resulting value back into a register.
#[derive(Debug)]
pub enum IduSignal {
    Inc,
    Dec,
    Noop,
}

/// Contains all the data for a signalled operation to the ALU.
#[derive(Debug)]
pub struct AluSignal {
    pub input_one: DataLocation,
    pub input_two: DataLocation,
    pub op: AluOp,
    pub output: DataLocation,
}

impl AluSignal {
    pub const fn move_into(src: DataLocation, dest: DataLocation) -> Self {
        // This is effectively a noop than a move
        Self {
            input_one: src,
            input_two: src,
            op: AluOp::Move,
            output: dest,
        }
    }

    pub const fn move_into_a(src: DataLocation) -> Self {
        Self::move_into(src, DataLocation::Register(HalfRegister::A))
    }

    const fn accumulator_op(src: DataLocation, op: AluOp) -> Self {
        Self {
            input_one: DataLocation::Register(HalfRegister::A),
            input_two: src,
            op,
            output: DataLocation::Register(HalfRegister::A),
        }
    }

    pub const fn add(src: DataLocation) -> Self {
        Self::accumulator_op(src, AluOp::Add)
    }

    pub const fn inc(reg: DataLocation) -> Self {
        Self {
            input_one: reg,
            input_two: DataLocation::Literal(1),
            op: AluOp::Add,
            output: reg,
        }
    }

    pub const fn adc(src: DataLocation) -> Self {
        Self::accumulator_op(src, AluOp::Adc)
    }

    pub const fn sub(src: DataLocation) -> Self {
        Self::accumulator_op(src, AluOp::Subtract)
    }

    pub const fn dec(reg: DataLocation) -> Self {
        Self {
            input_one: reg,
            input_two: DataLocation::Literal(1),
            op: AluOp::Subtract,
            output: reg,
        }
    }

    pub const fn sbc(src: DataLocation) -> Self {
        Self::accumulator_op(src, AluOp::Sbc)
    }

    pub const fn and(src: DataLocation) -> Self {
        Self::accumulator_op(src, AluOp::And)
    }

    pub const fn xor(src: DataLocation) -> Self {
        Self::accumulator_op(src, AluOp::Xor)
    }

    pub const fn or(src: DataLocation) -> Self {
        Self::accumulator_op(src, AluOp::Or)
    }

    pub const fn cp(src: DataLocation) -> Self {
        Self::accumulator_op(src, AluOp::Cp)
    }

    pub fn daa() -> Self {
        Self::accumulator_op(HalfRegister::A.into(), AluOp::Daa)
    }

    pub fn rla() -> Self {
        Self::accumulator_op(HalfRegister::A.into(), AluOp::Rla)
    }

    pub fn rlca() -> Self {
        Self::accumulator_op(HalfRegister::A.into(), AluOp::Rlca)
    }

    pub fn rra() -> Self {
        Self::accumulator_op(HalfRegister::A.into(), AluOp::Rra)
    }

    pub fn rrca() -> Self {
        Self::accumulator_op(HalfRegister::A.into(), AluOp::Rrca)
    }

    const fn bit_op(src: DataLocation, op: AluOp) -> Self {
        Self {
            input_one: src,
            input_two: src,
            op,
            output: src,
        }
    }

    pub fn bit(bit: u8, src: impl Into<DataLocation>) -> Self {
        debug_assert!(bit < 8);
        Self::bit_op(src.into(), AluOp::Bit(bit))
    }

    pub fn res(bit: u8, src: impl Into<DataLocation>) -> Self {
        debug_assert!(bit < 8);
        Self::bit_op(src.into(), AluOp::Res(bit))
    }

    pub fn set(bit: u8, src: impl Into<DataLocation>) -> Self {
        debug_assert!(bit < 8);
        Self::bit_op(src.into(), AluOp::Set(bit))
    }
}

/// When an operation is signalled to the ALU, the ALU need to know what binary operation to
/// perform. This enumerates those operations.
#[derive(Debug)]
pub enum AluOp {
    Add,
    SignedAdd,
    Subtract,
    And,
    Or,
    Xor,
    Move,
    Adc,
    Sbc,
    Cp,
    Bit(u8),
    Res(u8),
    Set(u8),
    Rl,
    Rlc,
    Rr,
    Rrc,
    Sla,
    Sra,
    Swap,
    Srl,
    Daa,
    Rla,
    Rlca,
    Rra,
    Rrca,
}

impl Instruction {
    pub(crate) fn execute<M: MemoryLikeExt>(self, mut state: GameboyState<'_, M>) {
        info!("Executing {self}");
        #[cfg(debug_assertions)]
        let length = self.length(&state) as usize / 4;
        match self {
            Instruction::Load(load_op) => load_op.execute(&mut state),
            Instruction::ControlOp(control_op) => control_op.execute(&mut state),
            Instruction::Jump(jump_op) => jump_op.execute(&mut state),
            Instruction::Arithmetic(arithmetic_op) => arithmetic_op.execute(&mut state),
            Instruction::Interrupt(interrupt_op) => interrupt_op.execute(&mut state),
            Instruction::Daa => {
                state.tick(MCycle::final_with_alu(AluSignal::daa()));
            }
            Instruction::Scf => {
                state.tick(MCycle::final_cycle());
                state.cpu.f.n = false;
                state.cpu.f.h = false;
                state.cpu.f.c = true;
            }
            Instruction::Cpl => {
                state.tick(MCycle::final_cycle());
                let a = state.cpu.a.0;
                state.cpu.a = (!a).into();
                state.cpu.f.n = true;
                state.cpu.f.h = true;
            }
            Instruction::Ccf => {
                state.tick(MCycle::final_cycle());
                state.cpu.f.n = false;
                state.cpu.f.h = false;
                let c = state.cpu.f.c;
                state.cpu.f.c = !c;
            }
            Instruction::Rla => state.tick(MCycle::final_with_alu(AluSignal::rla())),
            Instruction::Rlca => state.tick(MCycle::final_with_alu(AluSignal::rlca())),
            Instruction::Rra => state.tick(MCycle::final_with_alu(AluSignal::rra())),
            Instruction::Rrca => state.tick(MCycle::final_with_alu(AluSignal::rrca())),
            Instruction::Di => {
                state.tick(MCycle::final_cycle());
                state.cpu.to_set_ime = false;
                state.cpu.ime = false;
            }
            Instruction::Ei => {
                state.tick(MCycle::final_cycle());
                state.cpu.to_set_ime = true;
            }
            Instruction::Prefixed => {
                state.tick(MCycle::final_cycle());
                let op = state.cpu.read_prefixed_op();
                op.execute(&mut state);
            }
            Instruction::Stall => state.tick(MCycle {
                addr_bus: PointerReg::PC,
                action: AddrAction::Noop,
                idu: None,
                alu: None,
            }),
            Instruction::Stopped => state.tick(MCycle {
                addr_bus: PointerReg::PC,
                action: AddrAction::Noop,
                idu: None,
                alu: None,
            }),
            Instruction::Transfer => {
                if matches!(state.cpu.state, CpuState::Running) {
                    state.tick(MCycle::noop());
                    state.tick(MCycle::noop());
                    state.tick(MCycle::noop());
                    state.tick(MCycle::noop());
                    state.tick(MCycle::noop());
                    state.tick(MCycle::noop());
                    state.mem.vram_transfer()
                }
            }
            Instruction::Unused => panic!("Attempted to execute an invalid operation code!!"),
        }
        debug_assert_eq!(state.cycle_count, length);
        state.cpu.ime |= state.cpu.to_set_ime;
        state.cpu.to_set_ime = false;
    }

    /// Returns the number of ticks to will take to complete this instruction.
    /// Takes a reference to the CPU in order to determine if this instruction will pass any
    /// conditions.
    fn length<M: MemoryLikeExt>(&self, state: &GameboyState<'_, M>) -> u8 {
        match self {
            Instruction::Load(op) => op.length(),
            Instruction::ControlOp(op) => op.length(),
            Instruction::Jump(op) => op.length(state.cpu),
            Instruction::Arithmetic(op) => op.length(),
            Instruction::Interrupt(_) => 20,
            Instruction::Daa => 4,
            Instruction::Scf => 4,
            Instruction::Cpl => 4,
            Instruction::Ccf => 4,
            Instruction::Di => 4,
            Instruction::Ei => 4,
            Instruction::Transfer => 24,
            Instruction::Unused => 0,
            Instruction::Prefixed => {
                let pc = state.cpu.pc;
                parse_prefixed_instruction(state.mem.read_byte(pc.0)).length()
            }
            Instruction::Rla => 4,
            Instruction::Rlca => 4,
            Instruction::Rra => 4,
            Instruction::Rrca => 4,
            Instruction::Stall => 1,
            Instruction::Stopped => 1,
        }
    }

    /// Returns the size of the bytes to took to construct this instruction
    pub const fn size(&self) -> u8 {
        match self {
            Instruction::Load(op) => op.size(),
            Instruction::ControlOp(op) => op.size(),
            Instruction::Jump(op) => op.size(),
            Instruction::Arithmetic(op) => op.size(),
            Instruction::Interrupt(_) => 0,
            Instruction::Daa => 1,
            Instruction::Scf => 1,
            Instruction::Cpl => 1,
            Instruction::Ccf => 1,
            Instruction::Di => 1,
            Instruction::Ei => 1,
            Instruction::Transfer => 0,
            Instruction::Unused => 0,
            Instruction::Prefixed => 2,
            Instruction::Rla => 1,
            Instruction::Rlca => 1,
            Instruction::Rra => 1,
            Instruction::Rrca => 1,
            Instruction::Stall => 1,
            Instruction::Stopped => 0,
        }
    }
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, derive_more::From, derive_more::Display)]
#[display("{_variant}")]
pub enum SomeByte {
    #[display("{_0}")]
    Referenced(RegOrPointer),
    #[display("")]
    Direct,
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, derive_more::Display)]
#[display("{_variant}")]
pub enum WideReg {
    #[display("BC")]
    BC,
    #[display("DE")]
    DE,
    #[display("HL")]
    HL,
    #[display("SP")]
    SP,
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, derive_more::Display)]
#[display("{_variant}")]
pub enum WideRegWithoutSP {
    #[display("BC")]
    BC,
    #[display("DE")]
    DE,
    #[display("HL")]
    HL,
    #[display("AF")]
    AF,
}

impl WideRegWithoutSP {
    pub fn split(self) -> (HalfRegister, HalfRegister) {
        match self {
            WideRegWithoutSP::BC => (HalfRegister::B, HalfRegister::C),
            WideRegWithoutSP::DE => (HalfRegister::D, HalfRegister::E),
            WideRegWithoutSP::HL => (HalfRegister::H, HalfRegister::L),
            WideRegWithoutSP::AF => (HalfRegister::A, HalfRegister::F),
        }
    }
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, derive_more::Display)]
#[display("{_variant}")]
pub enum Condition {
    #[display("Z")]
    Zero,
    #[display("NZ")]
    NotZero,
    #[display("C")]
    Carry,
    #[display("NC")]
    NotCarry,
}

impl Condition {
    pub fn passed(&self, cpu: &Cpu) -> bool {
        match self {
            Condition::Zero => cpu.zero_flag(),
            Condition::NotZero => !cpu.zero_flag(),
            Condition::Carry => cpu.carry_flag(),
            Condition::NotCarry => !cpu.carry_flag(),
        }
    }
}

/// There are special operations for loading into the A register, so it is easier to have a special
/// enum for the unique types of pointers they use.
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, derive_more::Display)]
#[display("{_variant}")]
pub enum LoadAPointer {
    /// Use the BC register
    #[display("BC")]
    BC,
    /// Use the DE register
    #[display("DE")]
    DE,
    /// Use the HL register and increment after performing the operation
    #[display("HL+")]
    Hli,
    /// Use the HL register and decrement after performing the operation
    #[display("HL-")]
    Hld,
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, derive_more::Display)]
#[display("{_variant}")]
pub enum HalfRegister {
    #[display("A")]
    A,
    #[display("F")]
    F,
    #[display("B")]
    B,
    #[display("C")]
    C,
    #[display("D")]
    D,
    #[display("E")]
    E,
    #[display("H")]
    H,
    #[display("L")]
    L,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, From, IsVariant, derive_more::Display)]
#[display("{_variant}")]
pub enum RegOrPointer {
    #[display("{_0}")]
    Reg(HalfRegister),
    #[display("Pointer")]
    Pointer,
}

pub enum InnerRegOrPointer {
    A,
    B,
    C,
    D,
    E,
    H,
    L,
    Pointer,
}

impl InnerRegOrPointer {
    pub const fn convert(self) -> RegOrPointer {
        match self {
            InnerRegOrPointer::A => RegOrPointer::Reg(HalfRegister::A),
            InnerRegOrPointer::B => RegOrPointer::Reg(HalfRegister::B),
            InnerRegOrPointer::C => RegOrPointer::Reg(HalfRegister::C),
            InnerRegOrPointer::D => RegOrPointer::Reg(HalfRegister::D),
            InnerRegOrPointer::E => RegOrPointer::Reg(HalfRegister::E),
            InnerRegOrPointer::H => RegOrPointer::Reg(HalfRegister::H),
            InnerRegOrPointer::L => RegOrPointer::Reg(HalfRegister::L),
            InnerRegOrPointer::Pointer => RegOrPointer::Pointer,
        }
    }
}
