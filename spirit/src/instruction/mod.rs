use crate::cpu::Cpu;

use derive_more::From;
use derive_more::IsVariant;

mod arithmetic;
mod bit;
mod bit_shift;
mod control;
mod interrupt;
mod jump;
mod load;

pub use arithmetic::*;
pub use bit::*;
pub use bit_shift::*;
pub use control::*;
pub use interrupt::*;
pub use jump::*;
pub use load::*;

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, derive_more::Display)]
#[display("{_variant}")]
pub enum Instruction {
    #[display("{_0}")]
    Load(LoadOp),
    #[display("{_0}")]
    BitShift(BitShiftOp),
    #[display("{_0}")]
    ControlOp(ControlOp),
    #[display("{_0}")]
    Bit(BitOp),
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
    /// This is not a real instruction, but its used to communicate that the VRAM DMA is
    /// transferring data. One of these operations represents 16 bytes being transferred.
    Transfer,
}

impl Instruction {
    /// Returns the number of ticks to will take to complete this instruction.
    /// Takes a reference to the CPU in order to determine if this instruction will pass any
    /// conditions.
    pub fn length(&self, cpu: &Cpu) -> u8 {
        match self {
            Instruction::Load(op) => op.length(),
            Instruction::BitShift(op) => op.length(),
            Instruction::ControlOp(op) => op.length(),
            Instruction::Bit(op) => op.length(),
            Instruction::Jump(op) => op.length(cpu),
            Instruction::Arithmetic(op) => op.length(),
            Instruction::Interrupt(_) => 20, // Interrupts are, basically, calls but last 5 cycles
            Instruction::Daa => 4,
            Instruction::Scf => 4,
            Instruction::Cpl => 4,
            Instruction::Ccf => 4,
            Instruction::Di => 4,
            Instruction::Ei => 4,
            // It takes 24 ticks to transfer 16 bytes.
            Instruction::Transfer => 24,
        }
    }

    /// Returns the size of the bytes to took to construct this instruction
    pub const fn size(&self) -> u8 {
        match self {
            Instruction::Load(op) => op.size(),
            Instruction::BitShift(op) => op.size(),
            Instruction::ControlOp(op) => op.size(),
            Instruction::Bit(op) => op.size(),
            Instruction::Jump(op) => op.size(),
            Instruction::Arithmetic(op) => op.size(),
            Instruction::Interrupt(_) => 0, // Interrupts don't move the PC
            Instruction::Daa => 1,
            Instruction::Scf => 1,
            Instruction::Cpl => 1,
            Instruction::Ccf => 1,
            Instruction::Di => 1,
            Instruction::Ei => 1,
            // This is a fake instruction and it should not move the PC
            Instruction::Transfer => 0,
        }
    }
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, derive_more::From, derive_more::Display)]
#[display("{_variant}")]
pub enum SomeByte {
    #[display("{_0}")]
    Referenced(RegOrPointer),
    #[display("0x{_0:0>2X}")]
    Direct(u8),
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
