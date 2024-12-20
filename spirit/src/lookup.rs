use array_concat::concat_arrays;

use crate::{cpu::Cpu, mem::MemoryMap};

use derive_more::{From, IsVariant};

pub fn parse_instruction(mem: &MemoryMap, pc: u16) -> Instruction {
    OP_LOOKUP[mem.read_byte(pc) as usize](mem, pc)
}

fn parse_prefixed_instruction(mem: &MemoryMap, pc: u16) -> Instruction {
    PREFIXED_OP_LOOKUP[mem.read_byte(pc) as usize](mem, pc)
}

type OpArray<const N: usize> = [fn(&MemoryMap, u16) -> Instruction; N];

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
    #[display("{_0}")]
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
            Instruction::Interrupt(_) => 5, // Interrupts are, basically, calls but last 5 cycles
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

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, derive_more::Display)]
#[display("{_variant}")]
#[repr(u16)]
pub enum InterruptOp {
    #[display("VBlank")]
    VBlank = 0x0040,
    #[display("LCD")]
    LCD = 0x0048,
    #[display("Timer")]
    Timer = 0x0050,
    #[display("Serial")]
    Serial = 0x0058,
    #[display("Joypad")]
    Joypad = 0x0060,
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
pub enum ArithmeticOp {
    #[display("ADD {_0}")]
    Add(SomeByte),
    #[display("ADD HL, {_0}")]
    Add16(WideReg),
    #[display("ADD SP, {_0}")]
    AddSP(i8),
    #[display("ADC {_0}")]
    Adc(SomeByte),
    #[display("SUB {_0}")]
    Sub(SomeByte),
    #[display("SBC {_0}")]
    Sbc(SomeByte),
    #[display("AND {_0}")]
    And(SomeByte),
    #[display("XOR {_0}")]
    Xor(SomeByte),
    #[display("OR {_0}")]
    Or(SomeByte),
    #[display("CP {_0}")]
    Cp(SomeByte),
    #[display("INC {_0}")]
    Inc(RegOrPointer),
    #[display("INC {_0}")]
    Inc16(WideReg),
    #[display("DEC {_0}")]
    Dec(RegOrPointer),
    #[display("DEC {_0}")]
    Dec16(WideReg),
}

impl ArithmeticOp {
    /// Returns the number of ticks to will take to complete this instruction.
    pub fn length(&self) -> u8 {
        match self {
            ArithmeticOp::Add16(_) => 8,
            ArithmeticOp::Add(SomeByte::Referenced(RegOrPointer::Reg(_))) => 4,
            ArithmeticOp::Add(_) => 8,
            ArithmeticOp::Adc(SomeByte::Referenced(RegOrPointer::Reg(_))) => 4,
            ArithmeticOp::Adc(_) => 8,
            ArithmeticOp::Sub(SomeByte::Referenced(RegOrPointer::Reg(_))) => 4,
            ArithmeticOp::Sub(_) => 8,
            ArithmeticOp::Sbc(SomeByte::Referenced(RegOrPointer::Reg(_))) => 4,
            ArithmeticOp::Sbc(_) => 8,
            ArithmeticOp::And(SomeByte::Referenced(RegOrPointer::Reg(_))) => 4,
            ArithmeticOp::And(_) => 8,
            ArithmeticOp::Xor(SomeByte::Referenced(RegOrPointer::Reg(_))) => 4,
            ArithmeticOp::Xor(_) => 8,
            ArithmeticOp::Or(SomeByte::Referenced(RegOrPointer::Reg(_))) => 4,
            ArithmeticOp::Or(_) => 8,
            ArithmeticOp::Cp(SomeByte::Referenced(RegOrPointer::Reg(_))) => 4,
            ArithmeticOp::Cp(_) => 8,
            ArithmeticOp::Inc(_) => 8,
            ArithmeticOp::Dec(_) => 8,
            ArithmeticOp::Inc16(_) => 8,
            ArithmeticOp::Dec16(_) => 8,
            ArithmeticOp::AddSP(_) => 16,
        }
    }

    /// Returns the size of the bytes to took to construct this instruction
    pub const fn size(&self) -> u8 {
        match self {
            ArithmeticOp::Add16(_) => 1,
            ArithmeticOp::Add(SomeByte::Direct(_)) => 2,
            ArithmeticOp::Add(_) => 1,
            ArithmeticOp::Adc(SomeByte::Direct(_)) => 2,
            ArithmeticOp::Adc(_) => 1,
            ArithmeticOp::Sub(SomeByte::Direct(_)) => 2,
            ArithmeticOp::Sub(_) => 1,
            ArithmeticOp::Sbc(SomeByte::Direct(_)) => 2,
            ArithmeticOp::Sbc(_) => 1,
            ArithmeticOp::And(SomeByte::Direct(_)) => 2,
            ArithmeticOp::And(_) => 1,
            ArithmeticOp::Xor(SomeByte::Direct(_)) => 2,
            ArithmeticOp::Xor(_) => 1,
            ArithmeticOp::Or(SomeByte::Direct(_)) => 2,
            ArithmeticOp::Or(_) => 1,
            ArithmeticOp::Cp(SomeByte::Direct(_)) => 2,
            ArithmeticOp::Cp(_) => 1,
            ArithmeticOp::Inc(_) => 1,
            ArithmeticOp::Dec(_) => 1,
            ArithmeticOp::Inc16(_) => 1,
            ArithmeticOp::Dec16(_) => 1,
            ArithmeticOp::AddSP(_) => 2,
        }
    }
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, derive_more::Display)]
#[display("{_variant}")]
pub enum JumpOp {
    /// Op Codes: 0x20, 0x30, 0x28, 0x38
    #[display("JR {_0}, 0x{_1:0>2X}")]
    ConditionalRelative(Condition, i8),
    /// Op Code: 0x18
    #[display("JR 0x{_0:0>2X}")]
    Relative(i8),
    /// Op Codes: 0xC2, 0xD2, 0xCA, 0xDA
    #[display("JP {_0}, 0x{_1:0>4X}")]
    ConditionalAbsolute(Condition, u16),
    /// Op Code: 0xC3
    #[display("JP 0x{_0:0>2X}")]
    Absolute(u16),
    /// Op Code: 0xE9
    #[display("JP HL")]
    JumpToHL,
    /// Op Code: 0xCD
    #[display("CALL 0x{_0:0>4X}")]
    Call(u16),
    /// Op Codes: 0xC4, 0xD4, 0xCC, 0xDC
    #[display("CALL {_0}, 0x{_1:0>4X}")]
    ConditionalCall(Condition, u16),
    /// Op Code: 0xC9
    #[display("RET")]
    Return,
    /// Op Codes: 0xC0, 0xD0, 0xC8, 0xD8
    #[display("RET {_0}")]
    ConditionalReturn(Condition),
    /// Op Code: 0xD9
    /// Return from the subroutine and enable intrupts
    #[display("RETI")]
    ReturnAndEnable,
    /// Op Code: 0xC7
    #[display("RST 0x00")]
    RST00,
    /// Op Code: 0xCF
    #[display("RST 0x08")]
    RST08,
    /// Op Code: 0xD7
    #[display("RST 0x10")]
    RST10,
    /// Op Code: 0xDF
    #[display("RST 0x18")]
    RST18,
    /// Op Code: 0xE7
    #[display("RST 0x20")]
    RST20,
    /// Op Code: 0xEF
    #[display("RST 0x28")]
    RST28,
    /// Op Code: 0xF7
    #[display("RST 0x30")]
    RST30,
    /// Op Code: 0xFF
    #[display("RST 0x38")]
    RST38,
}

impl JumpOp {
    /// Returns the number of ticks to will take to complete this instruction.
    pub fn length(&self, cpu: &Cpu) -> u8 {
        match self {
            JumpOp::ConditionalRelative(cond, _) => 8 + (4 * cond.passed(cpu) as u8),
            JumpOp::Relative(_) => 12,
            JumpOp::ConditionalAbsolute(cond, _) => 12 + (4 * cond.passed(cpu) as u8),
            JumpOp::Absolute(_) => 16,
            JumpOp::JumpToHL => 4,
            JumpOp::Call(_) => 24,
            JumpOp::ConditionalCall(cond, _) => 12 + (12 * cond.passed(cpu) as u8),
            JumpOp::Return => 16,
            JumpOp::ConditionalReturn(cond) => 8 + (12 * cond.passed(cpu) as u8),
            JumpOp::ReturnAndEnable => 16,
            JumpOp::RST00 => 16,
            JumpOp::RST08 => 16,
            JumpOp::RST10 => 16,
            JumpOp::RST18 => 16,
            JumpOp::RST20 => 16,
            JumpOp::RST28 => 16,
            JumpOp::RST30 => 16,
            JumpOp::RST38 => 16,
        }
    }

    /// Returns the size of the bytes to took to construct this instruction
    pub const fn size(&self) -> u8 {
        match self {
            JumpOp::ConditionalRelative(_, _) => 2,
            JumpOp::Relative(_) => 2,
            JumpOp::ConditionalAbsolute(_, _) => 3,
            JumpOp::Absolute(_) => 3,
            JumpOp::JumpToHL => 1,
            JumpOp::Call(_) => 3,
            JumpOp::ConditionalCall(_, _) => 3,
            JumpOp::Return => 1,
            JumpOp::ConditionalReturn(_) => 1,
            JumpOp::ReturnAndEnable => 1,
            JumpOp::RST00 => 1,
            JumpOp::RST08 => 1,
            JumpOp::RST10 => 1,
            JumpOp::RST18 => 1,
            JumpOp::RST20 => 1,
            JumpOp::RST28 => 1,
            JumpOp::RST30 => 1,
            JumpOp::RST38 => 1,
        }
    }
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, derive_more::Display)]
#[display("{_variant}")]
pub enum ControlOp {
    #[display("HALT")]
    Halt,
    #[display("NOOP")]
    Noop,
    #[display("Stop 0x{_0:0>2X}")]
    Stop(u8),
}

impl ControlOp {
    /// Returns the number of ticks to will take to complete this instruction.
    pub fn length(&self) -> u8 {
        match self {
            ControlOp::Noop => 4,
            ControlOp::Stop(_) => 4,
            ControlOp::Halt => 4,
        }
    }

    /// Returns the size of the bytes to took to construct this instruction
    pub const fn size(&self) -> u8 {
        match self {
            ControlOp::Noop => 1,
            ControlOp::Stop(_) => 2,
            ControlOp::Halt => 1,
        }
    }
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
pub enum LoadOp {
    /// Used for opcodes in 0x40..0x80
    #[display("LD {src} -> {dest}")]
    Basic {
        dest: RegOrPointer,
        src: RegOrPointer,
    },
    /// Used for opcodes 0xX1
    #[display("LD 0x{_1:0>4X} -> {_0}")]
    Direct16(WideReg, u16),
    /// Used for opcodes 0x_6 and 0x_E
    #[display("LD 0x{_1:0>2X} -> {_0}")]
    Direct(RegOrPointer, u8),
    /// Used for opcodes 0x_A
    #[display("LD {_0} -> A")]
    LoadIntoA(LoadAPointer),
    /// Used for opcodes 0x_2
    #[display("LD A -> {_0}")]
    StoreFromA(LoadAPointer),
    /// Opcode: 0x08
    /// Store SP & $FF at address n16 and SP >> 8 at address n16 + 1.
    #[display("LD SP -> 0x{_0:0>4X}")]
    StoreSP(u16),
    /// Opcode: 0xF9
    #[display("LD SP -> HL")]
    HLIntoSP,
    /// Opcode: 0xF8
    /// Add the signed value e8 to SP and store the result in HL.
    #[display("LD SP + {_0} -> HL")]
    SPIntoHL(i8),
    /// Used for opcodes 0x_1
    #[display("POP {_0}")]
    Pop(WideRegWithoutSP),
    /// Used for opcodes 0x_5
    #[display("PUSH {_0}")]
    Push(WideRegWithoutSP),
    /// Used for opcode 0xE0
    #[display("LDH A -> 0xFF{_0:0>2X}")]
    LoadHigh(u8),
    /// Used for opcode 0xF0
    #[display("LDH 0xFF{_0:0>2X} -> A")]
    StoreHigh(u8),
    /// Used for opcode 0xE2
    #[display("LDHCA")]
    Ldhca,
    /// Used for opcode 0xF2
    #[display("LDHAC")]
    Ldhac,
    /// Used for opcode 0xEA
    #[display("LD 0x{ptr:0>4X} -> A")]
    LoadA { ptr: u16 },
    /// Used for opcode 0xFA
    #[display("LD A -> 0x{ptr:0>4X}")]
    StoreA { ptr: u16 },
}

impl LoadOp {
    /// Returns the number of ticks to will take to complete this instruction.
    pub fn length(&self) -> u8 {
        match self {
            LoadOp::Basic {
                dest: RegOrPointer::Pointer,
                src: RegOrPointer::Pointer,
            } => 4,
            LoadOp::Basic {
                dest: RegOrPointer::Pointer,
                ..
            }
            | LoadOp::Basic {
                src: RegOrPointer::Pointer,
                ..
            } => 8,
            LoadOp::Basic { .. } => 4,
            LoadOp::Direct16(_, _) => 12,
            LoadOp::Direct(RegOrPointer::Pointer, _) => 12,
            LoadOp::Direct(_, _) => 8,
            LoadOp::LoadIntoA(_) => 8,
            LoadOp::StoreFromA(_) => 8,
            LoadOp::StoreSP(_) => 20,
            LoadOp::HLIntoSP => 8,
            LoadOp::SPIntoHL(_) => 12,
            LoadOp::Pop(_) => 12,
            LoadOp::Push(_) => 16,
            LoadOp::LoadHigh(_) => 12,
            LoadOp::StoreHigh(_) => 12,
            LoadOp::Ldhca => 8,
            LoadOp::Ldhac => 8,
            LoadOp::LoadA { .. } => 16,
            LoadOp::StoreA { .. } => 16,
        }
    }

    /// Returns the size of the bytes to took to construct this instruction
    pub const fn size(&self) -> u8 {
        match self {
            LoadOp::Basic { .. } => 1,
            LoadOp::Direct16(_, _) => 3,
            LoadOp::Direct(_, _) => 2,
            LoadOp::LoadIntoA(_) => 1,
            LoadOp::StoreFromA(_) => 1,
            LoadOp::StoreSP(_) => 3,
            LoadOp::HLIntoSP => 1,
            LoadOp::SPIntoHL(_) => 2,
            LoadOp::Pop(_) => 1,
            LoadOp::Push(_) => 1,
            LoadOp::LoadHigh(_) => 2,
            LoadOp::StoreHigh(_) => 2,
            LoadOp::Ldhca => 1,
            LoadOp::Ldhac => 1,
            LoadOp::LoadA { .. } => 3,
            LoadOp::StoreA { .. } => 3,
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
    fn passed(&self, cpu: &Cpu) -> bool {
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
pub enum BitShiftOp {
    #[display("RLC {_0}")]
    Rlc(RegOrPointer),
    #[display("RLCA")]
    Rlca,
    #[display("RRC {_0}")]
    Rrc(RegOrPointer),
    #[display("RRCA")]
    Rrca,
    #[display("RL {_0}")]
    Rl(RegOrPointer),
    #[display("RLA")]
    Rla,
    #[display("RR {_0}")]
    Rr(RegOrPointer),
    #[display("RRA")]
    Rra,
    #[display("SLA {_0}")]
    Sla(RegOrPointer),
    #[display("SRA {_0}")]
    Sra(RegOrPointer),
    #[display("SWAP {_0}")]
    Swap(RegOrPointer),
    #[display("SRL {_0}")]
    Srl(RegOrPointer),
}

impl BitShiftOp {
    /// Returns the number of ticks to will take to complete this instruction.
    pub fn length(&self) -> u8 {
        match self {
            BitShiftOp::Rlca => 4,
            BitShiftOp::Rlc(RegOrPointer::Pointer) => 16,
            BitShiftOp::Rlc(_) => 8,
            BitShiftOp::Rrca => 4,
            BitShiftOp::Rrc(RegOrPointer::Pointer) => 16,
            BitShiftOp::Rrc(_) => 8,
            BitShiftOp::Rl(RegOrPointer::Pointer) => 16,
            BitShiftOp::Rl(_) => 8,
            BitShiftOp::Rla => 4,
            BitShiftOp::Rr(RegOrPointer::Pointer) => 16,
            BitShiftOp::Rr(_) => 8,
            BitShiftOp::Rra => 4,
            BitShiftOp::Sla(RegOrPointer::Pointer) => 16,
            BitShiftOp::Sla(_) => 8,
            BitShiftOp::Sra(RegOrPointer::Pointer) => 16,
            BitShiftOp::Sra(_) => 8,
            BitShiftOp::Swap(RegOrPointer::Pointer) => 16,
            BitShiftOp::Swap(_) => 8,
            BitShiftOp::Srl(RegOrPointer::Pointer) => 16,
            BitShiftOp::Srl(_) => 8,
        }
    }

    /// Returns the size of the bytes to took to construct this instruction
    pub const fn size(&self) -> u8 {
        match self {
            BitShiftOp::Rlca => 1,
            BitShiftOp::Rlc(_) => 2,
            BitShiftOp::Rrca => 1,
            BitShiftOp::Rrc(_) => 2,
            BitShiftOp::Rla => 1,
            BitShiftOp::Rl(_) => 2,
            BitShiftOp::Rra => 1,
            BitShiftOp::Rr(_) => 2,
            BitShiftOp::Sla(_) => 2,
            BitShiftOp::Sra(_) => 2,
            BitShiftOp::Swap(_) => 2,
            BitShiftOp::Srl(_) => 2,
        }
    }
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

enum InnerRegOrPointer {
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
    const fn convert(self) -> RegOrPointer {
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

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, derive_more::Display)]
#[display("{op} {bit} {reg}")]
pub struct BitOp {
    pub bit: u8,
    pub reg: RegOrPointer,
    pub op: BitOpInner,
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, derive_more::Display)]
#[display("{_variant}")]
pub enum BitOpInner {
    #[display("BIT")]
    Bit,
    #[display("RES")]
    Res,
    #[display("SET")]
    Set,
}

impl BitOp {
    /// Returns the number of ticks to will take to complete this instruction.
    pub fn length(&self) -> u8 {
        8 + 4 * (self.reg.is_pointer() as u8)
    }

    /// Returns the size of the bytes to took to construct this instruction
    pub const fn size(&self) -> u8 {
        2
    }
}

macro_rules! define_op {
    () => {
        |data, pc| {
            unreachable!(
                "Op code '0x{:X}' does not correspond to any valid operation",
                data.read_byte(pc)
            )
        }
    };
    (DAA) => {
        |_, _| Instruction::Daa
    };
    (SCF) => {
        |_, _| Instruction::Scf
    };
    (CPL) => {
        |_, _| Instruction::Cpl
    };
    (CCF) => {
        |_, _| Instruction::Ccf
    };
    (NOOP) => {
        |_, _| Instruction::ControlOp(ControlOp::Noop)
    };
    (JR) => {
        |data, pc| Instruction::Jump(JumpOp::Relative(data.read_byte(pc + 1) as i8))
    };
    (JR, $r: ident) => {
        |data, pc| {
            Instruction::Jump(JumpOp::ConditionalRelative(
                Condition::$r,
                data.read_byte(pc + 1) as i8,
            ))
        }
    };
    (JP) => {
        |data, pc| {
            Instruction::Jump(JumpOp::Absolute(u16::from_le_bytes([
                data.read_byte(pc + 1),
                data.read_byte(pc + 2),
            ])))
        }
    };
    (JP, HL) => {
        |_, _| Instruction::Jump(JumpOp::JumpToHL)
    };
    (JP, $r: ident) => {
        |data, pc| {
            Instruction::Jump(JumpOp::ConditionalAbsolute(
                Condition::$r,
                u16::from_le_bytes([data.read_byte(pc + 1), data.read_byte(pc + 2)]),
            ))
        }
    };
    (STOP) => {
        |data, pc| Instruction::ControlOp(ControlOp::Stop(data.read_byte(pc + 1)))
    };
    (ADD) => {
        |data, pc| Instruction::Arithmetic(ArithmeticOp::Add(SomeByte::Direct(data.read_byte(pc + 1))))
    };
    (ADD, SP) => {
        |data, pc| Instruction::Arithmetic(ArithmeticOp::AddSP(data.read_byte(pc + 1) as i8))
    };
    (ADD, $r: ident) => {
        |_, _| Instruction::Arithmetic(ArithmeticOp::Add(InnerRegOrPointer::$r.convert().into()))
    };
    (ADC) => {
        |data, pc| Instruction::Arithmetic(ArithmeticOp::Adc(SomeByte::Direct(data.read_byte(pc + 1))))
    };
    (ADC, $r: ident) => {
        |_, _| Instruction::Arithmetic(ArithmeticOp::Adc(InnerRegOrPointer::$r.convert().into()))
    };
    (SUB) => {
        |data, pc| Instruction::Arithmetic(ArithmeticOp::Sub(SomeByte::Direct(data.read_byte(pc + 1))))
    };
    (SUB, $r: ident) => {
        |_, _| Instruction::Arithmetic(ArithmeticOp::Sub(InnerRegOrPointer::$r.convert().into()))
    };
    (SBC) => {
        |data, pc| Instruction::Arithmetic(ArithmeticOp::Sbc(SomeByte::Direct(data.read_byte(pc + 1))))
    };
    (SBC, $r: ident) => {
        |_, _| Instruction::Arithmetic(ArithmeticOp::Sbc(InnerRegOrPointer::$r.convert().into()))
    };
    (AND) => {
        |data, pc| Instruction::Arithmetic(ArithmeticOp::And(SomeByte::Direct(data.read_byte(pc + 1))))
    };
    (AND, $r: ident) => {
        |_, _| Instruction::Arithmetic(ArithmeticOp::And(InnerRegOrPointer::$r.convert().into()))
    };
    (XOR) => {
        |data, pc| Instruction::Arithmetic(ArithmeticOp::Xor(SomeByte::Direct(data.read_byte(pc + 1))))
    };
    (XOR, $r: ident) => {
        |_, _| Instruction::Arithmetic(ArithmeticOp::Xor(InnerRegOrPointer::$r.convert().into()))
    };
    (OR) => {
        |data, pc| Instruction::Arithmetic(ArithmeticOp::Or(SomeByte::Direct(data.read_byte(pc + 1))))
    };
    (OR, $r: ident) => {
        |_, _| Instruction::Arithmetic(ArithmeticOp::Or(InnerRegOrPointer::$r.convert().into()))
    };
    (CP) => {
        |data, pc| Instruction::Arithmetic(ArithmeticOp::Cp(SomeByte::Direct(data.read_byte(pc + 1))))
    };
    (CP, $r: ident) => {
        |_, _| Instruction::Arithmetic(ArithmeticOp::Cp(InnerRegOrPointer::$r.convert().into()))
    };
    (ADD16, $r: ident) => {
        |_, _| Instruction::Arithmetic(ArithmeticOp::Add16(WideReg::$r))
    };
    (INC, $r: ident) => {
        |_, _| Instruction::Arithmetic(ArithmeticOp::Inc(InnerRegOrPointer::$r.convert()))
    };
    (INC16, $r: ident) => {
        |_, _| Instruction::Arithmetic(ArithmeticOp::Inc16(WideReg::$r))
    };
    (DEC, $r: ident) => {
        |_, _| Instruction::Arithmetic(ArithmeticOp::Dec(InnerRegOrPointer::$r.convert()))
    };
    (DEC16, $r: ident) => {
        |_, _| Instruction::Arithmetic(ArithmeticOp::Dec16(WideReg::$r))
    };
    (LD, $r: ident, A) => {
        |_, _| Instruction::Load(LoadOp::StoreFromA(LoadAPointer::$r))
    };
    (LD SP) => {
        |data, pc| {
            Instruction::Load(LoadOp::StoreSP(u16::from_le_bytes([
                data.read_byte(pc + 1),
                data.read_byte(pc + 1),
            ])))
        }
    };
    (LoadA) => {
        |data, pc| {
            Instruction::Load(LoadOp::LoadA {
                ptr: u16::from_le_bytes([data.read_byte(pc + 1), data.read_byte(pc + 2)]),
            })
        }
    };
    (StoreA) => {
        |data, pc| {
            Instruction::Load(LoadOp::StoreA {
                ptr: u16::from_le_bytes([data.read_byte(pc + 1), data.read_byte(pc + 2)]),
            })
        }
    };
    (LD, $r: ident) => {
        |data, pc| {
            Instruction::Load(LoadOp::Direct(
                InnerRegOrPointer::$r.convert(),
                data.read_byte(pc + 1),
            ))
        }
    };
    (LD16, $r: ident) => {
        |data, pc| {
            Instruction::Load(LoadOp::Direct16(
                WideReg::$r,
                u16::from_le_bytes([data.read_byte(pc + 1), data.read_byte(pc + 2)]),
            ))
        }
    };
    (LD, A, $r: ident) => {
        |_, _| Instruction::Load(LoadOp::LoadIntoA(LoadAPointer::$r))
    };
    (LD, HL, SP) => {
        |data, pc| Instruction::Load(LoadOp::SPIntoHL(data.read_byte(pc + 1) as i8))
    };
    (LD, SP, HL) => {
        |_, _| Instruction::Load(LoadOp::HLIntoSP)
    };
    (LD, $r1: ident, $r2: ident,) => {
        |_, _| {
            Instruction::Load(LoadOp::Basic {
                dest: InnerRegOrPointer::$r1.convert(),
                src: InnerRegOrPointer::$r2.convert(),
            })
        }
    };
    (LD, Pointer, Pointer,) => {
        |_, _| Instruction::ControlOp(ControlOp::Halt)
    };
    (POP, $r: ident) => {
        |_, _| Instruction::Load(LoadOp::Pop(WideRegWithoutSP::$r))
    };
    (PUSH, $r: ident) => {
        |_, _| Instruction::Load(LoadOp::Push(WideRegWithoutSP::$r))
    };
    (RET) => {
        |_, _| Instruction::Jump(JumpOp::Return)
    };
    (RETI) => {
        |_, _| Instruction::Jump(JumpOp::ReturnAndEnable)
    };
    (RET, $r: ident) => {
        |_, _| Instruction::Jump(JumpOp::ConditionalReturn(Condition::$r))
    };
    (CALL) => {
        |data, pc| {
            Instruction::Jump(JumpOp::Call(u16::from_le_bytes([
                data.read_byte(pc + 1),
                data.read_byte(pc + 2),
            ])))
        }
    };
    (CALL, $r: ident) => {
        |data, pc| {
            Instruction::Jump(JumpOp::ConditionalCall(
                Condition::$r,
                u16::from_le_bytes([data.read_byte(pc + 1), data.read_byte(pc + 2)]),
            ))
        }
    };
    (PREFIX) => {
        |mem, pc| parse_prefixed_instruction(mem, pc + 1)
    };
    (DI) => {
        |_, _| Instruction::Di
    };
    (EI) => {
        |_, _| Instruction::Ei
    };
    (RST00) => {
        |_, _| Instruction::Jump(JumpOp::RST00)
    };
    (RST08) => {
        |_, _| Instruction::Jump(JumpOp::RST08)
    };
    (RST10) => {
        |_, _| Instruction::Jump(JumpOp::RST10)
    };
    (RST18) => {
        |_, _| Instruction::Jump(JumpOp::RST18)
    };
    (RST20) => {
        |_, _| Instruction::Jump(JumpOp::RST20)
    };
    (RST28) => {
        |_, _| Instruction::Jump(JumpOp::RST28)
    };
    (RST30) => {
        |_, _| Instruction::Jump(JumpOp::RST30)
    };
    (RST38) => {
        |_, _| Instruction::Jump(JumpOp::RST38)
    };
    (LoadHigh) => {
        |data, pc| Instruction::Load(LoadOp::LoadHigh(data.read_byte(pc + 1)))
    };
    (StoreHigh) => {
        |data, pc| Instruction::Load(LoadOp::StoreHigh(data.read_byte(pc + 1)))
    };
    (LDHCA) => {
        |_, _| Instruction::Load(LoadOp::Ldhca)
    };
    (LDHAC) => {
        |_, _| Instruction::Load(LoadOp::Ldhac)
    };
    /* --- Prefixed op definitions --- */
    (RLCA) => {
        |_, _| Instruction::BitShift(BitShiftOp::Rlca)
    };
    (RLA) => {
        |_, _| Instruction::BitShift(BitShiftOp::Rla)
    };
    (RL, $r: ident) => {
        |_, _| Instruction::BitShift(BitShiftOp::Rl(InnerRegOrPointer::$r.convert()))
    };
    (RLC, $r: ident) => {
        |_, _| Instruction::BitShift(BitShiftOp::Rlc(InnerRegOrPointer::$r.convert()))
    };
    (RRCA) => {
        |_, _| Instruction::BitShift(BitShiftOp::Rrca)
    };
    (RRA) => {
        |_, _| Instruction::BitShift(BitShiftOp::Rra)
    };
    (RRC, $r: ident) => {
        |_, _| Instruction::BitShift(BitShiftOp::Rrc(InnerRegOrPointer::$r.convert()))
    };
    (RR, $r: ident) => {
        |_, _| Instruction::BitShift(BitShiftOp::Rr(InnerRegOrPointer::$r.convert()))
    };
    (SLA, $r: ident) => {
        |_, _| Instruction::BitShift(BitShiftOp::Sla(InnerRegOrPointer::$r.convert()))
    };
    (SRA, $r: ident) => {
        |_, _| Instruction::BitShift(BitShiftOp::Sra(InnerRegOrPointer::$r.convert()))
    };
    (SWAP, $r: ident) => {
        |_, _| Instruction::BitShift(BitShiftOp::Swap(InnerRegOrPointer::$r.convert()))
    };
    (SRL, $r: ident) => {
        |_, _| Instruction::BitShift(BitShiftOp::Srl(InnerRegOrPointer::$r.convert()))
    };
    (BIT, $b: literal, $r: ident) => {
        |data, pc| {
            Instruction::Bit(BitOp {
                bit: (data.read_byte(pc) - 0x40) / 8,
                reg: InnerRegOrPointer::$r.convert(),
                op: BitOpInner::Bit,
            })
        }
    };
    (RES, $b: literal, $r: ident) => {
        |data, pc| {
            Instruction::Bit(BitOp {
                bit: (data.read_byte(pc) - 0x80) / 8,
                reg: InnerRegOrPointer::$r.convert(),
                op: BitOpInner::Res,
            })
        }
    };
    (SET, $b: literal, $r: ident) => {
        |data, pc| {
            Instruction::Bit(BitOp {
                bit: (data.read_byte(pc) - 0xC0) / 8,
                reg: InnerRegOrPointer::$r.convert(),
                op: BitOpInner::Set,
            })
        }
    };
}

macro_rules! define_op_chunk {
    (LD) => {{
        const OPS: OpArray<0x40> = concat_arrays!(
            define_op_chunk!(LD, B),
            define_op_chunk!(LD, C),
            define_op_chunk!(LD, D),
            define_op_chunk!(LD, E),
            define_op_chunk!(LD, H),
            define_op_chunk!(LD, L),
            define_op_chunk!(LD, Pointer),
            define_op_chunk!(LD, A)
        );
        OPS
    }};
    ($x: ident, NUM) => {{
        const OPS: OpArray<0x40> = concat_arrays!(
            define_op_chunk!($x, 0,),
            define_op_chunk!($x, 1,),
            define_op_chunk!($x, 2,),
            define_op_chunk!($x, 3,),
            define_op_chunk!($x, 4,),
            define_op_chunk!($x, 5,),
            define_op_chunk!($x, 6,),
            define_op_chunk!($x, 7,)
        );
        OPS
    }};
    ($x: ident, $i: literal,) => {{
        const OPS: OpArray<0x08> = [
            define_op!($x, $i, B),
            define_op!($x, $i, C),
            define_op!($x, $i, D),
            define_op!($x, $i, E),
            define_op!($x, $i, H),
            define_op!($x, $i, L),
            define_op!($x, $i, Pointer),
            define_op!($x, $i, A),
        ];
        OPS
    }};
    ($x: ident) => {{
        const OPS: OpArray<8> = [
            define_op!($x, B),
            define_op!($x, C),
            define_op!($x, D),
            define_op!($x, E),
            define_op!($x, H),
            define_op!($x, L),
            define_op!($x, Pointer),
            define_op!($x, A),
        ];
        OPS
    }};
    ($x: ident, $r: ident) => {{
        const OPS: OpArray<8> = [
            define_op!($x, $r, B,),
            define_op!($x, $r, C,),
            define_op!($x, $r, D,),
            define_op!($x, $r, E,),
            define_op!($x, $r, H,),
            define_op!($x, $r, L,),
            define_op!($x, $r, Pointer,),
            define_op!($x, $r, A,),
        ];
        OPS
    }};
}

macro_rules! define_op_lookup_table {
    () => {
        concat_arrays!(
            define_op_lookup_table!(CHUNK_ONE),
            define_op_lookup_table!(CHUNK_TWO),
            define_op_lookup_table!(CHUNK_THREE),
            define_op_lookup_table!(CHUNK_FOUR)
        )
    };
    (PREFIXED) => {
        concat_arrays!(
            define_op_chunk!(RLC),
            define_op_chunk!(RRC),
            define_op_chunk!(RL),
            define_op_chunk!(RR),
            define_op_chunk!(SLA),
            define_op_chunk!(SRA),
            define_op_chunk!(SWAP),
            define_op_chunk!(SRL),
            define_op_chunk!(BIT, NUM),
            define_op_chunk!(RES, NUM),
            define_op_chunk!(SET, NUM)
        )
    };
    // NOTE: It is planned to use a transposition method for the top and bottom rows-of-four of
    // the op table. That way, they all can be defined in a similar way (in rows),
    // transposed in columns, and concatinated.
    (CHUNK_ONE) => {{
        const TO_TRANSPOSED: [OpArray<4>; 0x10] = [
            [
                define_op!(NOOP),
                define_op!(STOP),
                define_op!(JR, NotZero),
                define_op!(JR, NotCarry),
            ],
            [
                define_op!(LD16, BC),
                define_op!(LD16, DE),
                define_op!(LD16, HL),
                define_op!(LD16, SP),
            ],
            [
                define_op!(LD, BC, A),
                define_op!(LD, DE, A),
                define_op!(LD, Hli, A),
                define_op!(LD, Hld, A),
            ],
            [
                define_op!(INC16, BC),
                define_op!(INC16, DE),
                define_op!(INC16, HL),
                define_op!(INC16, SP),
            ],
            [
                define_op!(INC, B),
                define_op!(INC, D),
                define_op!(INC, H),
                define_op!(INC, Pointer),
            ],
            [
                define_op!(DEC, B),
                define_op!(DEC, D),
                define_op!(DEC, H),
                define_op!(DEC, Pointer),
            ],
            [
                define_op!(LD, B),
                define_op!(LD, D),
                define_op!(LD, H),
                define_op!(LD, Pointer),
            ],
            [
                define_op!(RLCA),
                define_op!(RLA),
                define_op!(DAA),
                define_op!(SCF),
            ],
            [
            define_op!(LD SP),
                define_op!(JR),
                define_op!(JR, Zero),
                define_op!(JR, Carry),
            ],
            [
                define_op!(ADD16, BC),
                define_op!(ADD16, DE),
                define_op!(ADD16, HL),
                define_op!(ADD16, SP),
            ],
            [
                define_op!(LD, A, BC),
                define_op!(LD, A, DE),
                define_op!(LD, A, Hli),
                define_op!(LD, A, Hld),
            ],
            [
                define_op!(DEC16, BC),
                define_op!(DEC16, DE),
                define_op!(DEC16, HL),
                define_op!(DEC16, SP),
            ],
            [
                define_op!(INC, C),
                define_op!(INC, E),
                define_op!(INC, L),
                define_op!(INC, A),
            ],
            [
                define_op!(DEC, C),
                define_op!(DEC, E),
                define_op!(DEC, L),
                define_op!(DEC, A),
            ],
            [
                define_op!(LD, C),
                define_op!(LD, E),
                define_op!(LD, L),
                define_op!(LD, A),
            ],
            [
                define_op!(RRCA),
                define_op!(RRA),
                define_op!(CPL),
                define_op!(CCF),
            ],
        ];
        const TRANSPOSED: [OpArray<16>; 4] = transpose!(TO_TRANSPOSED);
        const CHUNK: OpArray<0x40> =
            concat_arrays!(TRANSPOSED[0], TRANSPOSED[1], TRANSPOSED[2], TRANSPOSED[3]);
        CHUNK
    }};
    (CHUNK_TWO) => {
        define_op_chunk!(LD)
    };
    (CHUNK_THREE) => {{
        const CHUNK: OpArray<0x40> = concat_arrays!(
            define_op_chunk!(ADD),
            define_op_chunk!(ADC),
            define_op_chunk!(SUB),
            define_op_chunk!(SBC),
            define_op_chunk!(AND),
            define_op_chunk!(XOR),
            define_op_chunk!(OR),
            define_op_chunk!(CP)
        );
        CHUNK
    }};
    (CHUNK_FOUR) => {{
        const TO_TRANSPOSED: [OpArray<4>; 0x10] = [
            [
                define_op!(RET, NotZero),
                define_op!(RET, NotCarry),
                define_op!(LoadHigh),
                define_op!(StoreHigh),
            ],
            [
                define_op!(POP, BC),
                define_op!(POP, DE),
                define_op!(POP, HL),
                define_op!(POP, AF),
            ],
            [
                define_op!(JP, NotZero),
                define_op!(JP, NotCarry),
                define_op!(LDHCA),
                define_op!(LDHAC),
            ],
            [
                define_op!(JP),
                define_op!(), // DONE
                define_op!(), // DONE
                define_op!(DI),
            ],
            [
                define_op!(CALL, NotCarry),
                define_op!(CALL, NotCarry),
                define_op!(), // DONE
                define_op!(), // DONE
            ],
            [
                define_op!(PUSH, BC),
                define_op!(PUSH, DE),
                define_op!(PUSH, HL),
                define_op!(PUSH, AF),
            ],
            [
                define_op!(ADD),
                define_op!(SUB),
                define_op!(AND),
                define_op!(OR),
            ],
            [
                define_op!(RST00),
                define_op!(RST10),
                define_op!(RST20),
                define_op!(RST30),
            ],
            [
                define_op!(RET, Zero),
                define_op!(RET, Carry),
                define_op!(ADD, SP),
                define_op!(LD, HL, SP),
            ],
            [
                define_op!(RET),
                define_op!(RETI),
                define_op!(JP, HL),
                define_op!(LD, SP, HL),
            ],
            [
                define_op!(JP, Zero),
                define_op!(JP, Carry),
                define_op!(LoadA),
                define_op!(StoreA),
            ],
            [
                define_op!(PREFIX),
                define_op!(), // DONE
                define_op!(), // DONE
                define_op!(EI),
            ],
            [
                define_op!(CALL, Zero),
                define_op!(CALL, Carry),
                define_op!(), // DONE
                define_op!(), // DONE
            ],
            [
                define_op!(CALL),
                define_op!(), // DONE
                define_op!(), // DONE
                define_op!(), // DONE
            ],
            [
                define_op!(ADC),
                define_op!(SBC),
                define_op!(XOR),
                define_op!(CP),
            ],
            [
                define_op!(RST08),
                define_op!(RST18),
                define_op!(RST28),
                define_op!(RST38),
            ],
        ];
        const TRANSPOSED: [OpArray<16>; 4] = transpose!(TO_TRANSPOSED);
        const CHUNK: OpArray<0x40> =
            concat_arrays!(TRANSPOSED[0], TRANSPOSED[1], TRANSPOSED[2], TRANSPOSED[3]);
        CHUNK
    }};
}

macro_rules! transpose {
    ($arr: ident) => {{
        const TRANSPOSED: [OpArray<16>; 4] = [
            transpose!($arr, 0),
            transpose!($arr, 1),
            transpose!($arr, 2),
            transpose!($arr, 3),
        ];
        TRANSPOSED
    }};
    ($arr: ident, $i: literal) => {{
        const INNER: OpArray<16> = [
            $arr[0][$i],
            $arr[1][$i],
            $arr[2][$i],
            $arr[3][$i],
            $arr[4][$i],
            $arr[5][$i],
            $arr[6][$i],
            $arr[7][$i],
            $arr[8][$i],
            $arr[9][$i],
            $arr[10][$i],
            $arr[11][$i],
            $arr[12][$i],
            $arr[13][$i],
            $arr[14][$i],
            $arr[15][$i],
        ];
        INNER
    }};
}

const OP_LOOKUP: OpArray<0x100> = define_op_lookup_table!();
const PREFIXED_OP_LOOKUP: OpArray<0x100> = define_op_lookup_table!(PREFIXED);

#[cfg(test)]
mod test {
    use crate::mem::MemoryMap;

    use super::{parse_instruction, parse_prefixed_instruction};

    #[test]
    fn dedupped_op_lookup_tables() {
        let mut data = MemoryMap::construct();
        // Test standard ops
        let mut ops: Vec<_> = (0..=u8::MAX)
            .filter_map(|i| {
                data.rom_mut()[0] = i;
                std::panic::catch_unwind(|| parse_instruction(&data, 0)).ok()
            })
            .collect();
        // Ensure (almost) all of the operations are actually returning unique values
        let len = (u8::MAX - 10) as usize;
        assert_eq!(len, ops.len());
        ops.dedup();
        assert_eq!(len, ops.len());

        // Test prefixed ops
        let mut more_ops: Vec<_> = (0..=u8::MAX)
            .filter_map(|i| {
                data.rom_mut()[0] = i;
                std::panic::catch_unwind(|| parse_prefixed_instruction(&data, 0)).ok()
            })
            .collect();
        // Ensure all of the operations are actually returning unique values
        assert_eq!(0x100, more_ops.len());
        more_ops.dedup();
        assert_eq!(0x100, more_ops.len());
        ops.extend(more_ops.into_iter());
        ops.dedup();
        assert_eq!(len + 0x100, ops.len());
    }
}
