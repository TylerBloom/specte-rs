
use super::*;

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
            ArithmeticOp::Inc(RegOrPointer::Pointer) => 12,
            ArithmeticOp::Inc(RegOrPointer::Reg(_)) => 4,
            ArithmeticOp::Dec(RegOrPointer::Pointer) => 12,
            ArithmeticOp::Dec(RegOrPointer::Reg(_)) => 4,
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
