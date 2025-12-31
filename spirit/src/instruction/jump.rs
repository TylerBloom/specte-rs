
use super::*;

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, derive_more::Display)]
#[display("{_variant}")]
pub enum JumpOp {
    /// Op Codes: 0x20, 0x30, 0x28, 0x38
    #[display("JR {_0}")]
    ConditionalRelative(Condition),
    /// Op Code: 0x18
    #[display("JR")]
    Relative,
    /// Op Codes: 0xC2, 0xD2, 0xCA, 0xDA
    #[display("JP {_0}")]
    ConditionalAbsolute(Condition),
    /// Op Code: 0xC3
    #[display("JP")]
    Absolute,
    /// Op Code: 0xE9
    #[display("JP HL")]
    JumpToHL,
    /// Op Code: 0xCD
    #[display("CALL ")]
    Call,
    /// Op Codes: 0xC4, 0xD4, 0xCC, 0xDC
    #[display("CALL {_0}")]
    ConditionalCall(Condition),
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
    pub(crate) fn execute<M: MemoryLikeExt>(self, state: GameboyState<'_, M>) {
        match self {
            JumpOp::ConditionalRelative(condition) => todo!(),
            JumpOp::Relative => todo!(),
            JumpOp::ConditionalAbsolute(condition) => todo!(),
            JumpOp::Absolute => todo!(),
            JumpOp::JumpToHL => todo!(),
            JumpOp::Call => todo!(),
            JumpOp::ConditionalCall(condition) => todo!(),
            JumpOp::Return => todo!(),
            JumpOp::ConditionalReturn(condition) => todo!(),
            JumpOp::ReturnAndEnable => todo!(),
            JumpOp::RST00 => todo!(),
            JumpOp::RST08 => todo!(),
            JumpOp::RST10 => todo!(),
            JumpOp::RST18 => todo!(),
            JumpOp::RST20 => todo!(),
            JumpOp::RST28 => todo!(),
            JumpOp::RST30 => todo!(),
            JumpOp::RST38 => todo!(),
        }
    }

    /// Returns the number of ticks to will take to complete this instruction.
    pub fn length(&self, cpu: &Cpu) -> u8 {
        match self {
            JumpOp::ConditionalRelative(cond) => 8 + (4 * cond.passed(cpu) as u8),
            JumpOp::Relative => 12,
            JumpOp::ConditionalAbsolute(cond) => 12 + (4 * cond.passed(cpu) as u8),
            JumpOp::Absolute => 16,
            JumpOp::JumpToHL => 4,
            JumpOp::Call => 24,
            JumpOp::ConditionalCall(cond) => 12 + (12 * cond.passed(cpu) as u8),
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
            JumpOp::ConditionalRelative(_) => 2,
            JumpOp::Relative => 2,
            JumpOp::ConditionalAbsolute(_) => 3,
            JumpOp::Absolute => 3,
            JumpOp::JumpToHL => 1,
            JumpOp::Call => 3,
            JumpOp::ConditionalCall(_) => 3,
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
