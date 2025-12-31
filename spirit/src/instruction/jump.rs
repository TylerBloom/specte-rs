
use super::*;

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
    pub(crate) fn execute<M: MemoryLikeExt>(self, state: GameboyState<'_, M>) {
        match self {
            JumpOp::ConditionalRelative(condition, _) => todo!(),
            JumpOp::Relative(_) => todo!(),
            JumpOp::ConditionalAbsolute(condition, _) => todo!(),
            JumpOp::Absolute(_) => todo!(),
            JumpOp::JumpToHL => todo!(),
            JumpOp::Call(_) => todo!(),
            JumpOp::ConditionalCall(condition, _) => todo!(),
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
