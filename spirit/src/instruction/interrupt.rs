use crate::GameboyState;
use crate::mem::MemoryLikeExt;

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

impl InterruptOp {
    pub(crate) fn execute<M: MemoryLikeExt>(self, _state: &mut GameboyState<'_, M>) {
        match self {
            InterruptOp::VBlank => todo!(),
            InterruptOp::LCD => todo!(),
            InterruptOp::Timer => todo!(),
            InterruptOp::Serial => todo!(),
            InterruptOp::Joypad => todo!(),
        }
    }
}
