use std::ops::{Index, IndexMut};

use crate::{instruction::{Instruction, LoadOp, ControlOp}, mbc::MemoryBankController};

static START_UP_HEADER: &[u8; 0x900] = include_bytes!("cgb.bin");

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FullRegister {
    AF,
    BC,
    DE,
    HL,
    SP,
    PC,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HalfRegister {
    A,
    B,
    C,
    D,
    E,
    H,
    L,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RegisterFlags {
    Z,
    N,
    H,
    C,
}

#[derive(Debug, Default)]
pub(crate) struct Cpu {
    /// The AF register
    af: u16,
    /// The BC register
    bc: u16,
    /// The DE register
    de: u16,
    /// The HL register
    hl: u16,
    /// The SP register
    sp: u16,
    /// The PC register
    pc: u16,
}

fn check_bit(src: u8, bit: u8) -> bool {
    (src & (0x1 << bit)) == bit
}

impl Cpu {
    /// Constructs a new CPU with each register set to 0.
    pub fn new() -> Self {
        Self {
            ..Default::default()
        }
    }

    /// Get the top four bits of the F register
    #[inline]
    pub fn flags(&self) -> u8 {
        self.af as u8 & 0xF0
    }

    /// Returns the value of the Z flag
    pub fn zero_flag(&self) -> bool {
        check_bit(self.flags(), 7)
    }

    /// Returns the value of the Z flag
    pub fn carry_flag(&self) -> bool {
        check_bit(self.flags(), 7)
    }

    pub fn read_op(&self, mbc: &MemoryBankController) -> Instruction {
        mbc.read_op(self.sp)
    }

    /// A method similar to `Self::read_op`, but is ran during start up, when the ROM that is
    /// burned-into the CPU is mapped over normal memory.
    pub(crate) fn start_up_read_op(&self, mbc: &MemoryBankController) -> Instruction {
        match self.sp {
            sp @ 0x0000..=0x00FF | sp @ 0x0200..=0x08FF => {
                Instruction::parse(&START_UP_HEADER[sp as usize..])
            }
            sp => mbc.read_op(sp),
        }
    }

    pub fn execute(&mut self, instr: Instruction, mbc: &mut MemoryBankController) {
        match instr {
            Instruction::Control(op) => self.execute_control_op(op, mbc),
            Instruction::Load(op) => self.execute_load_op(op, mbc),
            Instruction::ArithLog(_) => todo!(),
            _ => todo!()
        }
    }

    pub fn execute_control_op(&mut self, op: ControlOp, mbc: &mut MemoryBankController) {
        match op {
            ControlOp::NoOp => { },
            ControlOp::Stop(_) => todo!(),
            ControlOp::Halt => todo!(),
            ControlOp::DI => todo!(),
            ControlOp::EI => todo!(),
            ControlOp::Scf => todo!(),
            ControlOp::Ccf => todo!(),
        }
    }

    pub fn execute_load_op(&mut self, op: LoadOp, mbc: &mut MemoryBankController) {
        match op {
            LoadOp::FullLoad(_, _) => todo!(),
            LoadOp::HalfLoad { reg, data } => todo!(),
            LoadOp::SwapHalfReg { src, dest } => todo!(),
            LoadOp::HLIncStore => todo!(),
            LoadOp::HLDecStore => todo!(),
            LoadOp::HLIncLoad => todo!(),
            LoadOp::HLDecLoad => todo!(),
            LoadOp::StoreSP(_) => todo!(),
            LoadOp::SPLoad => todo!(),
            LoadOp::SignedPointerLoad(_) => todo!(),
            LoadOp::Pop(_) => todo!(),
            LoadOp::Push(_) => todo!(),
            _ => todo!(),
        }
    }

    /// A method similar to `Self::execute`, but is ran during start up, when the ROM that is
    /// burned-into the CPU is mapped over normal memory.
    ///
    /// Returns a the next operation should the start up process not be completed.
    pub fn start_up_execute(
        &mut self,
        instr: Instruction,
        mbc: &mut MemoryBankController,
    ) -> Option<Instruction> {
        // TODO: Do we need to work about remapped memory??
        // self.execute(instr, mbc)
        todo!()
    }
}

impl Index<FullRegister> for Cpu {
    type Output = u16;

    fn index(&self, index: FullRegister) -> &Self::Output {
        match index {
            FullRegister::AF => &self.af,
            FullRegister::BC => &self.bc,
            FullRegister::DE => &self.de,
            FullRegister::HL => &self.hl,
            FullRegister::SP => &self.sp,
            FullRegister::PC => &self.pc,
        }
    }
}

impl IndexMut<FullRegister> for Cpu {
    fn index_mut(&mut self, index: FullRegister) -> &mut Self::Output {
        match index {
            FullRegister::AF => &mut self.af,
            FullRegister::BC => &mut self.bc,
            FullRegister::DE => &mut self.de,
            FullRegister::HL => &mut self.hl,
            FullRegister::SP => &mut self.sp,
            FullRegister::PC => &mut self.pc,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{Cpu, FullRegister};

    fn init_cpu() -> Cpu {
        Cpu {
            af: FullRegister::AF as u16,
            bc: FullRegister::BC as u16,
            de: FullRegister::DE as u16,
            hl: FullRegister::HL as u16,
            sp: FullRegister::SP as u16,
            pc: FullRegister::PC as u16,
        }
    }

    #[test]
    fn full_register_index() {
        let mut cpu = init_cpu();

        let af = FullRegister::AF;
        assert_eq!(cpu[af], af as u16);
        cpu[af] = 0;
        assert_eq!(cpu[af], 0);

        let bc = FullRegister::BC;
        assert_eq!(cpu[bc], FullRegister::BC as u16);
        cpu[bc] = 0;
        assert_eq!(cpu[bc], 0);

        let de = FullRegister::DE;
        assert_eq!(cpu[de], FullRegister::DE as u16);
        cpu[de] = 0;
        assert_eq!(cpu[de], 0);

        let hl = FullRegister::HL;
        assert_eq!(cpu[hl], FullRegister::HL as u16);
        cpu[hl] = 0;
        assert_eq!(cpu[hl], 0);

        let sp = FullRegister::SP;
        assert_eq!(cpu[sp], FullRegister::SP as u16);
        cpu[sp] = 0;
        assert_eq!(cpu[sp], 0);

        let pc = FullRegister::PC;
        assert_eq!(cpu[pc], FullRegister::PC as u16);
        cpu[pc] = 0;
        assert_eq!(cpu[pc], 0);
    }
}
