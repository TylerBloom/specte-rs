use std::ops::{Index, IndexMut};

use crate::instruction::Instruction;

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
pub struct Cpu {
    af: u16,
    bc: u16,
    de: u16,
    hl: u16,
    sp: u16,
    pc: u16,
}

impl Cpu {
    pub fn execute(&mut self, instr: Instruction) {
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
