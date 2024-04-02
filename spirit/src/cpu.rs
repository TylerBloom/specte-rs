use std::ops::{Index, IndexMut};

use crate::{
    lookup::{
        parse_instruction, BitShiftOp, Instruction, LoadAPointer, LoadOp, RegOrPointer, WideReg,
        WideRegWithoutSP,
    },
    mbc::MemoryBankController,
};

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
        let sp = match self.sp {
            sp @ 0x0000..=0x00FF | sp @ 0x0200..=0x08FF => sp,
            sp => mbc.read_op(sp),
        };
        parse_instruction(sp)
    }

    pub fn execute(&mut self, instr: Instruction, mbc: &mut MemoryBankController) {
        match instr {
            Instruction::Load(op) => self.execute_load_op(op, mbc),
            Instruction::BitShift(op) => self.execute_bit_shift_op(op, mbc),
            Instruction::ControlOp(_) => todo!(),
            Instruction::Bit(_) => todo!(),
            Instruction::Jump(_) => todo!(),
            Instruction::Arithmetic(_) => todo!(),
            Instruction::Daa => todo!(),
            Instruction::Scf => todo!(),
            Instruction::Cpl => todo!(),
            Instruction::Ccf => todo!(),
            Instruction::Di => todo!(),
            Instruction::Ei => todo!(),
        }
    }

    fn execute_bit_shift_op(&mut self, op: BitShiftOp, mbc: &mut MemoryBankController) {
        match op {
            BitShiftOp::Rlc(reg) => {}
            BitShiftOp::Rrc(_) => todo!(),
            BitShiftOp::Rl(reg) => {
                let mut val = self.get_half_value(reg, mbc);
                val = val.rotate_left(1);
                let carry = (val & 0x1) == 1;
                val &= (0xFF & self.carry_flag() as u8);
                self.set_carry_flag(carry);
                self.set_zero_flag(val == 0);
                self.store_half_value(reg, mbc, val);
            }
            BitShiftOp::Rr(reg) => {
                let mut val = self.get_half_value(reg, mbc);
                val = val.rotate_right(1);
                let carry = (val & 0x80) == 1;
                val &= (0xFF & ((self.carry_flag() as u8) << 7));
                self.set_carry_flag(carry);
                self.set_zero_flag(val == 0);
                self.store_half_value(reg, mbc, val);
            }
            BitShiftOp::Sla(_) => todo!(),
            BitShiftOp::Sra(_) => todo!(),
            BitShiftOp::Swap(reg) => {
                let mut val = self.get_half_value(reg, mbc);
                let lw = val & 0xF0 >> 4;
                let hi = val & 0x0F << 4;
                val = hi | lw;
                self.set_zero_flag(val == 0);
                self.store_half_value(reg, mbc, val);
            },
            BitShiftOp::Srl(_) => todo!(),
        }
    }

    fn set_carry_flag(&mut self, flag: bool) {
        todo!()
    }

    fn set_zero_flag(&mut self, flag: bool) {
        todo!()
    }

    fn execute_load_op(&mut self, op: LoadOp, mbc: &mut MemoryBankController) {
        match op {
            LoadOp::Basic {
                dest: RegOrPointer::Pointer,
                src: RegOrPointer::Pointer,
            } => {
                todo!("halt")
            }
            LoadOp::Basic { dest, src } => {
                self.store_half_value(dest, mbc, self.get_half_value(src, mbc))
            }
            LoadOp::Direct16(reg, val) => self[reg] = val,
            LoadOp::Direct(reg, val) => self.store_half_value(reg, mbc, val),
            LoadOp::LoadIntoA(ptr) => {
                let val = *self.deref_ptr(mbc, ptr);
                self.store_reg_a(val)
            }
            LoadOp::StoreFromA(ptr) => *self.deref_ptr(mbc, ptr) = self.get_reg_a(),
            LoadOp::StoreSP(val) => {
                let [lw, hi] = self.sp.to_le_bytes();
                mbc[val] = lw;
                mbc[val + 1] = hi;
            }
            LoadOp::HLIntoSP => self.sp = self.hl,
            LoadOp::SPIntoHL(val) => {
                self.hl = if val >= 0 {
                    self.sp + val as u16
                } else {
                    self.sp - val as u16
                }
            }
            LoadOp::Pop(reg) => {
                let a = mbc[self.sp];
                self.sp += 1;
                let b = mbc[self.sp];
                self.sp += 1;
                self[reg] = u16::from_le_bytes([a, b]);
            }
            LoadOp::Push(reg) => {
                let [a, b] = match reg {
                    WideRegWithoutSP::BC => self.bc.to_le_bytes(),
                    WideRegWithoutSP::DE => self.de.to_le_bytes(),
                    WideRegWithoutSP::HL => self.hl.to_le_bytes(),
                    WideRegWithoutSP::AF => self.af.to_le_bytes(),
                };
                mbc[self.sp] = a;
                self.sp -= 1;
                mbc[self.sp] = b;
                self.sp -= 1;
            }
            LoadOp::LoadHigh(val) => self.store_reg_a(mbc[u16::from_le_bytes([0xFF, val])]),
            LoadOp::StoreHigh(val) => mbc[u16::from_le_bytes([0xFF, val])] = self.get_reg_a(),
            LoadOp::LoadHighCarry => {
                self.store_reg_a(mbc[u16::from_le_bytes([0xFF, self.carry_flag() as u8])])
            }
            LoadOp::StoreHighCarry => {
                mbc[u16::from_le_bytes([0xFF, self.carry_flag() as u8])] = self.get_reg_a()
            }
            LoadOp::LoadA { ptr } => mbc[ptr] = self.get_reg_a(),
            LoadOp::StoreA { ptr } => self.store_reg_a(mbc[ptr]),
        }
    }

    fn deref_ptr<'a, 'b>(
        &'a mut self,
        mbc: &'b mut MemoryBankController,
        ptr: LoadAPointer,
    ) -> &'b mut u8 {
        match ptr {
            LoadAPointer::BC => &mut mbc[self.bc],
            LoadAPointer::DE => &mut mbc[self.de],
            LoadAPointer::Hli => {
                let digest = &mut mbc[self.hl];
                self.hl += 1;
                digest
            }
            LoadAPointer::Hld => {
                let digest = &mut mbc[self.hl];
                self.hl -= 1;
                digest
            }
        }
    }

    fn get_half_value(&self, reg: RegOrPointer, mbc: &MemoryBankController) -> u8 {
        match reg {
            RegOrPointer::A => self.get_reg_a(),
            RegOrPointer::B => self.get_reg_b(),
            RegOrPointer::C => self.get_reg_c(),
            RegOrPointer::D => self.get_reg_d(),
            RegOrPointer::E => self.get_reg_e(),
            RegOrPointer::H => self.get_reg_h(),
            RegOrPointer::L => self.get_reg_l(),
            RegOrPointer::Pointer => mbc[self.hl],
        }
    }

    #[inline(always)]
    fn get_reg_a(&self) -> u8 {
        self.af.to_ne_bytes()[1]
    }

    #[inline(always)]
    fn store_reg_a(&self, val: u8) {
        todo!()
    }

    #[inline(always)]
    fn get_reg_b(&self) -> u8 {
        self.bc.to_ne_bytes()[1]
    }

    #[inline(always)]
    fn store_reg_b(&self, val: u8) {
        todo!()
    }

    #[inline(always)]
    fn get_reg_c(&self) -> u8 {
        self.bc.to_ne_bytes()[0]
    }

    #[inline(always)]
    fn store_reg_c(&self, val: u8) {
        todo!()
    }

    #[inline(always)]
    fn get_reg_d(&self) -> u8 {
        self.de.to_ne_bytes()[1]
    }

    #[inline(always)]
    fn store_reg_d(&self, val: u8) {
        todo!()
    }

    #[inline(always)]
    fn get_reg_e(&self) -> u8 {
        self.de.to_ne_bytes()[0]
    }

    #[inline(always)]
    fn store_reg_e(&self, val: u8) {
        todo!()
    }

    #[inline(always)]
    fn get_reg_h(&self) -> u8 {
        self.hl.to_ne_bytes()[1]
    }

    #[inline(always)]
    fn store_reg_h(&self, val: u8) {
        todo!()
    }

    #[inline(always)]
    fn get_reg_l(&self) -> u8 {
        self.hl.to_ne_bytes()[0]
    }

    #[inline(always)]
    fn store_reg_l(&self, val: u8) {
        todo!()
    }

    fn store_half_value(&self, reg: RegOrPointer, mbc: &mut MemoryBankController, val: u8) {
        todo!()
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

impl Index<WideReg> for Cpu {
    type Output = u16;

    fn index(&self, index: WideReg) -> &Self::Output {
        match index {
            WideReg::BC => &self.bc,
            WideReg::DE => &self.de,
            WideReg::HL => &self.hl,
            WideReg::SP => &self.sp,
        }
    }
}

impl IndexMut<WideReg> for Cpu {
    fn index_mut(&mut self, index: WideReg) -> &mut Self::Output {
        match index {
            WideReg::BC => &mut self.bc,
            WideReg::DE => &mut self.de,
            WideReg::HL => &mut self.hl,
            WideReg::SP => &mut self.sp,
        }
    }
}

impl Index<WideRegWithoutSP> for Cpu {
    type Output = u16;

    fn index(&self, index: WideRegWithoutSP) -> &Self::Output {
        match index {
            WideRegWithoutSP::BC => &self.bc,
            WideRegWithoutSP::DE => &self.de,
            WideRegWithoutSP::HL => &self.hl,
            WideRegWithoutSP::AF => &self.af,
        }
    }
}

impl IndexMut<WideRegWithoutSP> for Cpu {
    fn index_mut(&mut self, index: WideRegWithoutSP) -> &mut Self::Output {
        match index {
            WideRegWithoutSP::BC => &mut self.bc,
            WideRegWithoutSP::DE => &mut self.de,
            WideRegWithoutSP::HL => &mut self.hl,
            WideRegWithoutSP::AF => &mut self.af,
        }
    }
}
