use std::ops::{Index, IndexMut};

use crate::{
    lookup::{
        parse_instruction, ArithmeticOp, BitShiftOp, Condition, ControlOp, Instruction, JumpOp,
        LoadAPointer, LoadOp, RegOrPointer, WideReg, WideRegWithoutSP,
    },
    mbc::MemoryMap,
};

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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct Flags {
    /// The zero flag
    z: bool,
    /// The substraction flag
    n: bool,
    /// The half-carry flag
    h: bool,
    /// The full carry flag
    c: bool,
}

impl Flags {
    pub fn as_byte(&self) -> u8 {
        ((self.z as u8) << 7)
            | ((self.n as u8) << 6)
            | ((self.h as u8) << 5)
            | ((self.c as u8) << 4)
    }
}

#[derive(Debug, Default)]
pub(crate) struct Cpu {
    a: u8,
    f: Flags,
    b: u8,
    c: u8,
    d: u8,
    e: u8,
    h: u8,
    l: u8,
    /// The SP register
    sp: u16,
    /// The PC register
    pc: u16,
    /// Once the gameboy has halted, this flag is set. Note that the gameboy can continue to be
    /// ticked, but the stack pointer is not moved, so it will continue to cycle without change.
    done: bool,
}

fn check_bit(src: u8, bit: u8) -> bool {
    let bit = 0x1 << bit;
    (src & bit) == bit
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
    pub fn flags(&self) -> &Flags {
        &self.f
    }

    /// Returns the value of the Z flag
    pub fn zero_flag(&self) -> bool {
        self.f.z
    }

    /// Returns the value of the N flag
    pub fn subtraction_flag(&self) -> bool {
        self.f.n
    }

    /// Returns the value of the H flag
    pub fn half_carry_flag(&self) -> bool {
        self.f.h
    }

    /// Returns the value of the Z flag
    pub fn carry_flag(&self) -> bool {
        self.f.c
    }

    pub fn read_op(&self, mem: &MemoryMap) -> Instruction {
        mem.read_op(self.sp)
    }

    /// A method similar to `Self::read_op`, but is ran during start up, when the ROM that is
    /// burned-into the CPU is mapped over normal memory.
    pub(crate) fn start_up_read_op(&self, mem: &MemoryMap) -> Instruction {
        println!("Reading start up op from: {:X}", self.pc);
        mem.read_op(self.pc)
    }

    pub fn execute(&mut self, instr: Instruction, mem: &mut MemoryMap) {
        println!("Next op: {instr:X?}");
        let len = instr.size();
        self.pc += (!self.done as u16) * (len as u16);
        match instr {
            Instruction::Load(op) => self.execute_load_op(op, mem),
            Instruction::BitShift(op) => self.execute_bit_shift_op(op, mem),
            Instruction::ControlOp(op) => self.execute_control_op(op, mem),
            Instruction::Bit(_) => todo!(),
            Instruction::Jump(op) => self.execute_jump_op(op, mem),
            Instruction::Arithmetic(op) => self.execute_arithmetic_op(op, mem),
            Instruction::Daa => todo!(),
            Instruction::Scf => todo!(),
            Instruction::Cpl => {
                let val = !self.get_reg_a();
                self.store_reg_a(val);
                self.f.n = true;
                self.f.h = true;
            }
            Instruction::Ccf => todo!(),
            Instruction::Di => todo!(),
            Instruction::Ei => todo!(),
        }
    }

    fn ptr(&self) -> u16 {
        u16::from_be_bytes([self.h, self.l])
    }

    fn execute_arithmetic_op(&mut self, op: ArithmeticOp, mem: &mut MemoryMap) {
        match op {
            ArithmeticOp::Add16(_) => todo!(),
            ArithmeticOp::Add(_) => todo!(),
            ArithmeticOp::AddDirect(_) => todo!(),
            ArithmeticOp::Adc(_) => todo!(),
            ArithmeticOp::AdcDirect(_) => todo!(),
            ArithmeticOp::Sub(_) => todo!(),
            ArithmeticOp::SubDirect(_) => todo!(),
            ArithmeticOp::Sbc(_) => todo!(),
            ArithmeticOp::SbcDirect(_) => todo!(),
            ArithmeticOp::And(_) => todo!(),
            ArithmeticOp::AndDirect(_) => todo!(),
            ArithmeticOp::Xor(reg) => {
                let val = self.read_byte(mem, reg);
                let new = self.get_reg_a() ^ val;
                if new == 0 {
                    self.f.z = true;
                }
                self.store_reg_a(new);
            }
            ArithmeticOp::XorDirect(_) => todo!(),
            ArithmeticOp::Or(_) => todo!(),
            ArithmeticOp::OrDirect(_) => todo!(),
            ArithmeticOp::Cp(_) => todo!(),
            ArithmeticOp::CpDirect(_) => todo!(),
            ArithmeticOp::Inc(_) => todo!(),
            ArithmeticOp::Dec(reg) => {
                let val = match reg {
                    RegOrPointer::A => {
                        let val = self.get_reg_a() - 1;
                        self.store_reg_a(val);
                        val
                    }
                    RegOrPointer::B => {
                        let val = self.get_reg_b() - 1;
                        self.store_reg_b(val);
                        val
                    }
                    RegOrPointer::C => {
                        let val = self.get_reg_c() - 1;
                        self.store_reg_c(val);
                        val
                    }
                    RegOrPointer::D => {
                        let val = self.get_reg_d() - 1;
                        self.store_reg_d(val);
                        val
                    }
                    RegOrPointer::E => {
                        let val = self.get_reg_e() - 1;
                        self.store_reg_e(val);
                        val
                    }
                    RegOrPointer::H => {
                        let val = self.get_reg_h() - 1;
                        self.store_reg_h(val);
                        val
                    }
                    RegOrPointer::L => {
                        let val = self.get_reg_l() - 1;
                        self.store_reg_l(val);
                        val
                    }
                    RegOrPointer::Pointer => {
                        let val = mem[self.ptr()] - 1;
                        mem[self.ptr()] = val;
                        val
                    }
                };
                if val == 0 {
                    self.f.z = true;
                }
                self.f.n = true;
                self.f.h = ((val >> 3) != 0) as bool;
            }
            ArithmeticOp::Inc16(_) => todo!(),
            ArithmeticOp::Dec16(_) => todo!(),
            ArithmeticOp::AddSP(_) => todo!(),
        }
    }

    fn read_byte(&self, mem: &MemoryMap, reg: RegOrPointer) -> u8 {
        match reg {
            RegOrPointer::A => self.get_reg_a(),
            RegOrPointer::B => self.get_reg_b(),
            RegOrPointer::C => self.get_reg_c(),
            RegOrPointer::D => self.get_reg_d(),
            RegOrPointer::E => self.get_reg_e(),
            RegOrPointer::H => self.get_reg_h(),
            RegOrPointer::L => self.get_reg_l(),
            RegOrPointer::Pointer => mem[self.ptr()],
        }
    }

    fn matches(&self, cond: Condition) -> bool {
        match cond {
            Condition::Zero => self.zero_flag(),
            Condition::NotZero => !self.zero_flag(),
            Condition::Carry => self.carry_flag(),
            Condition::NotCarry => !self.carry_flag(),
        }
    }

    fn execute_jump_op(&mut self, op: JumpOp, mem: &mut MemoryMap) {
        match op {
            JumpOp::ConditionalRelative(cond, val) => {
                if self.matches(cond) {
                    if val < 0 {
                        self.pc -= val.abs() as u16;
                    } else {
                        self.pc += val as u16;
                    }
                }
            }
            JumpOp::Relative(_) => todo!(),
            JumpOp::ConditionalAbsolute(_, _) => todo!(),
            JumpOp::JumpToHL => todo!(),
            JumpOp::Absolute(val) => self.pc = val,
            JumpOp::Call(ptr) => {
                let [hi, lo] = self.pc.to_be_bytes();
                self.sp -= 1;
                mem[self.sp] = hi;
                self.sp -= 1;
                mem[self.sp] = lo;
                self.pc = ptr;
            }
            JumpOp::ConditionalCall(_, _) => todo!(),
            JumpOp::Return => todo!(),
            JumpOp::ConditionalReturn(_) => todo!(),
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

    fn execute_control_op(&mut self, op: ControlOp, mem: &mut MemoryMap) {
        match op {
            ControlOp::Noop => {}
            ControlOp::Stop(_) => self.done = true,
        }
    }

    fn execute_bit_shift_op(&mut self, op: BitShiftOp, mem: &mut MemoryMap) {
        match op {
            BitShiftOp::Rlc(reg) => {}
            BitShiftOp::Rrc(_) => todo!(),
            BitShiftOp::Rl(reg) => {
                let mut val = self.get_half_value(reg, mem);
                val = val.rotate_left(1);
                let carry = (val & 0x1) == 1;
                val &= (0xFF & self.carry_flag() as u8);
                self.f.c = carry;
                self.f.z = val == 0;
                self.store_half_value(reg, mem, val);
            }
            BitShiftOp::Rr(reg) => {
                let mut val = self.get_half_value(reg, mem);
                val = val.rotate_right(1);
                let carry = (val & 0x80) == 1;
                val &= (0xFF & ((self.carry_flag() as u8) << 7));
                self.f.c = carry;
                self.f.z = val == 0;
                self.store_half_value(reg, mem, val);
            }
            BitShiftOp::Sla(_) => todo!(),
            BitShiftOp::Sra(_) => todo!(),
            BitShiftOp::Swap(reg) => {
                let mut val = self.get_half_value(reg, mem);
                let lw = val & 0xF0 >> 4;
                let hi = val & 0x0F << 4;
                val = hi | lw;
                self.f.z = val == 0;
                self.store_half_value(reg, mem, val);
            }
            BitShiftOp::Srl(_) => todo!(),
        }
    }

    fn execute_load_op(&mut self, op: LoadOp, mem: &mut MemoryMap) {
        match op {
            LoadOp::Basic {
                dest: RegOrPointer::Pointer,
                src: RegOrPointer::Pointer,
            } => {
                todo!("halt")
            }
            LoadOp::Basic { dest, src } => {
                self.store_half_value(dest, mem, self.get_half_value(src, mem))
            }
            LoadOp::Direct16(reg, val) => self[reg] = val,
            LoadOp::Direct(reg, val) => self.store_half_value(reg, mem, val),
            LoadOp::LoadIntoA(ptr) => {
                let val = *self.deref_ptr(mem, ptr);
                self.store_reg_a(val)
            }
            LoadOp::StoreFromA(ptr) => *self.deref_ptr(mem, ptr) = self.get_reg_a(),
            LoadOp::StoreSP(val) => {
                let [lw, hi] = self.sp.to_le_bytes();
                mem[val] = lw;
                mem[val + 1] = hi;
            }
            LoadOp::HLIntoSP => self.sp = self.ptr(),
            LoadOp::SPIntoHL(val) => {
                let [h, l] = if val >= 0 {
                    self.sp + val as u16
                } else {
                    self.sp - val as u16
                }
                .to_be_bytes();
                self.h = h;
                self.l = l;
            }
            LoadOp::Pop(reg) => {
                let a = mem[self.sp];
                self.sp += 1;
                let b = mem[self.sp];
                self.sp += 1;
                self[reg] = u16::from_le_bytes([a, b]);
            }
            LoadOp::Push(reg) => {
                let [a, b] = match reg {
                    WideRegWithoutSP::BC => [self.b, self.c],
                    WideRegWithoutSP::DE => [self.d, self.e],
                    WideRegWithoutSP::HL => [self.h, self.l],
                    WideRegWithoutSP::AF => [self.a, self.f.as_byte()],
                };
                mem[self.sp] = a;
                self.sp -= 1;
                mem[self.sp] = b;
                self.sp -= 1;
            }
            LoadOp::LoadHigh(val) => self.store_reg_a(mem[u16::from_le_bytes([0xFF, val])]),
            LoadOp::StoreHigh(val) => mem[u16::from_le_bytes([0xFF, val])] = self.get_reg_a(),
            LoadOp::LoadHighCarry => {
                self.store_reg_a(mem[u16::from_le_bytes([0xFF, self.carry_flag() as u8])])
            }
            LoadOp::StoreHighCarry => {
                mem[u16::from_le_bytes([0xFF, self.carry_flag() as u8])] = self.get_reg_a()
            }
            LoadOp::LoadA { ptr } => mem[ptr] = self.get_reg_a(),
            LoadOp::StoreA { ptr } => self.store_reg_a(mem[ptr]),
        }
    }

    fn deref_ptr<'a, 'b>(&'a mut self, mem: &'b mut MemoryMap, ptr: LoadAPointer) -> &'b mut u8 {
        match ptr {
            LoadAPointer::BC => &mut mem[self.bc],
            LoadAPointer::DE => &mut mem[self.de],
            LoadAPointer::Hli => {
                let digest = &mut mem[self.hl];
                self.hl += 1;
                digest
            }
            LoadAPointer::Hld => {
                let digest = &mut mem[self.hl];
                self.hl -= 1;
                digest
            }
        }
    }

    fn get_half_value(&self, reg: RegOrPointer, mem: &MemoryMap) -> u8 {
        match reg {
            RegOrPointer::A => self.get_reg_a(),
            RegOrPointer::B => self.get_reg_b(),
            RegOrPointer::C => self.get_reg_c(),
            RegOrPointer::D => self.get_reg_d(),
            RegOrPointer::E => self.get_reg_e(),
            RegOrPointer::H => self.get_reg_h(),
            RegOrPointer::L => self.get_reg_l(),
            RegOrPointer::Pointer => mem[self.hl],
        }
    }

    #[inline(always)]
    fn get_reg_a(&self) -> u8 {
        self.af.to_ne_bytes()[1]
    }

    #[inline(always)]
    fn store_reg_a(&mut self, val: u8) {
        self.af = (self.af & 0x00FF) | ((val as u16) << 8);
    }

    #[inline(always)]
    fn get_reg_b(&self) -> u8 {
        self.bc.to_ne_bytes()[1]
    }

    #[inline(always)]
    fn store_reg_b(&mut self, val: u8) {
        self.bc = (self.bc & 0x00FF) | ((val as u16) << 8);
    }

    #[inline(always)]
    fn get_reg_c(&self) -> u8 {
        self.bc.to_ne_bytes()[0]
    }

    #[inline(always)]
    fn store_reg_c(&mut self, val: u8) {
        self.bc = (self.bc & 0xFF00) | val as u16;
    }

    #[inline(always)]
    fn get_reg_d(&self) -> u8 {
        self.de.to_ne_bytes()[1]
    }

    #[inline(always)]
    fn store_reg_d(&mut self, val: u8) {
        self.de = (self.de & 0x00FF) | ((val as u16) << 8);
    }

    #[inline(always)]
    fn get_reg_e(&self) -> u8 {
        self.de.to_ne_bytes()[0]
    }

    #[inline(always)]
    fn store_reg_e(&mut self, val: u8) {
        self.de = (self.de & 0xFF00) | val as u16;
    }

    #[inline(always)]
    fn get_reg_h(&self) -> u8 {
        self.hl.to_ne_bytes()[1]
    }

    #[inline(always)]
    fn store_reg_h(&mut self, val: u8) {
        self.hl = (self.hl & 0x00FF) | ((val as u16) << 8);
    }

    #[inline(always)]
    fn get_reg_l(&self) -> u8 {
        self.hl.to_ne_bytes()[0]
    }

    #[inline(always)]
    fn store_reg_l(&mut self, val: u8) {
        self.hl = (self.hl & 0xFF00) | val as u16;
    }

    /// Stores the given byte into either an (half) register or into the MemoryMap using the HL
    /// register as an index.
    fn store_half_value(&mut self, reg: RegOrPointer, mem: &mut MemoryMap, val: u8) {
        match reg {
            RegOrPointer::A => self.store_reg_a(val),
            RegOrPointer::B => self.store_reg_b(val),
            RegOrPointer::C => self.store_reg_c(val),
            RegOrPointer::D => self.store_reg_d(val),
            RegOrPointer::E => self.store_reg_e(val),
            RegOrPointer::H => self.store_reg_h(val),
            RegOrPointer::L => self.store_reg_l(val),
            RegOrPointer::Pointer => {
                mem[self.ptr()] = val;
            }
        }
    }

    /// A method similar to `Self::execute`, but is ran during start up, when the ROM that is
    /// burned-into the CPU is mapped over normal memory.
    ///
    /// Returns a the next operation should the start up process not be completed.
    pub fn start_up_execute(
        &mut self,
        instr: Instruction,
        mem: &mut MemoryMap,
    ) -> Option<Instruction> {
        self.execute(instr, mem);
        println!("Applied startup op: {:?}", self);
        (self.sp != 0x0100).then(|| self.start_up_read_op(mem))
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

#[cfg(test)]
mod tests {
    use crate::{
        lookup::{Instruction, LoadOp, RegOrPointer},
        mbc::MemoryMap,
    };

    use super::Cpu;

    fn init() -> (Cpu, MemoryMap) {
        static CART: &[u8] = include_bytes!("../tests/roms/acid/which.gb");
        let cpu = Cpu::default();
        let mem = MemoryMap::new(CART);
        (cpu, mem)
    }

    fn reg_iter() -> &'static [RegOrPointer] {
        static REGS: &[RegOrPointer] = &[
            RegOrPointer::A,
            RegOrPointer::B,
            RegOrPointer::C,
            RegOrPointer::D,
            RegOrPointer::E,
            RegOrPointer::H,
            RegOrPointer::L,
            RegOrPointer::Pointer,
        ];
        REGS
    }

    #[test]
    fn test_cpl() {
        let (mut cpu, mut mem) = init();
        assert_eq!(cpu.pc, 0);
        cpu.execute(Instruction::Cpl, &mut mem);
        assert_eq!(cpu.pc, 1);
        assert_eq!(cpu.get_reg_a(), u8::MAX);
        assert!(cpu.subtraction_flag(), "{cpu:#X?}");
        assert!(cpu.half_carry_flag(), "{cpu:#X?}");
        cpu.store_reg_a(1);
        cpu.execute(Instruction::Cpl, &mut mem);
        assert_eq!(cpu.get_reg_a(), u8::MAX << 1);
        assert_eq!(cpu.pc, 2);
    }

    /*
    pub enum LoadOp {
        /// Used for opcodes 0xX1
        Direct16(WideReg, u16),
        /// Used for opcodes 0x_6 and 0x_E
        Direct(RegOrPointer, u8),
        /// Used for opcodes 0x_A
        LoadIntoA(LoadAPointer),
        /// Used for opcodes 0x_2
        StoreFromA(LoadAPointer),
        /// Opcode: 0x08
        /// Store SP & $FF at address n16 and SP >> 8 at address n16 + 1.
        StoreSP(u16),
        /// Opcode: 0xF9
        HLIntoSP,
        /// Opcode: 0xF8
        /// Add the signed value e8 to SP and store the result in HL.
        SPIntoHL(i8),
        /// Used for opcodes 0x_1
        Pop(WideRegWithoutSP),
        /// Used for opcodes 0x_5
        Push(WideRegWithoutSP),
        /// Used for opcode 0xE0
        LoadHigh(u8),
        /// Used for opcode 0xF0
        StoreHigh(u8),
        /// Used for opcode 0xE2
        LoadHighCarry,
        /// Used for opcode 0xF2
        StoreHighCarry,
        /// Used for opcode 0xEA
        LoadA { ptr: u16 },
        /// Used for opcode 0xFA
        StoreA { ptr: u16 },
    }
    */
    #[test]
    fn test_basic_load_op() {
        let (mut cpu, mut mem) = init();
        let iter = reg_iter().into_iter().copied();
        for (dest, src) in iter
            .clone()
            .flat_map(|r| iter.clone().map(move |rr| (r, rr)))
        {
            let op = Instruction::Load(LoadOp::Basic { dest, src });
            cpu.execute(op, &mut mem);
        }
    }
}
