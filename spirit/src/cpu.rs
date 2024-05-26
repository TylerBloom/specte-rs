use std::ops::{Index, IndexMut};

use crate::{
    lookup::{
        parse_instruction, ArithmeticOp, BitShiftOp, Condition, ControlOp, HalfRegister,
        Instruction, JumpOp, LoadAPointer, LoadOp, RegOrPointer, WideReg, WideRegWithoutSP,
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
    pub fn set_from_byte(&mut self, val: u8) {
        self.z = check_bit::<7>(val);
        self.n = check_bit::<6>(val);
        self.h = check_bit::<5>(val);
        self.c = check_bit::<4>(val);
    }

    pub fn as_byte(&self) -> u8 {
        bool_to_mask::<7>(self.z)
            | bool_to_mask::<6>(self.n)
            | bool_to_mask::<5>(self.h)
            | bool_to_mask::<4>(self.c)
    }
}

#[derive(Debug, Default)]
pub struct Cpu {
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

const fn bit_select<const B: u8>() -> u8 {
    const {
        match B {
            n @ 0..=7 => 0x1 << n,
            _ => panic!("You must select between the 0th and 7th bit!"),
        }
    }
}

const fn bool_to_mask<const B: u8>(val: bool) -> u8 {
    (val as u8) << B
}

const fn check_bit<const B: u8>(src: u8) -> bool {
    (src & bit_select::<B>()) == bit_select::<B>()
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
                self.a = !self.a;
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

    fn pc_bytes(&self) -> [u8; 2] {
        u16::to_be_bytes(self.pc)
    }

    fn set_ptr(&mut self, val: u16) {
        todo!()
    }

    fn read_wide_reg(&self, reg: WideReg) -> u16 {
        match reg {
            WideReg::BC => self.bc(),
            WideReg::DE => self.de(),
            WideReg::HL => self.ptr(),
            WideReg::SP => self.sp,
        }
    }

    fn write_wide_reg(&mut self, reg: WideReg, val: u16) {
        match reg {
            WideReg::BC => self.write_bc(val),
            WideReg::DE => self.write_de(val),
            WideReg::HL => self.set_ptr(val),
            WideReg::SP => self.sp = val,
        }
    }

    fn write_wide_reg_without_sp(&mut self, reg: WideRegWithoutSP, val: u16) {
        match reg {
            WideRegWithoutSP::BC => self.write_bc(val),
            WideRegWithoutSP::DE => self.write_de(val),
            WideRegWithoutSP::HL => self.set_ptr(val),
            WideRegWithoutSP::AF => self.write_af(val),
        }
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
                let val = self.copy_byte(mem, reg);
                self.a ^= val;
                if self.a == 0 {
                    self.f.z = true;
                }
            }
            ArithmeticOp::XorDirect(_) => todo!(),
            ArithmeticOp::Or(_) => todo!(),
            ArithmeticOp::OrDirect(_) => todo!(),
            ArithmeticOp::Cp(_) => todo!(),
            ArithmeticOp::CpDirect(_) => todo!(),
            ArithmeticOp::Inc(_) => todo!(),
            ArithmeticOp::Dec(reg) => {
                let val = match reg {
                    RegOrPointer::Reg(reg) => {
                        let val = &mut self[reg];
                        *val -= 1;
                        *val
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

    fn ref_byte<'a>(&'a self, mem: &'a MemoryMap, reg: RegOrPointer) -> &'a u8 {
        match reg {
            RegOrPointer::Reg(reg) => &self[reg],
            RegOrPointer::Pointer => &mem[self.ptr()],
        }
    }

    fn ref_mut_byte<'a>(&'a mut self, mem: &'a mut MemoryMap, reg: RegOrPointer) -> &'a mut u8 {
        match reg {
            RegOrPointer::Reg(reg) => &mut self[reg],
            RegOrPointer::Pointer => &mut mem[self.ptr()],
        }
    }

    fn copy_byte(&self, mem: &MemoryMap, reg: RegOrPointer) -> u8 {
        match reg {
            RegOrPointer::Reg(reg) => self[reg],
            RegOrPointer::Pointer => mem[self.ptr()],
        }
    }

    fn update_byte<F>(&mut self, reg: RegOrPointer, mem: &mut MemoryMap, update: F) -> u8
    where
        F: FnOnce(&mut u8),
    {
        let digest = self.ref_mut_byte(mem, reg);
        update(digest);
        *digest
    }

    /// Stores the given byte into either an (half) register or into the MemoryMap using the HL
    /// register as an index.
    fn write_byte(&mut self, reg: RegOrPointer, mem: &mut MemoryMap, val: u8) {
        self.cow_byte(reg, mem, val);
    }

    /// Functions similiarly to `write_byte` but also returns the byte that was modified *after*
    /// the modification.
    fn cow_byte(&mut self, reg: RegOrPointer, mem: &mut MemoryMap, val: u8) -> u8 {
        let digest = self.ref_mut_byte(mem, reg);
        *digest = val;
        *digest
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
                let [hi, lo] = self.pc_bytes();
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
            ControlOp::Halt | ControlOp::Stop(_) => self.done = true,
        }
    }

    fn execute_bit_shift_op(&mut self, op: BitShiftOp, mem: &mut MemoryMap) {
        match op {
            BitShiftOp::Rlc(reg) => {}
            BitShiftOp::Rrc(_) => todo!(),
            BitShiftOp::Rl(reg) => {
                let mut val = self.copy_byte(mem, reg);
                val = val.rotate_left(1);
                let carry = (val & 0x1) == 1;
                val &= (0xFF & self.carry_flag() as u8);
                self.f.c = carry;
                self.f.z = val == 0;
                self.write_byte(reg, mem, val);
            }
            BitShiftOp::Rr(reg) => {
                let mut val = self.copy_byte(mem, reg);
                val = val.rotate_right(1);
                let carry = (val & 0x80) == 1;
                val &= (0xFF & ((self.carry_flag() as u8) << 7));
                self.f.c = carry;
                self.f.z = val == 0;
                self.write_byte(reg, mem, val);
            }
            BitShiftOp::Sla(_) => todo!(),
            BitShiftOp::Sra(_) => todo!(),
            BitShiftOp::Swap(reg) => {
                let val = self.update_byte(reg, mem, |val| {
                    let lw = *val & 0xF0 >> 4;
                    let hi = *val & 0x0F << 4;
                    *val = hi | lw;
                });
                self.f.z = val == 0;
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
            LoadOp::Basic { dest, src } => self.write_byte(dest, mem, self.copy_byte(mem, src)),
            LoadOp::Direct16(reg, val) => self.write_wide_reg(reg, val),
            LoadOp::Direct(reg, val) => self.write_byte(reg, mem, val),
            LoadOp::LoadIntoA(ptr) => {
                self.a = *self.deref_ptr(mem, ptr);
            }
            LoadOp::StoreFromA(ptr) => *self.deref_ptr(mem, ptr) = self.a,
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
                self.write_wide_reg_without_sp(reg, u16::from_le_bytes([a, b]));
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
            LoadOp::LoadHigh(val) => self.a = mem[u16::from_le_bytes([0xFF, val])],
            LoadOp::StoreHigh(val) => mem[u16::from_le_bytes([0xFF, val])] = self.a,
            LoadOp::LoadHighCarry => {
                self.a = mem[u16::from_le_bytes([0xFF, self.carry_flag() as u8])];
            }
            LoadOp::StoreHighCarry => {
                mem[u16::from_le_bytes([0xFF, self.carry_flag() as u8])] = self.a
            }
            LoadOp::LoadA { ptr } => mem[ptr] = self.a,
            LoadOp::StoreA { ptr } => self.a = mem[ptr],
        }
    }

    fn write_af(&mut self, val: u16) {
        let [a, f] = val.to_be_bytes();
        self.a = a;
        self.flags();
        todo!()
    }

    fn bc(&self) -> u16 {
        u16::from_be_bytes([self.b, self.c])
    }

    fn write_bc(&mut self, val: u16) {
        let [b, c] = val.to_be_bytes();
        self.b = b;
        self.c = c;
    }

    fn de(&self) -> u16 {
        u16::from_be_bytes([self.d, self.e])
    }

    fn write_de(&mut self, val: u16) {
        let [d, e] = val.to_be_bytes();
        self.d = d;
        self.e = e;
    }

    fn inc_ptr(&mut self, val: u8) {
        todo!()
    }

    fn dec_ptr(&mut self, val: u8) {
        todo!()
    }

    fn deref_ptr<'a, 'b>(&'a mut self, mem: &'b mut MemoryMap, ptr: LoadAPointer) -> &'b mut u8 {
        match ptr {
            LoadAPointer::BC => &mut mem[self.bc()],
            LoadAPointer::DE => &mut mem[self.de()],
            LoadAPointer::Hli => {
                let digest = &mut mem[self.ptr()];
                self.inc_ptr(1);
                digest
            }
            LoadAPointer::Hld => {
                let digest = &mut mem[self.ptr()];
                self.inc_ptr(1);
                digest
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

impl Index<HalfRegister> for Cpu {
    type Output = u8;

    fn index(&self, index: HalfRegister) -> &Self::Output {
        match index {
            HalfRegister::A => &self.a,
            HalfRegister::B => &self.b,
            HalfRegister::C => &self.c,
            HalfRegister::D => &self.d,
            HalfRegister::E => &self.e,
            HalfRegister::H => &self.h,
            HalfRegister::L => &self.l,
        }
    }
}

impl IndexMut<HalfRegister> for Cpu {
    fn index_mut(&mut self, index: HalfRegister) -> &mut Self::Output {
        match index {
            HalfRegister::A => &mut self.a,
            HalfRegister::B => &mut self.b,
            HalfRegister::C => &mut self.c,
            HalfRegister::D => &mut self.d,
            HalfRegister::E => &mut self.e,
            HalfRegister::H => &mut self.h,
            HalfRegister::L => &mut self.l,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        lookup::{HalfRegister, Instruction, LoadOp, RegOrPointer},
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
            RegOrPointer::Reg(HalfRegister::A),
            RegOrPointer::Reg(HalfRegister::B),
            RegOrPointer::Reg(HalfRegister::C),
            RegOrPointer::Reg(HalfRegister::D),
            RegOrPointer::Reg(HalfRegister::E),
            RegOrPointer::Reg(HalfRegister::H),
            RegOrPointer::Reg(HalfRegister::L),
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
        assert_eq!(cpu.a, u8::MAX);
        assert!(cpu.subtraction_flag(), "{cpu:#X?}");
        assert!(cpu.half_carry_flag(), "{cpu:#X?}");
        cpu.a = 1;
        cpu.execute(Instruction::Cpl, &mut mem);
        assert_eq!(cpu.a, u8::MAX << 1);
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
