use std::{
    borrow::BorrowMut,
    cell::RefCell,
    collections::HashSet,
    hash::{Hash, Hasher},
    num::Wrapping,
    ops::{Add, Index, IndexMut},
    sync::{Mutex, OnceLock},
};

use once_cell::sync::{Lazy, OnceCell};

use crate::{
    lookup::{
        parse_instruction, ArithmeticOp, BitOp, BitOpInner, BitShiftOp, Condition, ControlOp,
        HalfRegister, Instruction, JumpOp, LoadAPointer, LoadOp, RegOrPointer, SomeByte, WideReg,
        WideRegWithoutSP,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Hash)]
pub struct Flags {
    /// The zero flag
    pub z: bool,
    /// The substraction flag
    pub n: bool,
    /// The half-carry flag
    pub h: bool,
    /// The full carry flag
    pub c: bool,
}

impl Flags {
    pub fn set_from_byte(&mut self, val: u8) {
        self.z = check_bit_const::<7>(val);
        self.n = check_bit_const::<6>(val);
        self.h = check_bit_const::<5>(val);
        self.c = check_bit_const::<4>(val);
    }

    pub fn set_for_byte_shift_op(&mut self, z: bool, c: bool) {
        self.z = z;
        self.n = false;
        self.h = false;
        self.c = c;
    }

    pub fn as_byte(&self) -> u8 {
        bool_to_mask::<7>(self.z)
            | bool_to_mask::<6>(self.n)
            | bool_to_mask::<5>(self.h)
            | bool_to_mask::<4>(self.c)
    }
}

#[derive(Debug, Default, Hash, Clone, PartialEq, Eq)]
pub struct Cpu {
    a: Wrapping<u8>,
    f: Flags,
    b: Wrapping<u8>,
    c: Wrapping<u8>,
    d: Wrapping<u8>,
    e: Wrapping<u8>,
    h: Wrapping<u8>,
    l: Wrapping<u8>,
    /// The SP register
    sp: Wrapping<u16>,
    /// The PC register
    pc: Wrapping<u16>,
    allow_interupts: bool,
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

const fn check_bit(bit: u8, src: u8) -> bool {
    let bit = 0x1 << bit;
    (src & bit) == bit
}

const fn select_bit<const B: u8>(src: u8) -> u8 {
    src & bit_select::<B>()
}

const fn check_bit_const<const B: u8>(src: u8) -> bool {
    (src & bit_select::<B>()) == bit_select::<B>()
}

fn addition_operation(val: &mut u8, op: u8, flags: &mut Flags) {
    let (a, carry) = val.overflowing_add(op);
    flags.z = a == 0;
    flags.n = false;
    flags.h = (*val & 0x0F) + (op & 0x0F) > 0x0F;
    flags.c = carry;
    *val = a;
}

fn subtraction_operation(val: &mut u8, op: u8, flags: &mut Flags) {
    let (a, carry) = val.overflowing_sub(op);
    flags.z = a == 0;
    flags.n = true;
    flags.h = (*val & 0x0F).overflowing_sub(op & 0x0F).1;
    flags.c = a > *val;
    *val = a;
}

/// Takes a byte that is in standard binary representation and converts it to binary coded decimal.
fn to_bcd(mut val: u8, flags: &mut Flags) -> u8 {
    if !flags.n {
        // after an addition, adjust if (half-)carry occurred or if result is out of bounds
        if flags.c || val > 0x99 {
            val = val.wrapping_add(0x60);
            flags.c = true;
        }
        if flags.h || (val & 0x0f) > 0x09 {
            val = val.wrapping_add(0x6);
        }
    } else {
        if flags.c {
            val = val.wrapping_sub(0x60);
        }
        if flags.h {
            val = val.wrapping_sub(0x6);
        }
    }
    flags.z = val == 0;
    flags.h = false;
    val
}

impl Cpu {
    /// Constructs a new CPU with each register set to 0.
    pub fn new() -> Self {
        Self {
            ..Default::default()
        }
    }

    /// Get the top four bits of the F register
    pub fn flags(&self) -> &Flags {
        &self.f
    }

    pub fn flags_mut(&mut self) -> &mut Flags {
        &mut self.f
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
        mem.read_op(self.sp.0)
    }

    /// A method similar to `Self::read_op`, but is ran during start up, when the ROM that is
    /// burned-into the CPU is mapped over normal memory.
    pub(crate) fn start_up_read_op(&self, mem: &MemoryMap) -> Instruction {
        println!("Reading start up op from: 0x{:0>4X}", self.pc);
        mem.read_op(self.pc.0)
    }

    pub fn execute(&mut self, instr: Instruction, mem: &mut MemoryMap) {
        // TODO: Remove this! This is onlhy for testing before we impl interrupt handling and IO.
        mem[0xFF0F] = 0b1;
        static LOOP_CHECKER: OnceCell<Mutex<HashSet<u64>>> = OnceCell::new();
        let mut hasher = std::hash::DefaultHasher::new();
        self.hash(&mut hasher);
        instr.hash(&mut hasher);
        mem.hash(&mut hasher);
        let digest = hasher.finish();
        // let digest = (self.clone(), instr.clone(), mem.clone());
        let checker = LOOP_CHECKER.get_or_init(|| Mutex::new(HashSet::new()));
        if !checker.lock().unwrap().insert(digest) {
            panic!("Reached an identical state, which means the GB will loop forever!!");
        }
        // println!("Next op: {instr:X?}");
        println!(
            "Starting execution: PC=0x{:0>4X} SP=0x{:0>4X}",
            self.pc.0, self.sp.0
        );
        let len = instr.size();
        self.pc += (!self.done as u16) * (len as u16);
        match instr {
            Instruction::Load(op) => self.execute_load_op(op, mem),
            Instruction::BitShift(op) => self.execute_bit_shift_op(op, mem),
            Instruction::ControlOp(op) => self.execute_control_op(op, mem),
            Instruction::Bit(op) => self.execute_bit_op(op, mem),
            Instruction::Jump(op) => self.execute_jump_op(op, mem),
            Instruction::Arithmetic(op) => self.execute_arithmetic_op(op, mem),
            Instruction::Daa => self.a = Wrapping(to_bcd(self.a.0, &mut self.f)),
            Instruction::Scf => {
                self.f.n = false;
                self.f.h = false;
                self.f.c = true;
            }
            Instruction::Cpl => {
                self.a = !self.a;
                self.f.n = true;
                self.f.h = true;
            }
            Instruction::Ccf => {
                self.f.n = false;
                self.f.h = false;
                self.f.c = !self.f.c;
            }
            Instruction::Di => self.disable_interupts(),
            Instruction::Ei => self.enable_interupts(),
        }
        println!(
            "Ending execution: PC=0x{:0>4X} SP=0x{:0>4X}",
            self.pc.0, self.sp.0
        );
        println!("");
    }

    fn enable_interupts(&mut self) {
        self.allow_interupts = true;
    }

    fn disable_interupts(&mut self) {
        self.allow_interupts = false;
    }

    fn ptr(&self) -> u16 {
        u16::from_be_bytes([self.h.0, self.l.0])
    }

    fn pc_bytes(&self) -> [u8; 2] {
        u16::to_be_bytes(self.pc.0)
    }

    fn set_ptr(&mut self, val: u16) {
        let [h, l] = u16::to_be_bytes(val).map(Wrapping);
        self.h = h;
        self.l = l;
    }

    fn read_wide_reg(&self, reg: WideReg) -> u16 {
        match reg {
            WideReg::BC => self.bc(),
            WideReg::DE => self.de(),
            WideReg::HL => self.ptr(),
            WideReg::SP => self.sp.0,
        }
    }

    fn write_wide_reg(&mut self, reg: WideReg, val: u16) {
        match reg {
            WideReg::BC => self.write_bc(val),
            WideReg::DE => self.write_de(val),
            WideReg::HL => self.set_ptr(val),
            WideReg::SP => self.sp = Wrapping(val),
        }
    }

    fn update_wide_reg<F>(&mut self, reg: WideReg, update: F)
    where
        F: FnOnce(&mut u16),
    {
        let mut value = self.read_wide_reg(reg);
        update(&mut value);
        self.write_wide_reg(reg, value);
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
            ArithmeticOp::AddSP(val) => {
                let (sp, carry) = self.sp.0.overflowing_add_signed(val as i16);
                let h = if val < 0 {
                    (sp & 0x0F) < (val.abs() as u16 & 0x0F)
                } else {
                    ((sp & 0x0F) as u8) + ((val.abs() as u8) & 0x0F) > 0x0F
                };
                self.f.z = false;
                self.f.n = false;
                self.f.h = h;
                self.f.c = carry;
                self.sp = Wrapping(sp);
            }
            ArithmeticOp::Inc16(reg) => self.update_wide_reg(reg, |value| *value += 1),
            ArithmeticOp::Dec16(reg) => self.update_wide_reg(reg, |value| *value -= 1),
            ArithmeticOp::Add16(reg) => {
                let value = self.read_wide_reg(reg);
                let ptr = self.ptr();
                self.f.h = (ptr & 0x0F) + (value & 0x0F) > 0x0F;
                let (ptr, carry) = ptr.overflowing_add(value);
                self.set_ptr(ptr);
                self.f.n = false;
                self.f.c = carry;
            }
            ArithmeticOp::Add(byte) => {
                let byte = self.unwrap_some_byte(mem, byte);
                addition_operation(&mut self.a.0, byte, &mut self.f);
            }
            ArithmeticOp::Adc(byte) => {
                let byte = self.unwrap_some_byte(mem, byte);
                let (byte, carry) = byte.overflowing_add(self.f.c as u8);
                addition_operation(&mut self.a.0, byte, &mut self.f);
                self.f.c |= carry;
            }
            ArithmeticOp::Sub(byte) => {
                let byte = self.unwrap_some_byte(mem, byte);
                subtraction_operation(&mut self.a.0, byte, &mut self.f);
            }
            ArithmeticOp::Sbc(byte) => {
                let byte = self.unwrap_some_byte(mem, byte);
                let (byte, _) = byte.overflowing_add(self.f.c as u8);
                subtraction_operation(&mut self.a.0, byte, &mut self.f);
            }
            ArithmeticOp::And(byte) => {
                let byte = self.unwrap_some_byte(mem, byte);
                self.a &= byte;
                self.f.z = self.a.0 == 0;
                self.f.n = false;
                self.f.h = true;
                self.f.c = false;
            }
            ArithmeticOp::Xor(byte) => {
                let byte = self.unwrap_some_byte(mem, byte);
                self.a ^= byte;
                self.f.z = self.a.0 == 0;
                self.f.n = false;
                self.f.h = false;
                self.f.c = false;
            }
            ArithmeticOp::Or(byte) => {
                let byte = self.unwrap_some_byte(mem, byte);
                self.a |= byte;
                self.f.z = self.a.0 == 0;
                self.f.n = false;
                self.f.h = false;
                self.f.c = false;
            }
            ArithmeticOp::Cp(byte) => {
                let byte = self.unwrap_some_byte(mem, byte);
                let mut a = self.a;
                println!("Comparing: A=0x{a:0>2X} byte=0x{byte:0>2X}");
                subtraction_operation(&mut a.0, byte, &mut self.f);
            }
            ArithmeticOp::Inc(reg) => {
                let mut h = false;
                let val = self.update_byte(reg, mem, |byte| {
                    h = *byte & 0x0F == 0x0F;
                    *byte += 1
                });
                self.f.z = val == 0;
                self.f.n = false;
                self.f.h = h;
            }
            ArithmeticOp::Dec(reg) => {
                let mut h = false;
                let val = self.update_byte(reg, mem, |byte| {
                    h = *byte == 0;
                    *byte -= 1
                });
                self.f.z = val == 0;
                self.f.n = true;
                self.f.h = h;
            }
        }
    }

    fn ref_byte<'a>(&'a self, mem: &'a MemoryMap, reg: RegOrPointer) -> &'a u8 {
        match reg {
            RegOrPointer::Reg(reg) => &self[reg].0,
            RegOrPointer::Pointer => &mem[self.ptr()],
        }
    }

    fn ref_mut_byte<'a>(&'a mut self, mem: &'a mut MemoryMap, reg: RegOrPointer) -> &'a mut u8 {
        match reg {
            RegOrPointer::Reg(reg) => &mut self[reg].0,
            RegOrPointer::Pointer => &mut mem[self.ptr()],
        }
    }

    fn unwrap_some_byte(&self, mem: &MemoryMap, byte: SomeByte) -> u8 {
        match byte {
            SomeByte::Direct(byte) => byte,
            SomeByte::Referenced(reg) => self.copy_byte(mem, reg),
        }
    }

    fn copy_byte(&self, mem: &MemoryMap, reg: RegOrPointer) -> u8 {
        match reg {
            RegOrPointer::Reg(reg) => self[reg].0,
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
            Condition::Zero => self.f.z,
            Condition::NotZero => !self.f.z,
            Condition::Carry => self.f.c,
            Condition::NotCarry => !self.f.c,
        }
    }

    fn execute_jump_op(&mut self, op: JumpOp, mem: &mut MemoryMap) {
        fn rst<const N: u16>(cpu: &mut Cpu, mem: &mut MemoryMap) {
            cpu.sp -= 1;
            let [p, c] = cpu.pc_bytes();
            mem[cpu.sp.0] = p;
            cpu.sp -= 1;
            mem[cpu.sp.0] = c;
            cpu.pc = Wrapping(N);
        }
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
            JumpOp::Relative(val) => {
                if val < 0 {
                    self.pc -= val.abs() as u16;
                } else {
                    self.pc += val as u16;
                }
            }
            JumpOp::ConditionalAbsolute(cond, dest) => {
                if self.matches(cond) {
                    self.pc = Wrapping(dest);
                }
            }
            JumpOp::Absolute(dest) => self.pc = Wrapping(dest),
            JumpOp::JumpToHL => self.pc = Wrapping(self.ptr()),
            JumpOp::Call(ptr) => {
                println!("Calling subroutine.. storing PC: 0x{:0>4X}", self.pc.0);
                let [hi, lo] = self.pc_bytes();
                self.sp -= 1;
                mem[self.sp.0] = hi;
                self.sp -= 1;
                mem[self.sp.0] = lo;
                self.pc = Wrapping(ptr);
            }
            JumpOp::ConditionalCall(cond, dest) => {
                if self.matches(cond) {
                    let [hi, lo] = self.pc_bytes();
                    self.sp -= 1;
                    mem[self.sp.0] = hi;
                    self.sp -= 1;
                    mem[self.sp.0] = lo;
                    self.pc = Wrapping(dest);
                }
            }
            JumpOp::Return => {
                let lo = mem[self.sp.0];
                self.sp += 1;
                let hi = mem[self.sp.0];
                self.sp += 1;
                let pc = u16::from_be_bytes([hi, lo]);
                println!("Returning from subroutine.. loading PC: 0x{pc:0>4X}");
                self.pc = Wrapping(pc);
            }
            JumpOp::ConditionalReturn(cond) => {
                if self.matches(cond) {
                    let lo = mem[self.sp.0];
                    self.sp += 1;
                    let hi = mem[self.sp.0];
                    self.sp += 1;
                    self.pc = Wrapping(u16::from_be_bytes([hi, lo]));
                }
            }
            JumpOp::ReturnAndEnable => {
                self.enable_interupts();
                let lo = mem[self.sp.0];
                self.sp += 1;
                let hi = mem[self.sp.0];
                self.sp += 1;
                self.pc = Wrapping(u16::from_be_bytes([hi, lo]));
            }
            JumpOp::RST00 => rst::<0x00>(self, mem),
            JumpOp::RST08 => rst::<0x08>(self, mem),
            JumpOp::RST10 => rst::<0x10>(self, mem),
            JumpOp::RST18 => rst::<0x18>(self, mem),
            JumpOp::RST20 => rst::<0x20>(self, mem),
            JumpOp::RST28 => rst::<0x28>(self, mem),
            JumpOp::RST30 => rst::<0x30>(self, mem),
            JumpOp::RST38 => rst::<0x38>(self, mem),
        }
    }

    fn execute_control_op(&mut self, op: ControlOp, mem: &mut MemoryMap) {
        match op {
            ControlOp::Noop => {}
            ControlOp::Halt | ControlOp::Stop(_) => self.done = true,
        }
    }

    fn execute_bit_op(&mut self, op: BitOp, mem: &mut MemoryMap) {
        let BitOp { bit, reg, op } = op;
        debug_assert!(bit < 8);
        let byte = self.copy_byte(mem, reg);
        match op {
            BitOpInner::Bit => {
                self.f.z = !check_bit(bit, byte);
                self.f.n = false;
                self.f.h = true;
            }
            BitOpInner::Res => {
                self.update_byte(reg, mem, |byte| *byte &= !(0x1 << bit));
            }
            BitOpInner::Set => {
                self.update_byte(reg, mem, |byte| *byte |= 0x1 << bit);
            }
        }
    }

    fn execute_bit_shift_op(&mut self, op: BitShiftOp, mem: &mut MemoryMap) {
        match op {
            BitShiftOp::Rlc(reg) => {
                let mut carry = false;
                let byte = self.update_byte(reg, mem, |byte| {
                    let [c, new] = u16::from_be_bytes([0, *byte]).rotate_left(1).to_be_bytes();
                    carry = c == 1;
                    *byte = c | new;
                });
                self.f.set_for_byte_shift_op(byte != 0, carry)
            }
            BitShiftOp::Rlca => {
                let byte = self.a.0;
                let [c, new] = u16::from_be_bytes([0, byte]).rotate_left(1).to_be_bytes();
                self.a = Wrapping(c | new);
                self.f.set_for_byte_shift_op(byte != 0, c == 1)
            }
            BitShiftOp::Rrc(reg) => {
                let mut carry = false;
                let byte = self.update_byte(reg, mem, |byte| {
                    let [new, c] = u16::from_be_bytes([*byte, 0]).rotate_right(1).to_be_bytes();
                    carry = c == 0b1000_0000;
                    *byte = c | new;
                });
                self.f.set_for_byte_shift_op(byte != 0, carry)
            }
            BitShiftOp::Rrca => {
                let mut carry = false;
                let byte = self.a.0;
                let [new, c] = u16::from_be_bytes([byte, 0]).rotate_right(1).to_be_bytes();
                carry = c == 0x80;
                self.a = Wrapping(c | new);
                self.f.set_for_byte_shift_op(byte != 0, carry)
            }
            BitShiftOp::Rl(reg) => {
                let byte = self.copy_byte(mem, reg);
                let carry = self.f.c as u8;
                let mask = carry | (carry << 7);
                println!("Starting rotate left operation: mask=0x{mask:0>2X} byte=0x{byte:0>2X}");
                let mut new_carry = false;
                let byte = self.update_byte(reg, mem, |byte| {
                    let [c, new] = (u16::from_be_bytes([mask, *byte]).rotate_left(1)).to_be_bytes();
                    new_carry = c & 1 != 0;
                    *byte = new;
                });
                self.f.set_for_byte_shift_op(byte != 0, new_carry);
                println!(
                    "Ending rotate left operation: carry={} byte=0x{byte:0>2X}",
                    self.f.c
                );
            }
            BitShiftOp::Rla => {
                let byte = self.a.0;
                let carry = self.f.c as u8;
                let mask = carry | (carry << 7);
                println!(
                    "Starting rotate left operation on A: mask=0x{mask:0>2X} byte=0x{byte:0>2X}"
                );
                let [c, new] = (u16::from_be_bytes([mask, byte]).rotate_left(1)).to_be_bytes();
                self.a = Wrapping(new);
                self.f.set_for_byte_shift_op(new != 0, c & 1 != 0);
                println!(
                    "Ending rotate left operation on A: carry={} byte=0x{new:0>2X}",
                    self.f.c
                );
            }
            BitShiftOp::Rr(reg) => {
                let mask = self.f.c as u8;
                let mut new_carry = false;
                let byte = self.update_byte(reg, mem, |byte| {
                    let [new, c] = u16::from_be_bytes([*byte, mask])
                        .rotate_right(1)
                        .to_be_bytes();
                    new_carry = c == 0x80;
                    *byte = new;
                });
                self.f.set_for_byte_shift_op(byte != 0, new_carry)
            }
            BitShiftOp::Rra => {
                let mask = self.f.c as u8;
                let byte = self.a.0;
                let [new, c] = u16::from_be_bytes([byte, mask])
                    .rotate_right(1)
                    .to_be_bytes();
                self.a = Wrapping(new);
                self.f.set_for_byte_shift_op(new != 0, c == 0x80)
            }
            BitShiftOp::Sla(reg) => {
                let mut new_carry = false;
                let byte = self.update_byte(reg, mem, |byte| {
                    let [c, new] = u16::from_be_bytes([0, *byte]).rotate_left(1).to_be_bytes();
                    new_carry = c != 0;
                    *byte = new;
                });
                self.f.set_for_byte_shift_op(byte != 0, new_carry)
            }
            BitShiftOp::Sra(reg) => {
                let mut carry = false;
                let byte = self.update_byte(reg, mem, |byte| {
                    let bit = select_bit::<7>(*byte);
                    let [new, c] = u16::from_be_bytes([*byte, 0]).rotate_right(1).to_be_bytes();
                    carry = c == 0x80;
                    *byte = new | bit;
                });
                self.f.set_for_byte_shift_op(byte != 0, carry)
            }
            BitShiftOp::Swap(reg) => {
                let byte = self.update_byte(reg, mem, |byte| {
                    let lw = (*byte & 0xF0) >> 4;
                    let hi = (*byte & 0x0F) << 4;
                    *byte = hi | lw;
                });
                self.f.set_for_byte_shift_op(byte != 0, false)
            }
            BitShiftOp::Srl(reg) => {
                let mut carry = false;
                let byte = self.update_byte(reg, mem, |byte| {
                    let [new, c] = u16::from_be_bytes([*byte, 0]).rotate_right(1).to_be_bytes();
                    carry = c == 0x80;
                    *byte = new;
                });
                self.f.set_for_byte_shift_op(byte != 0, carry)
            }
        }
    }

    fn execute_load_op(&mut self, op: LoadOp, mem: &mut MemoryMap) {
        match op {
            LoadOp::Basic {
                dest: RegOrPointer::Pointer,
                src: RegOrPointer::Pointer,
            } => {
                unreachable!("This should be encoded as a HALT op")
            }
            LoadOp::Basic { dest, src } => self.write_byte(dest, mem, self.copy_byte(mem, src)),
            LoadOp::Direct16(reg, val) => self.write_wide_reg(reg, val),
            LoadOp::Direct(reg, val) => self.write_byte(reg, mem, val),
            LoadOp::LoadIntoA(ptr) => {
                self.a = Wrapping(*self.deref_ptr(mem, ptr));
            }
            LoadOp::StoreFromA(ptr) => *self.deref_mut_ptr(mem, ptr) = self.a.0,
            LoadOp::StoreSP(val) => {
                let [lw, hi] = self.sp.0.to_le_bytes();
                mem[val] = lw;
                mem[val + 1] = hi;
            }
            LoadOp::HLIntoSP => self.sp = Wrapping(self.ptr()),
            LoadOp::SPIntoHL(val) => {
                let [h, l] = if val >= 0 {
                    self.sp + Wrapping(val as u16)
                } else {
                    self.sp - Wrapping(val as u16)
                }
                .0
                .to_be_bytes()
                .map(Wrapping);
                self.h = h;
                self.l = l;
            }
            LoadOp::Pop(reg) => {
                let a = mem[self.sp.0];
                self.sp += 1;
                let b = mem[self.sp.0];
                self.sp += 1;
                self.write_wide_reg_without_sp(reg, u16::from_le_bytes([a, b]));
            }
            LoadOp::Push(reg) => {
                let [a, b] = match reg {
                    WideRegWithoutSP::BC => [self.b.0, self.c.0],
                    WideRegWithoutSP::DE => [self.d.0, self.e.0],
                    WideRegWithoutSP::HL => [self.h.0, self.l.0],
                    WideRegWithoutSP::AF => [self.a.0, self.f.as_byte()],
                };
                println!("Pushing ptr onto the stack: 0x{a:0>2X}{b:0>2X}");
                self.sp -= 1;
                mem[self.sp.0] = a;
                self.sp -= 1;
                mem[self.sp.0] = b;
            }
            LoadOp::LoadHigh(val) => self.a = Wrapping(mem[u16::from_le_bytes([0xFF, val])]),
            LoadOp::StoreHigh(val) => mem[u16::from_le_bytes([0xFF, val])] = self.a.0,
            LoadOp::LoadHighCarry => mem[u16::from_be_bytes([0xFF, self.c.0])] = self.a.0,
            LoadOp::StoreHighCarry => {
                self.a = Wrapping(mem[u16::from_be_bytes([0xFF, self.c.0])]);
            }
            LoadOp::LoadA { ptr } => mem[ptr] = self.a.0,
            LoadOp::StoreA { ptr } => self.a = Wrapping(mem[ptr]),
        }
    }

    fn write_af(&mut self, val: u16) {
        let [a, f] = val.to_be_bytes();
        self.a = Wrapping(a);
        self.f.set_from_byte(f);
    }

    fn bc(&self) -> u16 {
        u16::from_be_bytes([self.b.0, self.c.0])
    }

    fn write_bc(&mut self, val: u16) {
        let [b, c] = val.to_be_bytes().map(Wrapping);
        self.b = b;
        self.c = c;
    }

    fn de(&self) -> u16 {
        u16::from_be_bytes([self.d.0, self.e.0])
    }

    fn write_de(&mut self, val: u16) {
        let [d, e] = val.to_be_bytes().map(Wrapping);
        self.d = d;
        self.e = e;
    }

    fn inc_ptr(&mut self, val: u16) {
        let [h, l] = (self.ptr() + val).to_be_bytes().map(Wrapping);
        self.h = h;
        self.l = l;
    }

    fn dec_ptr(&mut self, val: u16) {
        let [h, l] = (self.ptr() - val).to_be_bytes().map(Wrapping);
        self.h = h;
        self.l = l;
    }

    fn deref_ptr<'a, 'b>(&'a mut self, mem: &'b mut MemoryMap, ptr: LoadAPointer) -> &'b u8 {
        match ptr {
            LoadAPointer::BC => &mem[self.bc()],
            LoadAPointer::DE => &mem[self.de()],
            LoadAPointer::Hli => {
                let digest = &mem[self.ptr()];
                self.inc_ptr(1);
                digest
            }
            LoadAPointer::Hld => {
                let digest = &mem[self.ptr()];
                self.inc_ptr(1);
                digest
            }
        }
    }

    fn deref_mut_ptr<'a, 'b>(
        &'a mut self,
        mem: &'b mut MemoryMap,
        ptr: LoadAPointer,
    ) -> &'b mut u8 {
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
        // println!("Applied startup op: {:?}", self);
        (self.sp.0 != 0x0100).then(|| self.start_up_read_op(mem))
    }
}

impl Index<HalfRegister> for Cpu {
    type Output = Wrapping<u8>;

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
    use std::num::Wrapping;

    use crate::{
        lookup::{BitShiftOp, HalfRegister, Instruction, LoadOp, RegOrPointer},
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
        assert_eq!(cpu.pc.0, 0);
        cpu.execute(Instruction::Cpl, &mut mem);
        assert_eq!(cpu.pc.0, 1);
        assert_eq!(cpu.a.0, u8::MAX);
        assert!(cpu.subtraction_flag(), "{cpu:#X?}");
        assert!(cpu.half_carry_flag(), "{cpu:#X?}");
        cpu.a = Wrapping(1);
        cpu.execute(Instruction::Cpl, &mut mem);
        assert_eq!(cpu.a.0, u8::MAX << 1);
        assert_eq!(cpu.pc.0, 2);
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

    #[test]
    fn test_bit_shift_ops() {
        let mut cpu = Cpu::default();
        let mut mem = MemoryMap::construct();
        let mem = &mut mem;
        // Testing RL* ops
        for (i, b) in [false, true]
            .into_iter()
            .flat_map(|b| (0u8..=u8::MAX).map(move |i| (i, b)))
        {
            // RLA section
            cpu.f.c = b;
            cpu.a = Wrapping(i);
            let instr = Instruction::BitShift(BitShiftOp::Rla);
            cpu.execute(instr, mem);
            let a = (i << 1) | b as u8;
            let carry = (i & 0x80) == 0x80;
            assert_eq!(cpu.a.0, a);
            assert_eq!(cpu.f.c, carry);
            // RL section
            cpu.b = Wrapping(i);
            cpu.f.c = b;
            let instr = Instruction::BitShift(BitShiftOp::Rl(RegOrPointer::Reg(HalfRegister::B)));
            cpu.execute(instr, mem);
            assert_eq!(cpu.b.0, a);
            assert_eq!(cpu.f.c, carry);
            // RLCA section
            cpu.a = Wrapping(i);
            cpu.f.c = b;
            let instr = Instruction::BitShift(BitShiftOp::Rlca);
            cpu.execute(instr, mem);
            let a = i.rotate_left(1);
            let carry = (i & 0x80) == 0x80;
            assert_eq!(cpu.a.0, a);
            assert_eq!(cpu.f.c, carry);
            // RLC section
            cpu.b = Wrapping(i);
            cpu.f.c = b;
            let instr = Instruction::BitShift(BitShiftOp::Rlc(RegOrPointer::Reg(HalfRegister::B)));
            cpu.execute(instr, mem);
            assert_eq!(cpu.b.0, a);
            assert_eq!(cpu.f.c, carry);
        }
        // Testing RR* ops
        for (i, b) in [false, true]
            .into_iter()
            .flat_map(|b| (0u8..=u8::MAX).map(move |i| (i, b)))
        {
            // Rra section
            cpu.f.c = b;
            cpu.a = Wrapping(i);
            let instr = Instruction::BitShift(BitShiftOp::Rra);
            cpu.execute(instr, mem);
            let a = (i >> 1) | (0x80 * (b as u8));
            let carry = (i & 0x01) == 0x01;
            assert_eq!(cpu.a.0, a);
            assert_eq!(cpu.f.c, carry);
            // Rr section
            cpu.b = Wrapping(i);
            cpu.f.c = b;
            let instr = Instruction::BitShift(BitShiftOp::Rr(RegOrPointer::Reg(HalfRegister::B)));
            cpu.execute(instr, mem);
            assert_eq!(cpu.b.0, a);
            assert_eq!(cpu.f.c, carry);
            // RRCA section
            cpu.a = Wrapping(i);
            cpu.f.c = b;
            let instr = Instruction::BitShift(BitShiftOp::Rrca);
            cpu.execute(instr, mem);
            let a = i.rotate_right(1);
            let carry = (i & 0x01) == 1;
            assert_eq!(cpu.a.0, a);
            assert_eq!(cpu.f.c, carry);
            // RRC section
            cpu.b = Wrapping(i);
            cpu.f.c = b;
            let instr = Instruction::BitShift(BitShiftOp::Rrc(RegOrPointer::Reg(HalfRegister::B)));
            cpu.execute(instr, mem);
            assert_eq!(cpu.b.0, a);
            assert_eq!(cpu.f.c, carry);
        }
        // Testing Shift ops
        for i in 0u8..=u8::MAX {
            // SLA section
            cpu.a = Wrapping(i);
            let instr = Instruction::BitShift(BitShiftOp::Sla(RegOrPointer::Reg(HalfRegister::A)));
            cpu.execute(instr, mem);
            let a = i << 1;
            let carry = (i & 0x80) == 0x80;
            assert_eq!(cpu.a.0, a);
            assert_eq!(cpu.f.c, carry);
            // SRA section
            cpu.a = Wrapping(i);
            let instr = Instruction::BitShift(BitShiftOp::Sra(RegOrPointer::Reg(HalfRegister::A)));
            cpu.execute(instr, mem);
            let a = (i & 0x80) | (i >> 1);
            let carry = (i & 0x01) == 0x01;
            assert_eq!(cpu.a.0, a);
            assert_eq!(cpu.f.c, carry);
            // SRL section
            cpu.a = Wrapping(i);
            let instr = Instruction::BitShift(BitShiftOp::Srl(RegOrPointer::Reg(HalfRegister::A)));
            cpu.execute(instr, mem);
            let a = i >> 1;
            let carry = (i & 0x01) == 0x01;
            assert_eq!(cpu.a.0, a);
            assert_eq!(cpu.f.c, carry);
        }
        // Testing SWAP op
        for i in 0u8..=u8::MAX {
            cpu.a = Wrapping(i);
            let instr = Instruction::BitShift(BitShiftOp::Swap(RegOrPointer::Reg(HalfRegister::A)));
            cpu.execute(instr, mem);
            let a = ((i & 0x0F) << 4) | ((i & 0xF0) >> 4);
            assert_eq!(cpu.a.0, a);
            assert!(!cpu.f.c);
        }
    }
}
