#![allow(dead_code, unused)]

use std::fmt::Debug;
use std::hash::Hash;
use std::ops::{Index, IndexMut};

mod mbc1;
use std::borrow::Cow;

pub use mbc1::*;
mod mbc2;
pub use mbc2::*;
mod mbc3;
pub use mbc3::*;
mod mbc5;
pub use mbc5::*;

use crate::cpu::check_bit_const;
use crate::lookup::{parse_instruction, Instruction, InterruptOp};
use crate::{ButtonInput, JoypadInput, SsabInput};

pub static START_UP_HEADER: &[u8; 0x900] = include_bytes!("../cgb.bin");

pub type StartUpHeaders = ([u8; 0x100], [u8; 0x700]);

static NINTENDO_LOGO: &[u8] = &[
    0xCE, 0xED, 0x66, 0x66, 0xCC, 0x0D, 0x00, 0x0B, 0x03, 0x73, 0x00, 0x83, 0x00, 0x0C, 0x00, 0x0D,
    0x00, 0x08, 0x11, 0x1F, 0x88, 0x89, 0x00, 0x0E, 0xDC, 0xCC, 0x6E, 0xE6, 0xDD, 0xDD, 0xD9, 0x99,
    0xBB, 0xBB, 0x67, 0x63, 0x6E, 0x0E, 0xEC, 0xCC, 0xDD, 0xDC, 0x99, 0x9F, 0xBB, 0xB9, 0x33, 0x3E,
];

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct MemoryMap {
    // The MBC
    mbc: MemoryBankController,
    // The video RAM
    vram: [u8; 0x2000],
    // The working RAM
    wram: ([u8; 0x1000], [u8; 0x1000]),
    // The Object attribute map
    oam: [u8; 0x100],
    io: IoRegisters,
    // High RAM
    pub(crate) hr: [u8; 0x7F],
    /// The interrupt enable register. Bits 0-4 flag where or not certain interrupt handlers can be
    /// called.
    ///  - Bit 0 corresponds to the VBlank interrupt
    ///  - Bit 1 corresponds to the LCD interrupt
    ///  - Bit 2 corresponds to the timer interrupt
    ///  - Bit 3 corresponds to the serial interrupt
    ///  - Bit 4 corresponds to the joypad interrupt
    /// When intexed, this register is at 0xFFFF.
    ie: u8,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq, Default)]
struct IoRegisters {
    /// ADDR FF00
    joypad: Joypad,
    /// ADDR FF01, FF02
    serial: (u8, u8),
    /// ADDR FF04, FF05, FF06, FF07
    /// There are the (divider, timer, timer modulo, tac)
    timer_div: [u8; 4],
    /// The counter for the timer controller
    tac: TimerControl,
    /// ADDR FF0F
    interrupt_flags: u8,
    /// ADDR FF10-FF26
    audio: [u8; 16],
    /// ADDR FF30-FF3F
    wave: [u8; 0x10],
    /// ADDR FF40-FF4B
    lcd: [u8; 0xC],
    /// ADDR FF4F
    vram_select: u8,
    /// ADDR FF50
    boot_status: u8,
    /// ADDR FF51-FF55
    vram_dma: [u8; 5],
    /// ADDR FF68-FF6B
    palettes: [u8; 4],
    /// ADDR FF70
    wram_select: u8,
    /// There are gaps amount the memory mapped IO registers. Any index into this that hits one of
    /// these gaps resets the value. Notably, this is also used when mutably indexing to the
    /// divider register but not while immutably indexing.
    /// ADDR FF03, FF08-FF0E, FF27-FF29, FF4C-FF4E, FF56-FF67, FF6C-FF6F
    dead_byte: u8,
}

/// This is a bit messy... The bottom half of this register is read-only via instruction and is
/// set at the same time the joypad interrupt is called. We can't expose this byte when mutably
/// indexing into, so we have to store two bytes. How do we keep them synchronized?
/// Let's say they are in sync and have the state 0b01_0000.
/// Then, the right button is press. An interrupt is fired and can write to both bytes.
/// Great, the current state is 0b010_0001 for both bytes.
/// Then, the CPU indexs to change the select-mode bytes.
/// Now, they are out of sync (because we only expose the byte that contains the select-mode bits,
/// so only the gets mutated). This causes a problem when you next read from this register. We
/// can't expose the byte that holds the read-only nibble, its out of date. But, we can't expose
/// the byte with the select-mode bits because the lower half could have been written to. Moreover,
/// neither byte can be mutated (safely) and we need an actual byte to return a reference to.
///
/// The solution: Both registers will be synchronized when a tick is processed. Currently, this
/// happens immediately before the instruction is actually processed, but this would also work if
/// it happened immediately after too. As long as the tick is applied in the same order everywhere.
#[derive(Debug, Default, PartialEq, Eq, Hash, Clone)]
struct Joypad {
    main: u8,
    dup: u8,
}

impl Joypad {
    /// Method is called when the memory is ticked. Because we have to virtualize the read-only
    /// nibble, we have to ensure the main and duplicate bits are synced. Additionally, there is
    /// this bit from the pandocs:
    /// > If neither buttons nor d-pad is selected ($30 was written), then the low nibble reads $F (all buttons released).
    /// Accounting for this is done here.
    fn tick(&mut self) {
        // The main byte is the byte that gets referenced when indexing. This means that any
        // changes to its lower half (the read-only nibble) must be ignored. These bits are stored
        // in the dup nibble. This method brings them in sync by overwriting the lower nibble of
        // the main byte with the lower nibble of the dup byte. At the same time, it is calculated
        // if both or neither of the select-mode bits are set (see the quote above). In this case,
        // the main bit is ORed with 0x0F.
        // NOTE: dup is never updated except when input is registered.
        self.main &= 0x30; // Take away everything but the select-mode bits
        let mut mask = self.main;
        mask = 0x10 & !(((mask & 0x20) >> 1) ^ (mask & 0x10)); // tmp is now either 0x10 or 0x00
        mask = mask.saturating_sub(1); // tmp is now either 0x0F or 0x00
        self.main |= mask | self.dup;
    }

    fn register_input(&mut self, input: ButtonInput) {
        let byte = match input {
            ButtonInput::Joypad(button)
                if check_bit_const::<4>(self.main) && !check_bit_const::<5>(self.main) =>
            {
                button as u8
            }
            ButtonInput::Ssab(button)
                if !check_bit_const::<4>(self.main) && check_bit_const::<5>(self.main) =>
            {
                button as u8
            }
            _ => return,
        };
        debug_assert_eq!(byte.count_ones(), 1);
        debug_assert!(byte < 0xF0);
        self.dup &= byte;
        self.main &= byte;
    }
}

impl Index<()> for Joypad {
    type Output = u8;

    fn index(&self, index: ()) -> &Self::Output {
        &self.main
    }
}

impl IndexMut<()> for Joypad {
    fn index_mut(&mut self, index: ()) -> &mut Self::Output {
        &mut self.main
    }
}

impl IoRegisters {
    fn tick(&mut self) {
        self.joypad.tick();
        let byte = &mut self.timer_div[0];
        *byte = byte.wrapping_add(1);
        if self.tac.update_and_tick(self.timer_div[3]) {
            match self.timer_div[1].checked_add(1) {
                Some(b) => self.timer_div[1] = b,
                None => {
                    let b = self.timer_div[1];
                    self.timer_div[0] = b;
                    self.interrupt_flags |= 0b100;
                }
            }
        }
    }
}

impl Index<u16> for IoRegisters {
    type Output = u8;

    fn index(&self, index: u16) -> &Self::Output {
        match index {
            0xFF00 => &self.joypad[()],
            0xFF01 => &self.serial.0,
            0xFF02 => &self.serial.1,
            n @ 0xFF04..=0xFF07 => &self.timer_div[(n - 0xFF04) as usize],
            0xFF0F => &self.interrupt_flags,
            n @ 0xFF10..=0xFF26 => &self.audio[(n - 0xFF10) as usize],
            n @ 0xFF30..=0xFF3F => &self.wave[(n - 0xFF30) as usize],
            n @ 0xFF40..=0xFF4B => &self.lcd[(n - 0xFF40) as usize],
            0xFF4F => &self.vram_select,
            0xFF50 => &self.boot_status,
            n @ 0xFF51..=0xFF55 => &self.vram_dma[(n - 0xFF51) as usize],
            n @ 0xFF68..=0xFF6B => &self.palettes[(n - 0xFF68) as usize],
            0xFF70 => &self.wram_select,
            0xFF03
            | 0xFF08..=0xFF0E
            | 0xFF27..=0xFF2F
            | 0xFF4C..=0xFF4E
            | 0xFF56..=0xFF67
            | 0xFF6C..=0xFF6F => &self.dead_byte,
            ..=0xFEFF | 0xFF71.. => unreachable!(
                "The MemoryMap should never index into the IO registers outside of 0xFF00-0xFF70!"
            ),
        }
    }
}

impl IndexMut<u16> for IoRegisters {
    fn index_mut(&mut self, index: u16) -> &mut Self::Output {
        match index {
            0xFF00 => &mut self.joypad[()],
            0xFF01 => &mut self.serial.0,
            0xFF02 => &mut self.serial.1,
            0xFF04 => {
                self.timer_div[0] = 0;
                self.dead_byte = 0;
                &mut self.dead_byte
            }
            n @ 0xFF05..=0xFF07 => &mut self.timer_div[(n - 0xFF04) as usize],
            0xFF0F => &mut self.interrupt_flags,
            n @ 0xFF10..=0xFF26 => &mut self.audio[(n - 0xFF10) as usize],
            n @ 0xFF30..=0xFF3F => &mut self.wave[(n - 0xFF30) as usize],
            n @ 0xFF40..=0xFF4B => &mut self.lcd[(n - 0xFF40) as usize],
            0xFF4F => &mut self.vram_select,
            0xFF50 => &mut self.boot_status,
            n @ 0xFF51..=0xFF55 => &mut self.vram_dma[(n - 0xFF51) as usize],
            n @ 0xFF68..=0xFF6B => &mut self.palettes[(n - 0xFF68) as usize],
            0xFF70 => &mut self.wram_select,
            0xFF03
            | 0xFF08..=0xFF0E
            | 0xFF27..=0xFF2F
            | 0xFF4C..=0xFF4E
            | 0xFF56..=0xFF67
            | 0xFF6C..=0xFF6F => &mut self.dead_byte,
            ..=0xFEFF | 0xFF71.. => unreachable!(
                "The MemoryMap should never index into the IO registers outside of 0xFF00-0xFF70!"
            ),
        }
    }
}

impl MemoryMap {
    pub fn new<'a, C: Into<Cow<'a, [u8]>>>(cart: C) -> Self {
        Self {
            mbc: MemoryBankController::new(cart),
            vram: [0; 0x2000],
            wram: ([0; 0x1000], [0; 0x1000]),
            oam: [0; 0x100],
            io: IoRegisters::default(),
            hr: [0; 0x7F],
            ie: 0,
        }
    }

    pub(crate) fn start_up_remap(&mut self) -> StartUpHeaders {
        let mut digest = ([0; 0x100], [0; 0x700]);
        for i in 0..=0xFF {
            digest.0[i] = START_UP_HEADER[i];
            self.mbc.direct_overwrite(i as u16, &mut digest.0[i]);
        }
        for i in 0..=0x6FF {
            digest.1[i] = START_UP_HEADER[i + 0x200];
            self.mbc
                .direct_overwrite((i + 0x200) as u16, &mut digest.1[i]);
        }
        digest
    }

    pub(crate) fn start_up_unmap(&mut self, mut headers: StartUpHeaders) {
        for i in 0..=0xFF {
            self.mbc.direct_overwrite(i as u16, &mut headers.0[i]);
        }
        for i in 0..=0x6FF {
            self.mbc
                .direct_overwrite((i + 0x200) as u16, &mut headers.1[i]);
        }
    }

    /// Reads the next operation to be performed. If the given IME flag is true, and an interrupts
    /// has been requested, the returned instruction will be a `call` to the corresponding
    /// interrupt handler. Otherwise, the given PC is used to decode the next instruction.
    pub fn read_op(&self, index: u16, ime: bool) -> Instruction {
        match self.check_interrupt() {
            Some(op) if ime => op,
            _ => parse_instruction(self, index),
        }
    }

    fn check_interrupt(&self) -> Option<Instruction> {
        match self.ie & self.io[0x0F] {
            0 => None,
            n => {
                if check_bit_const::<0>(n) {
                    Some(Instruction::Interrupt(InterruptOp::VBlank))
                } else if check_bit_const::<1>(n) {
                    Some(Instruction::Interrupt(InterruptOp::LCD))
                } else if check_bit_const::<2>(n) {
                    Some(Instruction::Interrupt(InterruptOp::Timer))
                } else if check_bit_const::<3>(n) {
                    Some(Instruction::Interrupt(InterruptOp::Serial))
                } else if check_bit_const::<4>(n) {
                    Some(Instruction::Interrupt(InterruptOp::Joypad))
                } else {
                    // Technically unreachable
                    None
                }
            }
        }
    }

    /// This method ticks the memory. The only thing this affects is the divider and timer
    /// registers.
    pub fn tick(&mut self) {
        self.io.tick();
    }

    pub fn request_vblank_int(&mut self) {
        self.io.interrupt_flags |= self.ie & 0b1;
    }

    pub fn request_lcd_int(&mut self) {
        self.io.interrupt_flags |= self.ie & 0b10;
    }

    pub fn request_timer_int(&mut self) {
        self.io.interrupt_flags |= self.ie & 0b100;
    }

    pub fn request_serial_int(&mut self) {
        self.io.interrupt_flags |= self.ie & 0b1000;
    }

    pub fn request_button_int(&mut self, input: ButtonInput) {
        self.io.joypad.register_input(input);
        self.io.interrupt_flags |= self.ie & 0b1_0000;
    }

    pub(crate) fn clear_interrupt_req(&mut self, op: InterruptOp) {
        let mask = match op {
            InterruptOp::VBlank => 0b1,
            InterruptOp::LCD => 0b10,
            InterruptOp::Timer => 0b100,
            InterruptOp::Serial => 0b1000,
            InterruptOp::Joypad => 0b1_0000,
        };
        self.io.interrupt_flags &= !mask;
    }
}

// #[cfg(test)]
impl MemoryMap {
    pub fn rom_mut(&mut self) -> &mut [u8] {
        let MemoryBankController::Direct { rom, .. } = &mut self.mbc else {
            panic!("Do not call MemoryMap::rom unless you called MemoryMap::construct");
        };
        rom.as_mut_slice()
    }

    /// Creates a dummy memory map that should only be used for testing. Notably, this will not
    /// have a ROM header, so it is not bootable.
    pub fn construct() -> Self {
        let rom = vec![0; 32000];
        let ram = vec![0; 4000];
        let mbc = MemoryBankController::Direct { rom, ram };
        Self {
            mbc,
            vram: [0; 0x2000],
            wram: ([0; 0x1000], [0; 0x1000]),
            oam: [0; 0x100],
            io: IoRegisters::default(),
            hr: [0; 0x7F],
            ie: 0,
        }
    }
}

impl Index<u16> for MemoryMap {
    type Output = u8;

    fn index(&self, index: u16) -> &Self::Output {
        match index {
            0x0000..=0x7FFF => &self.mbc[index],
            n @ 0x8000..=0x9FFF => &self.vram[n as usize - 0x8000],
            0xA000..=0xBFFF => todo!(),
            n @ 0xC000..=0xCFFF => &self.wram.0[n as usize - 0xC000],
            n @ 0xD000..=0xDFFF => &self.wram.1[n as usize - 0xD000],
            0xE000..=0xFDFF => todo!(),
            n @ 0xFE00..=0xFE9F => &self.oam[n as usize - 0xFE00],
            0xFEA0..=0xFEFF => unreachable!("No ROM should attempt to access this region"),
            n @ 0xFF00..=0xFF7F => &self.io[n],
            n @ 0xFF80..=0xFFFE => &self.hr[(n - 0xFF80) as usize],
            0xFFFF => &self.ie,
        }
    }
}

impl IndexMut<u16> for MemoryMap {
    fn index_mut(&mut self, index: u16) -> &mut Self::Output {
        match index {
            0x0000..=0x7FFF => &mut self.mbc[index],
            n @ 0x8000..=0x9FFF => &mut self.vram[n as usize - 0x8000],
            0xA000..=0xBFFF => todo!(),
            n @ 0xC000..=0xCFFF => &mut self.wram.0[n as usize - 0xC000],
            n @ 0xD000..=0xDFFF => &mut self.wram.1[n as usize - 0xD000],
            0xE000..=0xFDFF => todo!(),
            n @ 0xFE00..=0xFE9F => &mut self.oam[n as usize - 0xFE00],
            0xFEA0..=0xFEFF => unreachable!("No ROM should attempt to access this region"),
            n @ 0xFF00..=0xFF7F => &mut self.io[n],
            n @ 0xFF80..=0xFFFE => &mut self.hr[(n - 0xFF80) as usize],
            0xFFFF => &mut self.ie,
        }
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq, Default)]
pub enum TimerControl {
    #[default]
    Disabled,
    /// Ticks once per 256 M-cycles
    Slowest(u16),
    /// Ticks once per 64 M-cycles
    Slow(u8),
    /// Ticks once per 16 M-cycles
    Fast(u8),
    /// Ticks once per 4 M-cycles
    Fastest(u8),
}

impl TimerControl {
    /// Updates the inner counter that control when the timer register is inc-ed. Note that this
    /// method is called during every *clock* cycle.
    fn update_and_tick(&mut self, state: u8) -> bool {
        fn checked_reset<const R: u8>(byte: &mut u8) -> bool {
            *byte = byte.checked_add(1).unwrap_or(R);
            *byte == R
        }
        let other = Self::from_byte(state);
        if std::mem::discriminant(self) != std::mem::discriminant(&other) {
            // TODO: There are a few goofy things that can happen when changing the timer control,
            // which are discussed in the pandocs. Those need to be implemented here.
            *self = other;
        }
        match self {
            TimerControl::Disabled => false,
            TimerControl::Slowest(count) => {
                const RESET: u16 = u16::MAX - 256 * 4;
                *count = count.checked_add(1).unwrap_or(RESET);
                *count == RESET
            }
            TimerControl::Slow(count) => checked_reset::<0>(count),
            TimerControl::Fast(count) => checked_reset::<191>(count), // u8::MAX - 16*4
            TimerControl::Fastest(count) => checked_reset::<239>(count), // u8:::MAX - 16
        }
    }

    fn from_byte(byte: u8) -> Self {
        if check_bit_const::<2>(byte) {
            match byte & 0b11 {
                0b00 => Self::Slowest(0),
                0b01 => Self::Fastest(0),
                0b10 => Self::Fast(0),
                0b11 => Self::Slow(0),
                _ => unreachable!(),
            }
        } else {
            Self::Disabled
        }
    }
}

#[derive(Hash, Clone, PartialEq, Eq)]
pub enum MemoryBankController {
    /// There is no external MBC. The game ROM is mapped into the 32 KiB that starts at 0x0000 and
    /// extends to 0x7FFF. An additional 8 KiB of RAM could be connected. This 8 KiB starts at
    /// 0xA000 and extends to 0xBFFF.
    ///
    /// See the spec [here](https://gbdev.io/pandocs/nombc.html).
    Direct {
        // TODO: It is certainly possible that operations that are 2-bytes wide might need to
        // retrieve data over the ROM/RAM boundary. In that case, these vecs should be combined,
        // though this is might be a cornercase.
        /// A vec that holds 32 KiB (4,096 bytes).
        rom: Vec<u8>,
        /// A vec that holds 8 KiB (1,024 bytes).
        ram: Vec<u8>,
    },
    /// This memory controller is the first MBC chip and might be wired in two different ways.
    /// By default, this controller supports 512 KiB of ROM and 32 KiB of RAM.
    /// Alternatively, the controller can support up to 2 MiB of ROM and only 8 KiB of RAM.
    ///
    /// See the spec [here](https://gbdev.io/pandocs/MBC1.html).
    // TODO:
    // The spec says that any cartridges with 1 MiB or more of ROM use the second wiring, but what
    // about carts that use, say, 600 KiB? Should we just assume that larger carts use the
    // alternate wiring too?
    //
    // Looks like the answer is "yes". From the docs:
    // "Available RAM sizes are 8 KiB (at $A000–BFFF) and 32 KiB (in form of four 8K banks at
    // $A000–BFFF). 32 KiB is only available in cartridges with ROM <= 512 KiB."
    MBC1(MBC1),
    ///
    /// See the spec [here](https://gbdev.io/pandocs/MBC2.html).
    MBC2,
    ///
    /// See the spec [here](https://gbdev.io/pandocs/MBC3.html).
    MBC3,
    ///
    /// See the spec [here](https://gbdev.io/pandocs/MBC5.html).
    MBC5,
}

impl MemoryBankController {
    pub fn new<'a, C: Into<Cow<'a, [u8]>>>(cart: C) -> Self {
        let cart = cart.into();
        assert_eq!(&cart[0x0104..=0x133], NINTENDO_LOGO);
        let title: String = cart[0x0134..=0x0143].iter().map(|&b| b as char).collect();
        let is_cgb = match cart[0x143] {
            0x80 => true,
            0xC0 => false,
            // FIXME: Is this actually needed?
            b => {
                eprintln!("Unknown CGB code: {b}");
                false
            }
        };
        let rom_size = match cart[0x0148] {
            0x00 => 32,
            0x01 => 64,
            0x02 => 128,
            0x03 => 256,
            0x04 => 512,
            0x05 => 1024,
            0x06 => 2 * 1024,
            0x07 => 4 * 1024,
            0x08 => 8 * 1024,
            0x52 => todo!(), // 1.1 * 1024 * 1024,
            0x53 => todo!(), // 1.2 * 1024 * 1024,
            0x54 => todo!(), // 1.5 * 1024 * 1024,
            n => panic!("Unknown ROM size: {n}"),
        } * 1024;
        let ram_size: usize = match cart[0x0149] {
            0x00 => 0,
            0x02 => 8 * 1024,
            0x03 => 32 * 1024,
            0x04 => 128 * 1024,
            0x05 => 64 * 1024,
            n => panic!("Unknown RAM size: {n}"),
        };
        let head_check = cart[0x014D];
        assert_eq!(
            head_check,
            cart[0x0134..=0x014C]
                .iter()
                .copied()
                .fold(0u8, |acc, b| acc.wrapping_sub(b).wrapping_sub(1))
        );
        // Check cartridge type
        match cart[0x0147] {
            0x00 => {
                let rom = Vec::from(&cart[0x0000..=0x7FFF]);
                assert_eq!(rom_size, rom.len());
                let ram = vec![0; ram_size];
                Self::Direct { rom, ram }
            }
            0x01 => Self::MBC1(MBC1::new(rom_size, ram_size as usize, &cart)),
            // TODO: Does the info of this bit need to be passed to the MBC1 constructor?
            0x02 => Self::MBC1(MBC1::new(rom_size, ram_size as usize, &cart)),
            // TODO: We need to communicate that the cart has RAM that is maintained by a battery.
            0x03 => Self::MBC1(MBC1::new(rom_size, ram_size as usize, &cart)),
            0x05 => todo!(),
            0x06 => todo!(),
            0x08 => todo!(),
            0x09 => todo!(),
            0x0B => todo!(),
            0x0C => todo!(),
            0x0D => todo!(),
            0x0F => todo!(),
            0x10 => todo!(),
            0x11 => todo!(),
            0x12 => todo!(),
            0x13 => todo!(),
            0x19 => todo!(),
            0x1A => todo!(),
            0x1B => todo!(),
            0x1C => todo!(),
            0x1D => todo!(),
            0x1E => todo!(),
            0x20 => todo!(),
            0x22 => todo!(),
            0xFC => todo!(),
            0xFD => todo!(),
            0xFE => todo!(),
            0xFF => todo!(),
            n => panic!("Unknown cartridge type: {n}"),
        }
    }

    fn direct_overwrite(&mut self, index: u16, val: &mut u8) {
        match self {
            MemoryBankController::Direct { rom, ram } => {
                if index as usize >= rom.len() {
                    std::mem::swap(&mut ram[(index as usize) - rom.len()], val)
                } else {
                    std::mem::swap(&mut rom[index as usize], val)
                }
            }
            MemoryBankController::MBC1(_) => todo!(),
            MemoryBankController::MBC2 => todo!(),
            MemoryBankController::MBC3 => todo!(),
            MemoryBankController::MBC5 => todo!(),
        }
    }
}

impl Index<u16> for MemoryBankController {
    type Output = u8;

    fn index(&self, index: u16) -> &Self::Output {
        let index = index as usize;
        match self {
            MemoryBankController::Direct { rom, ram } => {
                if index < rom.len() {
                    &rom[index]
                } else {
                    &ram[index + 1]
                }
            }
            MemoryBankController::MBC1(_) => todo!(),
            MemoryBankController::MBC2 => todo!(),
            MemoryBankController::MBC3 => todo!(),
            MemoryBankController::MBC5 => todo!(),
        }
    }
}

impl IndexMut<u16> for MemoryBankController {
    fn index_mut(&mut self, index: u16) -> &mut Self::Output {
        let index = index as usize;
        match self {
            // TODO: This should probably panic (or something) if index < rom.len(), i.e. they are
            // trying to write to ROM.
            MemoryBankController::Direct { rom, ram } => {
                debug_assert!(
                    index + 1 >= rom.len(),
                    "Could not index into {index:X} because ROM ends as {:?}",
                    rom.len()
                );
                &mut ram[index - rom.len()]
            }
            MemoryBankController::MBC1(_) => todo!(),
            MemoryBankController::MBC2 => todo!(),
            MemoryBankController::MBC3 => todo!(),
            MemoryBankController::MBC5 => todo!(),
        }
    }
}

impl Debug for MemoryBankController {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MemoryBankController::Direct { rom, ram } => write!(
                f,
                "Direct {{ rom_size: {}, ram_size: {} }}",
                rom.len(),
                ram.len()
            ),
            MemoryBankController::MBC1(_) => todo!(),
            MemoryBankController::MBC2 => todo!(),
            MemoryBankController::MBC3 => todo!(),
            MemoryBankController::MBC5 => todo!(),
        }
    }
}
