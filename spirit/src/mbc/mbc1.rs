use std::ops::Range;

#[derive(Debug, Hash, Clone, PartialEq, Eq)]
pub struct MBC1 {
    kind: MBC1Kind,
    rom: Vec<u8>,
    rom_bank: u16,
    // TODO: This is currently unused, but it will be needed for bounds checking
    rom_bank_count: usize,
    ram: Vec<u8>,
    ram_bank: u16,
    // TODO: This is currently unused, but it will be needed for bounds checking
    ram_bank_count: usize,
    /// Determines if RAM can be read from and written to. The actual hardware uses an 8-bit
    /// register, so RAM is enabled when the lower 4 bits are set.
    ///
    /// Initially set to `false`, any writes to the memory addresses 0x0000 through 0x1FFF write to
    /// this register.
    ram_enabled: bool,
    /// Determines the banking mode. The actual hardware uses an 8-bit
    /// register, so RAM is enabled when the lower 4 bits are set.
    ///
    /// Initially set to `false`, any writes to the memory addresses 0x0000 through 0x1FFF write to
    /// this register.
    banking_mode: BankingMode,
}

impl MBC1 {
    pub fn new(rom_size: usize, ram_size: usize, cart: &[u8]) -> Self {
        // All MBC1 cartridges with 1 MiB of ROM or more use this alternate wiring
        let kind = if rom_size > 1024 * 1024 {
            MBC1Kind::Rewired
        } else {
            MBC1Kind::Standard
        };
        let rom_bank_count = rom_size / (16 * 1024);
        let rom = cart[..rom_size].to_owned();
        let ram_bank_count = ram_size / (16 * 1024);
        let ram = vec![0; ram_size];
        Self {
            kind,
            rom,
            rom_bank: 0,
            rom_bank_count,
            ram,
            ram_bank: 0,
            ram_bank_count,
            ram_enabled: false,
            banking_mode: BankingMode::Simple,
        }
    }

    pub fn read(&self, index: u16) -> u8 {
        match index {
            0x0000..=0x3FFF => self.rom[index as usize],
            // TODO: Handle wrap arounds should RAM be too small?
            i @ 0x4000..=0x7FFF => self.rom[self.rom_range()][(i - 0x4000) as usize],
            i @ 0xA000..=0xBFFF => self.ram[self.ram_range()][(i - 0xA000) as usize],
            i => unreachable!("Memory controller is unable to read from memory address: {i:#X}"),
        }
    }

    /// Writes to a register or RAM bank
    pub fn write(&mut self, index: u16, value: u8) {
        match index {
            0x0000..=0x1FFF => self.ram_enabled = (value & 0x0A) != 0,
            0x2000..=0x3FFF => self.rom_bank |= (value & 0b00011111) as u16,
            0x4000..=0x5FFF => match self.kind {
                MBC1Kind::Standard => self.ram_bank = value as u16,
                MBC1Kind::Rewired => self.rom_bank |= (value & 0b01100000) as u16,
            },
            0x6000..=0x7FFF if (value & 1) == 0 => self.banking_mode = BankingMode::Advanced,
            0x6000..=0x7FFF => self.banking_mode = BankingMode::Simple,
            i @ 0xA000..=0xBFFF => {
                let r = self.ram_range();
                // TODO: Handle wrap arounds should RAM be too small?
                self.ram[r][(i - 0xA000) as usize] = value;
            }
            _ => unreachable!("Memory controller is unable to write to memory address: {index:#X}"),
        }
    }

    /// Returns a range that will capture the selected ROM bank or the initial ROM bank if banking
    /// mode is simple.
    fn rom_range(&self) -> Range<usize> {
        let mut bank = self.rom_bank;
        if self.rom_bank == 0 {
            bank += 1
        }
        let start = ((self.banking_mode as u16) * 0x4000 * bank) as usize;
        let end = start + 0x4000;
        start..end
    }

    /// Returns a range that will capture the selected RAM bank (or the initial RAM bank if banking
    /// mode is simple).
    fn ram_range(&self) -> Range<usize> {
        let start = ((self.banking_mode as u16) * 0x2000 * self.ram_bank) as usize;
        let end = start + 0x2000;
        start..end
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MBC1Kind {
    Standard,
    Rewired,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BankingMode {
    Simple = 0,
    Advanced = 1,
}

// TODO:
// Write testing for this sections of the Pandocs:
// https://gbdev.io/pandocs/MBC1.html#20003fff--rom-bank-number-write-only
//
// Also, add tests for reads and writes from smaller carts.
