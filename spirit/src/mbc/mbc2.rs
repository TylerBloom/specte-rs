pub struct MBC2 {
    rom: Vec<u8>,
    // TODO: This can probably be replaced with NonZeroUsize
    rom_bank: usize,
    ram: Box<[u8; 512]>,
    ram_enabled: bool,
}

impl MBC2 {
    pub fn read(&self, index: u16) -> u8 {
        match index {
            i @ 0x0000..=0x3FFF => self.rom[i as usize],
            i @ 0x4000..=0x7FFF => {
                let start = std::cmp::max(self.rom_bank, 1) * 0x4000;
                self.rom[start..(start + 0x4000)][(i - 0x4000) as usize]
            }
            i @ 0xA000..=0xBFFF if self.ram_enabled => self.ram[(i & 0x01FF) as usize],
            i @ 0xA000..=0xBFFF => 0,
            i => unreachable!("MBC2 could not read from address: {i:#X}"),
        }
    }

    pub fn write(&mut self, index: u16, value: u8) {
        match index {
            i @ 0x0000..=0x3FFF => {
                if i & 0x0100 == 0 {
                    self.ram_enabled = value == 0x0A;
                } else {
                    self.rom_bank = (value & 0x0F) as usize;
                }
            }
            mut i @ 0xA000..=0xBFFF => {
                if self.ram_enabled {
                    i &= 0x01FF;
                    self.ram[i as usize] = 0x0F & value;
                }
            }
            i => unreachable!("MBC2 could not write to address: {i:#X}"),
        }
    }
}
