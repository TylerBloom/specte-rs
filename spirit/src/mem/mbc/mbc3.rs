// TODO: How should clocks be handled...
pub struct MBC3 {
    rom: Vec<u8>,
    rom_bank: usize,
    ram: Vec<u8>,
    ram_bank: usize,
}

impl MBC3 {
    pub fn read(&self, index: u16) -> u8 {
        todo!()
    }

    pub fn write(&mut self, index: u16, value: u8) {
        match index {
            0x0000..=0x1FFF => todo!(),
            0x2000..=0x3FFF => todo!(),
            0x4000..=0x5FFF => todo!(),
            0x6000..=0x7FFF => todo!(),
            0xA000..=0xBFFF => todo!(),
            i => unreachable!("MBC3 could not write to address: {i:#X}"),
        }
        todo!()
    }
}
