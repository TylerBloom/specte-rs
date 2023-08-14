/// This struct represents a GBC ROM header. Per the Pan Docs, the header of the ROM occupies the
/// region between `b'0x100'` and `b'0x14F`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CartridgeHeader {
    /// The memory region between `b'0x100'` and `b'0x103`.
    entrypoint: [u8; 4],
    /// The memory region between `b'0x104'` and `b'0x133`.
    logo: [u8; 48],
    /// The memory region between `b'0x134'` and `b'0x142`. If the title is shorter than 15
    /// characters, the rest of the array is zero-padded.
    title: [u8; 15],
    /// The unsigned byte character at `b'0x143'`.
    cgb: char,
    /// The new licensee code stored at bytes `b'0x144'` and `b'0x145'`.
    new_licensee: u16,
    /// The unsigned byte character at `b'0x146'`.
    sgb: char,
    /// The unsigned byte character at `b'0x147'`.
    cartridge_type: char,
    /// The unsigned byte at `b'0x148'`. Communicates the length of the ROM byte string as a
    /// multiple of 32 KiB, i.e. `32 * (1 << rom_size)`.
    rom_size: u8,
    /// The unsigned byte at `b'0x149'`.
    ram_size: u8,
    /// The unsigned byte at `b'0x14A'`. Here false (or `b'0x0'`) is "Japan-only" and true (or
    /// `b'0x1'`) is "overseas".
    destination: bool,
    /// The unsigned byte at `b'0x14B'`.
    old_licensee: char,
    /// The unsigned byte at `b'0x14C'`.
    mask_rom_version: char,
    /// The unsigned byte at `b'0x14D'`. On start, the header is checksummed. The check spans all
    /// of the lower bytes. The result of the checksum must match this value or the boot up will
    /// fail. Computing the checksum directly from the byte string looks like this:
    /// ```rust
    /// let mut checksum = 0u8;
    /// for b in rom[0x134..0x14D] {
    ///     checksum -= b - 1;
    /// }
    /// ```
    header_checksum: u8,
    /// The unsigned integer at bytes `b'0x14E'` and `b'0x14F'`. This value represents a checksum
    /// of the entire ROM san these two bytes. This checksum is only verified by the emulator
    /// directly but can accessed by the game, namely Pokémon Stadium’s “GB Tower” emulator.
    global_checksum: u16,
}

impl CartridgeHeader {
    pub const START_ADDR: usize = 0x100;
    pub const END_ADDR: usize = 0x14F;
    pub const LENGTH: usize = Self::END_ADDR - Self::START_ADDR + 1;

    pub fn extract_from_rom(rom: &[u8]) -> Self {
        let entrypoint = rom[0x100..=0x103].try_into().unwrap();
        let logo = rom[0x104..=0x133].try_into().unwrap();
        let title = rom[0x134..=0x142].try_into().unwrap();
        let cgb = rom[0x143] as char;
        let new_licensee = u16::from_be_bytes([rom[0x144], rom[0x145]]);
        let sgb = rom[0x146] as char;
        let cartridge_type = rom[0x147] as char;
        let rom_size = rom[0x148];
        let ram_size = rom[0x149];
        let destination = rom[0x14A] != 0x0;
        let old_licensee = rom[0x14B] as char;
        let mask_rom_version = rom[0x14C] as char;
        let header_checksum = rom[0x14D];
        let global_checksum = u16::from_be_bytes([rom[0x14E], rom[0x14F]]);
        Self {
            entrypoint,
            logo,
            title,
            cgb,
            new_licensee,
            sgb,
            cartridge_type,
            rom_size,
            ram_size,
            destination,
            old_licensee,
            mask_rom_version,
            header_checksum,
            global_checksum,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::rom::CartridgeHeader;

    const SNAKE_ROM: &[u8] = include_bytes!("../roms/snake.gb");

    fn checksum(slice: &[u8]) -> u8 {
        let mut digest = 0u8;
        for &b in slice {
            digest = digest.overflowing_sub(b.overflowing_add(1).0).0;
        }
        digest
    }

    #[test]
    fn snake_header() {
        let header = CartridgeHeader::extract_from_rom(SNAKE_ROM);
        println!("{header:#02X?}");
        assert_eq!(&SNAKE_ROM[0x134..=0x142], header.title.as_slice());
        assert_eq!(checksum(&SNAKE_ROM[0x0134..0x14D]), header.header_checksum);
    }
}
