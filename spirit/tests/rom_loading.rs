use spirit::Gameboy;


pub const ACID_WHICH: &[u8] = include_bytes!("roms/acid/which.gb");

#[test]
fn load_acid_which() {
    let gb = Gameboy::new(ACID_WHICH);
}
