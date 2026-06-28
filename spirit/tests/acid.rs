use spirit::Gameboy;

#[test]
#[should_panic]
fn acid_dmg_acid2() {
    let rom = include_bytes!("roms/acid/dmg-acid2.gb");
    let _gb = Gameboy::load_cartridge(rom.into()).complete();
    todo!()
}

#[test]
fn acid_cgb_acid2() {
    let rom = include_bytes!("roms/acid/cgb-acid2.gbc");
    let mut gb = Gameboy::load_cartridge(rom.into()).complete();
    (0..10).for_each(|_| gb.next_frame());
    let screen: Vec<u8> = gb
        .ppu
        .screen
        .iter()
        .flatten()
        .flat_map(|p| [p.r, p.g, p.b])
        .collect();
    insta::assert_binary_snapshot!("acid-cgb-acid2-screen.bin", screen);
}

#[test]
#[should_panic]
fn acid_cgb_acid_hell() {
    let rom = include_bytes!("roms/acid/cgb-acid-hell.gbc");
    let _gb = Gameboy::load_cartridge(rom.into()).complete();
    todo!()
}
