use spirit::Gameboy;

macro_rules! postcard_bytes {
    ($file:literal) => {
        postcard::from_bytes(include_bytes!($file)).unwrap()
    };
}

#[test]
#[should_panic]
fn acid_which() {
    let rom = include_bytes!("roms/acid/which.gb");
    let mut gb = Gameboy::new(rom).complete();
    todo!()
}

#[test]
#[should_panic]
fn acid_dmg_acid2() {
    let rom = include_bytes!("roms/acid/dmg-acid2.gb");
    let mut gb = Gameboy::new(rom).complete();
    todo!()
}

#[test]
fn acid_cgb_acid2() {
    let rom = include_bytes!("roms/acid/cgb-acid2.gbc");
    let mut gb = Gameboy::new(rom).complete();
    (0..10).for_each(|_| gb.next_frame().complete());
    let expected_mem = postcard_bytes!("data/acid-cgb-acid2-memory-map.postcard");
    assert_eq!(gb.mem, expected_mem);
    let expected_screen: Vec<Vec<_>> = postcard_bytes!("data/acid-cgb-acid2-screen.postcard");
    assert_eq!(gb.ppu.screen, expected_screen);
}

#[test]
#[should_panic]
fn acid_cgb_acid_hell() {
    let rom = include_bytes!("roms/acid/cgb-acid-hell.gbc");
    let mut gb = Gameboy::new(rom).complete();
    todo!()
}
