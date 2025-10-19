/* FIXME: Save data is incorrect
use std::fmt::Display;

use spirit::Gameboy;
use spirit::mem::MemoryMap;
use spirit::ppu::Pixel;

static START_UP_SCREENS: &[u8] = include_bytes!("data/start_up_screens.postcard");
static START_UP_MEMORY_MAPS: &[u8] = include_bytes!("data/start_up_memory_maps.postcard");

// #[test]
#[allow(dead_code)]
fn generate_frames() {
    let mut gb = Gameboy::new(include_bytes!("roms/acid/cgb-acid2.gbc").into());
    let mut datums = Vec::new();
    while !gb.is_complete() {
        gb.next_frame();
        datums.push(gb.mem.clone());
    }
    let data = postcard::to_allocvec(&datums).unwrap();
    std::fs::write("tests/data/start_up_memory_maps.postcard", data).unwrap();
}

#[test]
fn test_startup_frames() {
    let states: Vec<Vec<Vec<Pixel>>> = postcard::from_bytes(START_UP_SCREENS).unwrap();
    println!(
        "Find non-empty screen at {}",
        states
            .iter()
            .position(|screen| screen.iter().flatten().any(|p| *p != Pixel::BLACK))
            .unwrap()
    );
    let mut gb = Gameboy::new(include_bytes!("roms/acid/cgb-acid2.gbc").into());
    (0..9).for_each(|_| gb.next_frame());
    for (frame_num, state) in states.into_iter().enumerate() {
        assert_eq!(state.len(), 144);
        gb.next_frame();
        state
            .iter()
            .zip(gb.ppu.screen.iter())
            .enumerate()
            .filter(|(_, (known, actual))| known != actual)
            .for_each(|(line_num, (known, actual))| {
                println!(
                    "On frame {frame_num}, line {line_num}, expected scanline: {}",
                    DisplaySlice(known)
                );
                println!("Found scanline: {}", DisplaySlice(actual));
                panic!("Mismatched frame #{frame_num}");
            });
    }
    assert!(gb.is_complete());
}

#[test]
fn test_startup_memory_maps() {
    let states: Vec<MemoryMap> = postcard::from_bytes(START_UP_MEMORY_MAPS).unwrap();
    let mut gb = Gameboy::new(include_bytes!("roms/acid/cgb-acid2.gbc").into());
    for (frame_num, state) in states.into_iter().enumerate() {
        gb.next_frame();
        assert_eq!(
            state,
            gb.mem,
            "Mismatched memory map on frame #{frame_num}"
        );
    }
    assert!(gb.is_complete());
}

struct DisplaySlice<'a, T>(&'a [T]);

impl<T: Display> Display for DisplaySlice<'_, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut iter = self.0.iter();
        write!(f, "[")?;
        if let Some(val) = iter.next() {
            write!(f, "{val}")?;
        }
        iter.try_for_each(|val| write!(f, ", {val}"))?;
        write!(f, "]")
    }
}
*/
