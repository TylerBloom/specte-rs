use std::fmt::Display;

use spirit::{mem::MemoryMap, ppu::Pixel, Gameboy};

static START_UP_SCREENS: &[u8] = include_bytes!("data/start_up_screens.postcard");

#[test]
fn generate_frames() {
    let mut gb = Gameboy::new(include_bytes!("roms/acid/cgb-acid2.gbc"));
    let mut datums = Vec::new();
    while !gb.is_complete() {
        gb.frame_step().complete();
        datums.push(gb.gb().mem.clone());
    }
    let data = postcard::to_allocvec(&datums).unwrap();
    std::fs::write("tests/data/start_up_memory_maps.postcard", data).unwrap();
}
#[test]
fn test_startup_frames() {
    let states: Vec<Vec<Vec<Pixel>>> = postcard::from_bytes(START_UP_SCREENS).unwrap();
    let mut gb = Gameboy::new(include_bytes!("roms/acid/cgb-acid2.gbc"));
    for (frame_num, state) in states.into_iter().enumerate() {
        assert_eq!(state.len(), 144);
        gb.frame_step().complete();
        for (line_num, (known, acutal)) in state.iter().zip(gb.gb().ppu.screen.iter()).enumerate() {
            if known != acutal {
                println!(
                    "On frame {frame_num}, line {line_num}, expected scanline: {}",
                    DisplaySlice(known)
                );
                println!("Found scanline: {}", DisplaySlice(acutal));
                panic!("Mismatched frame #{frame_num}");
            }
        }
    }
    assert!(gb.is_complete());
}

#[test]
fn test_startup_memory_maps() {
    let states: Vec<MemoryMap> = postcard::from_bytes(START_UP_SCREENS).unwrap();
    let mut gb = Gameboy::new(include_bytes!("roms/acid/cgb-acid2.gbc"));
    for (frame_num, state) in states.into_iter().enumerate() {
        gb.frame_step().complete();
        assert_eq!(state, gb.gb().mem, "Mismatched memory map on frame #{frame_num}");
    }
    assert!(gb.is_complete());
}

struct DisplaySlice<'a, T>(&'a [T]);

impl<'a, T> Display for DisplaySlice<'a, T>
where
    T: Display,
{
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
