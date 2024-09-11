use std::fmt::Display;

use spirit::{ppu::Pixel, Gameboy};

static START_UP_SCREENS: &[u8] = include_bytes!("data/start_up_screens.postcard");

/*
#[test]
fn generate_frames() {
    let mut gb = Gameboy::new(include_bytes!("roms/acid/cgb-acid2.gbc")).start_up();
    let mut frames = Vec::new();
    while !gb.is_complete() {
        gb.frame_step().complete();
        frames.push(gb.gb().ppu.screen.clone());
    }
    let data = postcard::to_allocvec(&frames).unwrap();
    std::fs::write("tests/data/start_up_screens.postcard", data).unwrap();
}
*/

#[test]
fn test_startup_frames() {
    let states: Vec<Vec<Vec<Pixel>>> = postcard::from_bytes(START_UP_SCREENS).unwrap();
    let mut gb = Gameboy::new(include_bytes!("roms/acid/cgb-acid2.gbc")).start_up();
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

struct DisplaySlice<'a, T>(&'a [T]);

impl<'a, T> Display for DisplaySlice<'a, T>
where
    T: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut iter = self.0.iter();
        write!(f, "[")?;
        if let Some(val) = iter.next() {
            write!(f, "{val}");
        }
        iter.try_for_each(|val| write!(f, ", {val}"))?;
        write!(f, "]")
    }
}
