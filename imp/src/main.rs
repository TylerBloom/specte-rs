use std::path::PathBuf;
use std::str::FromStr;

use clap::Parser;
use spirit::Gameboy;

#[derive(Debug, Parser)]
#[command(version, about)]
struct Config {
    /// The path to the ROM to run.
    rom: String,
    /// The directory where the output files will be placed.
    output_dir: Option<String>,
    /// The number of screens past to step before saving the data.
    #[arg(default_value_t = 10)]
    frames: usize,
}

fn main() {
    let args = Config::parse();
    let mut path = PathBuf::from_str(&args.rom).unwrap();
    let rom = std::fs::read(&path).unwrap();
    let mut gb = Gameboy::new(rom).complete();
    (0..args.frames).for_each(|_| gb.next_frame());

    let filename_base = path
        .file_name()
        .unwrap()
        .to_str()
        .unwrap()
        .strip_suffix(path.extension().unwrap().to_str().unwrap())
        .unwrap()
        .strip_suffix('.')
        .unwrap()
        .to_owned();

    let output_dir = match &args.output_dir {
        Some(output_dir) => PathBuf::from_str(output_dir).unwrap(),
        None => {
            while !path.ends_with("tests") {
                path.pop();
            }
            path.push("data/");
            path
        }
    };

    std::fs::write(
        format!(
            "{}/{filename_base}-memory-map.postcard",
            output_dir.to_str().unwrap()
        ),
        postcard::to_allocvec(&gb.mem).unwrap(),
    )
    .unwrap();
    std::fs::write(
        format!(
            "{}/{filename_base}-screen.postcard",
            output_dir.to_str().unwrap()
        ),
        postcard::to_allocvec(&gb.ppu.screen).unwrap(),
    )
    .unwrap();
}
