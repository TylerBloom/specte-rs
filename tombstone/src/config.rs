use std::{collections::{HashMap, HashSet}, path::PathBuf, sync::LazyLock};

use serde::{Serialize, Deserialize};

use crate::command::Command;

pub static CONFIG: LazyLock<Config> = LazyLock::new(|| {
    let mut path = env!("CARGO_MANIFEST_DIR").parse::<PathBuf>().unwrap();
    path.push("config.toml");
    let data = std::fs::read_to_string(path).unwrap_or_default();
    toml::from_str(&data).unwrap_or_default()
});

#[derive(Debug, Default, Serialize, Deserialize)]
pub struct Config {
    per_roms: HashMap<String, PerRom>
}

#[derive(Debug, Serialize, Deserialize)]
pub struct PerRom {
    breakpoints: HashSet<u16>,
    macros: Vec<Vec<Command>>
}
