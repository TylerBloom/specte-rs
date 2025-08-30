//! This module contains the data structures for the entire emulator, including the trove location,
//! emulator config, keybinds, and more.

use std::{
    path::{Path, PathBuf},
    sync::LazyLock,
};

use serde::{Deserialize, Serialize};

use crate::trove::Trove;

pub(crate) static CONFIG_PATH: LazyLock<PathBuf> = LazyLock::new(|| {
    let mut base_dir: PathBuf = std::env::var("CARGO_MANIFEST_DIR")
        .unwrap_or_else(|_| todo!())
        .parse()
        .unwrap();
    base_dir.push("ghast.toml");
    base_dir
});

#[derive(Debug, Serialize, Deserialize)]
pub struct Config {
    /// The path to the game trove.
    #[serde(default)]
    pub(crate) trove_path: Option<PathBuf>,
    /// Whether or not to launch the last game on start.
    // TODO: This should be more than a bool. Variants such as "launch last snapshot" or "launch to
    // snapshot selection screen" should be options.
    #[serde(default)]
    launch_on_start: bool,
}

impl Config {
    pub fn read() -> Self {
        println!("Looking for config at {CONFIG_PATH:?}");
        toml::from_str(&std::fs::read_to_string(&*CONFIG_PATH).unwrap()).unwrap()
    }

    pub fn save(&self) {
        std::fs::write(&*CONFIG_PATH, toml::to_string_pretty(self).unwrap()).unwrap()
    }

    pub fn get_trove(&self) -> Trove {
        Trove::parse_or_default(self.trove_path.clone())
    }
}
