//! A "trove" contains all of a user's game data, including: collection of games, save data, and
//! all the per-game configs.
//! Each trove is broken into multiple layers.
//! The first later is some overall data (such as the last game played) as well as directories for
//! each unique game.
//! A game's directory has the base copy of that game's ROM and directories for each run of that
//! game. This is to help sidestep many games single save file per cartridge.
//! Each ROM instance contains a copy of the parent ROM, any snapshots taken, and a file ordering
//! the times that the snapshots were taken.
// TODO: Eventually, the game's directory will have a layer of configs for the emulator as well as
// cheap codes.
// TODO: When ROM patching is supported, ROM "recipes" will be added so users can create new game
// directories as new versions of the patch get released.

use std::path::PathBuf;

use serde::{Deserialize, Serialize};

use crate::config::CONFIG_PATH;

pub struct Trove {
    /// The trove carries around a path as the trove is largely an interface into the expected file
    /// structure of the trove.
    path: PathBuf,
    /// The trove data at the base of the trove.
    trove_data: TroveData,
}

impl Trove {
    /// Optionally takes a path to the trove directory as an argument. If one isn't supplied, we
    /// assume that it exists next to the config file.
    pub fn parse_or_default(path: Option<PathBuf>) -> Self {
        let mut trove_toml = path.unwrap_or_else(|| {
            let mut path = (*CONFIG_PATH).clone();
            path.push("trove");
            path
        });
        let path = trove_toml.clone();
        trove_toml.push(".trove.toml");
        let trove_data = toml::from_str(&std::fs::read_to_string(trove_toml).unwrap()).unwrap();
        Self { path, trove_data }
    }
}

/// Contains data about usage, such as the last game played.
#[derive(Debug, Serialize, Deserialize)]
struct TroveData {
    last_game: Option<String>,
}
