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


// TODO: To get an MVP working, the trove will just contain a copy of each can. Later, layers like
// the game sets will be added.
pub struct Trove {
    /// The trove carries around a path as the trove is largely an interface into the expected file
    /// structure of the trove.
    pub(crate) path: PathBuf,
    /// The trove data at the base of the trove.
    pub(crate) trove_data: TroveData,
}

impl Trove {
    /// Optionally takes a path to the trove directory as an argument. If one isn't supplied, we
    /// assume that it exists next to the config file.
    pub fn parse_or_default(path: Option<PathBuf>) -> Self {
        let mut trove_toml = path.unwrap_or_else(|| {
            let mut path = (*CONFIG_PATH).clone();
            path.pop();
            path.push("trove");
            path
        });
        let path = trove_toml.clone();
        trove_toml.push(".trove.toml");
        println!("Looking for trove at {trove_toml:?}");
        if !trove_toml.exists() {
            std::fs::write(&trove_toml, b"").unwrap();
        }
        let trove_data = toml::from_str(&std::fs::read_to_string(trove_toml).unwrap()).unwrap();
        Self { path, trove_data }
    }

    pub fn add_game(&mut self, path: PathBuf) {
        // Copy and trim the file name
        let mut dir = self.path.clone();
        dir.push(path.file_name().unwrap());
        std::fs::copy(path, dir).unwrap();
    }

    /*
    pub fn add_game(&mut self, path: PathBuf) -> GameSet {
        // Copy and trim the file name
        let mut dir = self.path.clone();
        dir.push(path.file_stem().unwrap());
        // Create a directory with the file name
        if dir.exists() && dir.is_dir() {
            return GameSet {
                path: dir,
            };
        }
        println!("Creating GameSet at: {dir:?}");
        std::fs::create_dir(&dir).unwrap();

        // Copy the file into the new directory
        let set_dir = dir.clone();
        dir.push(path.file_name().unwrap());
        std::fs::copy(path, dir).unwrap();
        GameSet { path: set_dir }
    }
    */
}

/// Contains data about usage, such as the last game played.
#[derive(Debug, Serialize, Deserialize)]
pub(crate) struct TroveData {
    #[serde(default)]
    last_game: Option<String>,
}

/// Represents a base game and all of its run instances
pub(crate) struct GameSet {
    path: PathBuf,
}

/// Represents a run of a given game, all of its screenshots, and a bit of metadata
pub(crate) struct GameInstance {
    /// The name of the ROM file.
    name: String,
    data: GameInstanceData,
    path: PathBuf,
}

impl GameInstance {
    pub fn screenshots(&self) -> Vec<PathBuf> {
        todo!()
    }
}

/// A bit of a metadata for a given game instance, such as the last screenshot file.
pub(crate) struct GameInstanceData {
    last_screenshot: Option<String>,
}
