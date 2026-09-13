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
use std::sync::Arc;

use rusqlite::Connection;
use rusqlite::OptionalExtension;
use serde::Deserialize;
use serde::Serialize;

// TODO: To get an MVP working, the trove will just contain a copy of each can. Later, layers like
// the game sets will be added.
pub struct Trove {
    pub(crate) conn: Connection,
}

impl Trove {
    pub fn new(conn: Connection) -> Self {
        conn.execute(
            "CREATE TABLE IF NOT EXISTS games (
            name   STRING PRIMARY KEY,
            rom BLOB
        )",
            (),
        )
        .unwrap();
        Self { conn }
    }

    pub fn add_game(&mut self, name: String, rom: Vec<u8>) {
        let _ = self
            .conn
            .execute("INSERT INTO games (name, rom) VALUES (?1, ?2)", (name, rom));
        #[cfg(target_family = "wasm")]
        self.save_db();
    }

    #[cfg(target_family = "wasm")]
    pub fn save_db(&self) {
        use base64::Engine;

        let blob = self.conn.serialize("main").unwrap();
        let encode = base64::engine::general_purpose::STANDARD.encode(&*blob);
        let storage = web_sys::window().unwrap().local_storage().unwrap().unwrap();
        storage
            .set_item(crate::config::wasm::TROVE_KEY, encode.as_str())
            .unwrap();
    }

    /// Given the name of a game in the trove, reads the file and returns the contents
    pub fn fetch_game(&self, name: &str) -> Vec<u8> {
        println!("Looking for game: {name:?}");
        self.conn
            .query_row("SELECT rom FROM games WHERE name = ?1", (name,), |row| {
                row.get(0)
            })
            .optional()
            .unwrap()
            .unwrap()
    }

    /// Reads back the sorted list of game names currently stored in the trove.
    pub(crate) fn game_names(&self) -> Vec<Arc<str>> {
        let mut games = self
            .conn
            .prepare("SELECT name FROM games")
            .unwrap()
            .query([])
            .unwrap()
            .mapped(|row| row.get(0))
            .collect::<Result<Vec<Arc<str>>, _>>()
            .unwrap();
        games.sort();
        games
    }
}

/// Contains data about usage, such as the last game played.
#[derive(Debug, Serialize, Deserialize)]
#[allow(dead_code)]
pub(crate) struct TroveData {
    #[serde(default)]
    last_game: Option<String>,
}

/// Represents a base game and all of its run instances
#[allow(dead_code)]
pub(crate) struct GameSet {
    path: PathBuf,
}

/// Represents a run of a given game, all of its screenshots, and a bit of metadata
#[allow(dead_code)]
pub(crate) struct GameInstance {
    /// The name of the ROM file.
    name: String,
    data: GameInstanceData,
    path: PathBuf,
}

#[allow(dead_code)]
impl GameInstance {
    pub fn screenshots(&self) -> Vec<PathBuf> {
        todo!()
    }
}

/// A bit of a metadata for a given game instance, such as the last screenshot file.
#[allow(dead_code)]
pub(crate) struct GameInstanceData {
    last_screenshot: Option<String>,
}
