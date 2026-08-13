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

use rusqlite::Connection;
use rusqlite::OptionalExtension;
use serde::Deserialize;
use serde::Serialize;
use xilem::WidgetView;
use xilem::view::flex_col;
use xilem::view::label;
use xilem::view::text_button;
use xilem::view::worker;
use xilem_core::fork;

use crate::config::CONFIG_PATH;
use crate::state::AddGameMessage;
use crate::state::HomeMessage;
use crate::state::UiMessage;
use crate::state::UiState;
use crate::utils::identity_proxy;

// TODO: To get an MVP working, the trove will just contain a copy of each can. Later, layers like
// the game sets will be added.
#[allow(dead_code)]
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
        self.conn
            .execute("INSERT INTO games (name, rom) VALUES (?1, ?2)", (name, rom))
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
    pub fn display(&self) -> impl WidgetView<UiState> + use<> {
        flex_col((
            label("Trove"),
            self.add_game_set_button(),
            self.display_games(),
        ))
    }

    pub fn add_game_set_button(&self) -> impl WidgetView<UiState> + use<> {
        let button = text_button("Add Game Set", |state: &mut UiState| {
            state.add_game_send.send(AddGameMessage::AddGame).unwrap();
        });
        let worker = worker(
            identity_proxy,
            |state: &mut UiState, send| {
                state
                    .add_game_send
                    .send(AddGameMessage::NewSender(send))
                    .unwrap()
            },
            |state: &mut UiState, (file_name, rom): (String, Vec<u8>)| {
                state.home.trove.add_game(file_name, rom);
            },
        );
        fork(button, worker)
    }

    pub fn display_games(&self) -> impl WidgetView<UiState> + use<> {
        let mut games = self
            .conn
            .prepare("SELECT name FROM games")
            .unwrap()
            .query([])
            .unwrap()
            .mapped(|row| row.get(0))
            .collect::<Result<Vec<String>, _>>()
            .unwrap();

        games.sort();

        let col = games
            .into_iter()
            .map(|file_name| {
                let file_name: &'static str = file_name.leak();
                text_button(file_name, move |state: &mut UiState| {
                    state.update(UiMessage::HomeMessage(HomeMessage::StartGame(
                        file_name.to_owned(),
                    )));
                })
            })
            .collect::<Vec<_>>();

        flex_col(col)
    }
}

/// Contains data about usage, such as the last game played.
#[derive(Debug, Serialize, Deserialize)]
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
