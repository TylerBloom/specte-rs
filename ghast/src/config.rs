//! This module contains the data structures for the entire emulator, including the trove location,
//! emulator config, keybinds, and more.

use std::path::PathBuf;

use rusqlite::Connection;
use serde::Deserialize;
use serde::Serialize;

use crate::trove::Trove;

#[derive(Debug, Clone, Serialize, Deserialize)]
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

#[cfg(not(target_family = "wasm"))]
mod native {
    use std::path::PathBuf;
    use std::sync::LazyLock;

    use super::*;

    pub(crate) static CONFIG_PATH: LazyLock<PathBuf> = LazyLock::new(|| {
        let mut base_dir: PathBuf = std::env::var("CARGO_MANIFEST_DIR")
            .unwrap_or_else(|_| todo!())
            .parse()
            .unwrap();
        base_dir.push("ghast.toml");
        base_dir
    });

    impl Config {
        pub fn read() -> Self {
            println!("Looking for config at {CONFIG_PATH:?}");
            toml::from_str(&std::fs::read_to_string(&*CONFIG_PATH).unwrap()).unwrap()
        }

        pub fn save(&self) {
            std::fs::write(&*CONFIG_PATH, toml::to_string_pretty(self).unwrap()).unwrap()
        }

        pub fn get_trove(&self) -> Trove {
            let conn = match self.trove_path.as_ref() {
                Some(path) => Connection::open(path).unwrap(),
                None => {
                    let mut path = CONFIG_PATH.clone();
                    path.pop();
                    path.push("trove.db3");
                    Connection::open(path).unwrap()
                }
            };
            Trove::new(conn)
        }
    }
}

#[cfg(target_family = "wasm")]
pub mod wasm {
    use base64::Engine;
    use base64::engine::general_purpose::STANDARD as BASE64;

    use super::*;

    const CONFIG_KEY: &str = "ghast-config";
    pub const TROVE_KEY: &str = "ghast-trove";

    fn local_storage() -> web_sys::Storage {
        web_sys::window().unwrap().local_storage().unwrap().unwrap()
    }

    impl super::Config {
        pub fn read() -> Self {
            // Mirrors the native target: an absent config is treated the same as the empty
            // `ghast.toml` checked into the repo, relying on every field's `#[serde(default)]`.
            let value = local_storage()
                .get_item(CONFIG_KEY)
                .unwrap()
                .unwrap_or_default();
            toml::from_str(&value).unwrap()
        }

        pub fn save(&self) {
            local_storage()
                .set_item(CONFIG_KEY, &toml::to_string_pretty(self).unwrap())
                .unwrap()
        }

        pub fn get_trove(&self) -> Trove {
            // The trove is stored as a base64-encoded, serialized SQLite database, since
            // `localStorage` only holds strings.
            let mut conn = Connection::open_in_memory().unwrap();
            // Mirrors the native target: `Connection::open` creates the database file when it's
            // missing, so a first run with no stored trove yet just gets a fresh, empty database.
            if let Some(encoded) = local_storage().get_item(TROVE_KEY).unwrap() {
                let bytes = BASE64.decode(encoded).unwrap();
                conn.deserialize_read_exact("main", bytes.as_slice(), bytes.len(), false)
                    .unwrap();
            }
            Trove::new(conn)
        }
    }
}
