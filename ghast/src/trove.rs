//! A "trove" contains all of a user's game data, including: collection of games, save data, and
//! all the per-game configs.
//! Each trove is broken into multiple layers.
//! The first later is some overall data (such as the last game played) as well as directories for
//! each unique game.
//! A game's directory has the base copy of that game's ROM and directories for each run of that
//! game. This is to help sidestep many games single save file per cartridge.
//! Each ROM instance contains a copy of the parent ROM, any snapshots taken, and a file ordering
//! the times that the snapshots were taken.
// TODO: When ROM patching is supported, ROM "recipes" will be added so users can create new game
// directories as new versions of the patch get released.

use std::sync::Arc;

use chrono::Utc;
use rusqlite::Connection;
use uuid::Uuid;

// Structure of DB:
// Mirrors UI. Top level table is list of games by name. Each game one or more "variants". A
// variant represents the current saved cartridge data and all snapshots. Snapshots are just the
// serialized emulator state.
//
//  Games Table:
//  Game name (ID), ROM Data, last updated
//
//  Variants Table:
//  Variant ID, variant name, Game name, cartiridge data, last updated
//
//  Snapshot table:
//  Snapshot ID, name, Variant ID, serialized emulator state, last updated
pub struct Trove {
    pub(crate) conn: Connection,
}

impl Trove {
    pub fn new(conn: Connection) -> Self {
        conn.execute(
            "
        CREATE TABLE IF NOT EXISTS games (
            name          STRING PRIMARY KEY,
            last_updated  DATETIME
        )",
            (),
        )
        .unwrap();
        conn.execute(
            "
        CREATE TABLE IF NOT EXISTS variants (
            id            BLOB PRIMARY KEY,
            name          STRING,
            game_name     STRING,
            cartridge     BLOB,
            last_updated  DATETIME
        )",
            (),
        )
        .unwrap();
        conn.execute(
            "
        CREATE TABLE IF NOT EXISTS snapshots (
            id            BLOB PRIMARY KEY,
            name          STRING,
            variant_id    BLOB,
            state         BLOB,
            last_updated  DATETIME
        )",
            (),
        )
        .unwrap();
        Self { conn }
    }

    pub fn add_game(&self, name: &str, rom: &[u8]) {
        self.conn
            .execute(
                "INSERT INTO games (name, last_updated) VALUES (?1, ?2)",
                (name, Utc::now()),
            )
            .unwrap();

        let variant = GameVariant {
            id: Uuid::new_v4(),
            name: name.into(),
            cart: rom.to_owned(),
        };
        self.add_variant(variant, name);
    }

    /// Reads back the sorted list of game names currently stored in the trove.
    pub(crate) fn list_games(&self) -> Vec<Arc<str>> {
        self.conn
            .prepare("SELECT name FROM games ORDER BY last_updated")
            .unwrap()
            .query([])
            .unwrap()
            .mapped(|row| row.get(0))
            .collect::<Result<_, _>>()
            .unwrap()
    }

    pub fn add_variant(&self, GameVariant { id, name, cart }: GameVariant, game_name: &str) {
        self.conn
            .execute(
                "INSERT INTO variants (id, name, game_name, cartridge, last_updated) VALUES (?1, ?2, ?3, ?4, ?5)",
                (id, name, game_name, cart, Utc::now()),
            ) .unwrap();

        #[cfg(target_family = "wasm")]
        self.save_db();
    }

    pub fn list_variants(&self, game_name: &str) -> Vec<GameVariant> {
        self.conn
            .prepare("SELECT * FROM variants WHERE game_name = ?1 ORDER BY last_updated")
            .unwrap()
            .query((game_name,))
            .unwrap()
            .mapped(|row| {
                Ok(GameVariant {
                    id: row.get("id")?,
                    name: row.get("name")?,
                    cart: row.get("cartridge")?,
                })
            })
            .collect::<Result<_, _>>()
            .unwrap()
    }

    pub fn update_variant_cart(&self, variant_id: Uuid, cart: &[u8]) {
        self.conn
            .execute(
                "UPDATE variants SET cartridge = ?1, last_updated = ?2 WHERE id = ?3",
                (cart, Utc::now(), variant_id),
            )
            .unwrap();

        #[cfg(target_family = "wasm")]
        self.save_db();
    }

    pub fn list_snapshots(&self, variant_id: Uuid) -> Vec<GameSnapshot> {
        self.conn
            .prepare("SELECT * FROM snapshots WHERE variant_id = ?1 ORDER BY last_updated")
            .unwrap()
            .query((variant_id,))
            .unwrap()
            .mapped(|row| {
                Ok(GameSnapshot {
                    id: row.get("id")?,
                    name: row.get("name")?,
                    variant_id: row.get("variant_id")?,
                    state: row.get("state")?,
                })
            })
            .collect::<Result<_, _>>()
            .unwrap()
    }

    pub fn add_snapshot(
        &self,
        GameSnapshot {
            id,
            name,
            variant_id,
            state,
        }: GameSnapshot,
    ) {
        self.conn
            .execute(
                "INSERT INTO snapshots (id, name, variant_id, state, last_updated) VALUES (?1, ?2, ?3, ?4, ?5)",
                (id, name, variant_id, state, Utc::now()),
            ) .unwrap();

        #[cfg(target_family = "wasm")]
        self.save_db();
    }

    pub fn update_snapshot(&self, snapshot_id: Uuid, state: &[u8]) {
        self.conn
            .execute(
                "UPDATE snapshots SET state = ?1, last_updated = ?2 WHERE id = ?3",
                (state, Utc::now(), snapshot_id),
            )
            .unwrap();

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
}

pub struct GameVariant {
    pub id: Uuid,
    pub name: Arc<str>,
    pub cart: Vec<u8>,
}

impl GameVariant {
    pub fn new(name: impl Into<Arc<str>>, cart: Vec<u8>) -> Self {
        Self {
            id: Uuid::new_v4(),
            name: name.into(),
            cart,
        }
    }
}

pub struct GameSnapshot {
    pub id: Uuid,
    pub name: Arc<str>,
    pub variant_id: Uuid,
    pub state: Vec<u8>,
}

impl GameSnapshot {
    pub fn new(variant_id: Uuid, name: impl Into<Arc<str>>, state: Vec<u8>) -> Self {
        Self {
            id: Uuid::new_v4(),
            name: name.into(),
            variant_id,
            state,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    static EMPTY_ROM: &[u8] = &[0; 0xFFusize];

    #[test]
    fn test_trove() {
        let trove = Trove::new(Connection::open_in_memory().unwrap());

        trove.add_game("test empty", EMPTY_ROM);
        assert_eq!(trove.list_games().len(), 1);
        assert_eq!(trove.list_variants("test empty").len(), 1);

        let variant_id = trove.list_variants("test empty").first().unwrap().id;
        trove.update_variant_cart(variant_id, EMPTY_ROM);
        assert_eq!(trove.list_games().len(), 1);
        assert_eq!(trove.list_variants("test empty").len(), 1);
        assert_eq!(trove.list_snapshots(variant_id).len(), 0);

        let snapshot_id = Uuid::new_v4();
        let snapshot = GameSnapshot {
            id: snapshot_id,
            name: "first snapshot".into(),
            variant_id,
            state: EMPTY_ROM.into(),
        };
        trove.add_snapshot(snapshot);
        assert_eq!(trove.list_games().len(), 1);
        assert_eq!(trove.list_variants("test empty").len(), 1);
        assert_eq!(trove.list_snapshots(variant_id).len(), 1);

        trove.update_snapshot(snapshot_id, EMPTY_ROM);
        assert_eq!(trove.list_games().len(), 1);
        assert_eq!(trove.list_variants("test empty").len(), 1);
        assert_eq!(trove.list_snapshots(variant_id).len(), 1);
    }
}
