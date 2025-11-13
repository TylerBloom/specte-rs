use std::collections::HashMap;
use std::collections::HashSet;
use std::path::PathBuf;

use serde::Deserialize;
use serde::Serialize;
use spirit::Gameboy;

use crate::command::Command;

#[derive(Debug, Default, Serialize, Deserialize)]
pub struct Config {
    per_roms: HashMap<String, PerRom>,
}

impl Config {
    pub fn load() -> Self {
        let path = Self::config_path();
        let data = std::fs::read(path).unwrap_or_default();
        serde_cbor::from_slice(&data).unwrap_or_default()
    }

    pub fn save(&self) {
        let data = serde_cbor::to_vec(self).unwrap();
        let path = Self::config_path();
        std::fs::write(path, data).unwrap();
    }

    fn config_path() -> PathBuf {
        let mut path = env!("CARGO_MANIFEST_DIR").parse::<PathBuf>().unwrap();
        path.push("config.cbor");
        path
    }

    pub fn load_game_config(mut self, rom: &[u8]) -> GameConfig {
        let id = format!("{:X}", md5::compute(rom));
        let game_config = self.per_roms.entry(id.clone()).or_default().clone();
        GameConfig {
            id,
            game_config,
            whole_config: self,
        }
    }
}

/// This is a wrapper type around a config for a specific ROM's config and the subsumming debugger
/// config. Since the majority of the changes and querying done during a session are for a specific
/// game, that specifc game config is duplicated and any changes are duplicated to the larger
/// config.
pub struct GameConfig {
    id: String,
    whole_config: Config,
    game_config: PerRom,
}

impl GameConfig {
    pub fn is_breakpoint(&self, pc: u16) -> bool {
        self.game_config.breakpoints.contains(&pc)
    }

    pub fn breakpoints(&self) -> impl Iterator<Item = u16> {
        self.game_config.breakpoints.iter().copied()
    }

    pub fn add_breakpoint(&mut self, bp: u16) {
        self.game_config.breakpoints.insert(bp);
        self.get_game_config().breakpoints.insert(bp);
        self.whole_config.save()
    }

    pub fn remove_breakpoint(&mut self, bp: u16) {
        self.game_config.breakpoints.remove(&bp);
        self.get_game_config().breakpoints.remove(&bp);
        self.whole_config.save()
    }

    pub fn add_macro(&mut self, name: String, mac: Vec<Command>) {
        self.game_config.macros.insert(name.clone(), mac.clone());
        self.get_game_config().macros.insert(name, mac);
        self.whole_config.save()
    }

    pub fn remove_macro(&mut self, name: &str) {
        self.game_config.macros.remove(name);
        self.get_game_config().macros.remove(name);
        self.whole_config.save()
    }

    pub fn get_macro(&self, name: &str) -> Option<&[Command]> {
        self.game_config.macros.get(name).map(Vec::as_slice)
    }

    pub fn add_save(&mut self, name: String, gb: Gameboy) {
        self.game_config.saves.insert(name.clone(), gb.clone());
        self.get_game_config().saves.insert(name, gb);
        self.whole_config.save()
    }

    pub fn remove_save(&mut self, name: &str) {
        self.game_config.saves.remove(name);
        self.get_game_config().saves.remove(name);
        self.whole_config.save()
    }

    pub fn get_save(&self, name: &str) -> Option<&Gameboy> {
        self.game_config.saves.get(name)
    }

    pub fn get_game_config(&mut self) -> &mut PerRom {
        self.whole_config
            .per_roms
            .get_mut(&self.id)
            .expect("This should be inserted on construction")
    }
}

#[derive(Debug, Default, Clone, Serialize, Deserialize)]
pub struct PerRom {
    breakpoints: HashSet<u16>,
    macros: HashMap<String, Vec<Command>>,
    saves: HashMap<String, Gameboy>,
}
