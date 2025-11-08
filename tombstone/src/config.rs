use std::{
    collections::{HashMap, HashSet},
    path::PathBuf,
};

use serde::{Deserialize, Serialize};

use crate::command::Command;

#[derive(Debug, Default, Serialize, Deserialize)]
pub struct Config {
    per_roms: HashMap<String, PerRom>,
}

impl Config {
    pub fn load() -> Self {
        let path = Self::config_path();
        let data = std::fs::read_to_string(path).unwrap_or_default();
        toml::from_str(&data).unwrap_or_default()
    }

    pub fn save(&self) {
        let data = toml::to_string(self).unwrap();
        let path = Self::config_path();
        std::fs::write(path, data).unwrap();
    }

    fn config_path() -> PathBuf {
        let mut path = env!("CARGO_MANIFEST_DIR").parse::<PathBuf>().unwrap();
        path.push("config.toml");
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
    macros: Vec<Vec<Command>>,
}
