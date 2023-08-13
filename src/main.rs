//!
//!
//!
//! # Notes
//! The Z80 CPU is big endian.

#![allow(dead_code)]

use std::{
    include_str,
    {
        collections::HashMap,
        fmt::{self, Display, Formatter},
    },
};

use serde::{Deserialize, Serialize};

mod rom;

const OPCODE_DATA: &str = include_str!("../roms/opcodes.json");

fn main() {
    let codes: OpCodes = serde_json::from_str(OPCODE_DATA).unwrap();
    println!(
        "{:?}",
        codes
            .unprefixed
            .iter()
            .find(|(i, _)| **i == HexStr([0]))
            .unwrap()
    );
    println!("{:?}", codes.get_unprefixed(0));
}

#[derive(Debug, Serialize, Deserialize)]
pub struct OpCodes {
    unprefixed: HashMap<HexStr, Instruction>,
    #[serde(rename = "cbprefixed")]
    cb_prefixed: HashMap<HexStr, Instruction>,
}

impl OpCodes {
    fn get_unprefixed(&self, index: u8) -> &Instruction {
        self.unprefixed.get(&HexStr([index])).unwrap()
    }
}

#[derive(Debug, Hash, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(transparent)]
#[repr(transparent)]
pub struct HexStr(#[serde(with = "hex::serde")] pub [u8; 1]);

#[derive(Debug, Serialize, Deserialize)]
pub struct Instruction {
    mnemonic: String,
    bytes: u8,
    cycles: Vec<usize>,
    operands: Vec<Operand>,
    immediate: bool,
    // TODO: Fix this...
    // flags: Option<Flag>,
    #[serde(default)]
    comment: String,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Operand {
    name: String,
    #[serde(default)]
    bytes: u8,
    immediate: bool,
}

#[derive(Debug, Serialize, Deserialize)]
pub enum Flag {
    Plus,
    Minus,
}

impl Display for Instruction {
    fn fmt(&self, _f: &mut Formatter<'_>) -> fmt::Result {
        todo!()
    }
}
