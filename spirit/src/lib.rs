//! Spirit is the core crate for the specters project. Contained here is the all of the logic for
//! creating a gameboy emulator and running it with a game ROM. This crate aims to be platform
//! agnostic and free of the UI-specifics. Other projects will wrap this logic in their own ways.
//! For example, the handheld device will have different IO than a desktop. Moreover, the syncing
//! webservice will not need a UI per se.
//!
//! # Notes
//! The Z80 CPU is big endian.

#![allow(dead_code)]

mod cpu;
mod decoder;
mod emulator;
mod gameboy;
mod instruction;
mod rom;
