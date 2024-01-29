# About
This repo contains an experimental cross-platform GBC emulator written in Rust.
Along with the raw emulator, this repo contains a series of tools and services to make the emulation expirence as smooth as possible.

Currently, this project is just getting started, but the current scope includes:
 - A GBC emulator that can be ran (at least):
   - Natively on Linux, Windows, and MacOs
   - In the browser via WASM
   - On embed devices including Raspberry PI, ESP32, and more
 - Save and file synchronization between all clients, including via a wire, the local network, bluetooth
 - Additional save syncing and backends will be available via a webservice

# Disclaimer
Construction of this emulator is just starting.
It remains unclear how much of the above scope is feasible.
