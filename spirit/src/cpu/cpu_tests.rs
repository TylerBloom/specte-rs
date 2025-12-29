use std::fmt::Display;
use std::path::PathBuf;
use std::rc::Rc;

use crate::mem::MemoryBank;
use crate::mem::MemoryBankController;
use crate::mem::MemoryMap;
use crate::mem::RamBank;
use crate::mem::VramDma;
use crate::mem::io::IoRegisters;
use crate::mem::vram::VRam;

use super::Cpu;
use super::Flags;
use serde::Deserialize;
use serde::Serialize;

#[test]
fn json_tests_00() {
    let manifest = env!("CARGO_MANIFEST_DIR");
    let report =
        TestBattery::<0x00>::construct(PathBuf::from(format!("{manifest}/tests/data"))).execute();
    println!("{report}");
    if !report.passed() {
        panic!("One or more failures reported!!");
    }
}

#[test]
fn json_tests_40() {
    let manifest = env!("CARGO_MANIFEST_DIR");
    let report =
        TestBattery::<0x40>::construct(PathBuf::from(format!("{manifest}/tests/data"))).execute();
    println!("{report}");
    if !report.passed() {
        panic!("One or more failures reported!!");
    }
}

#[test]
fn json_tests_80() {
    let manifest = env!("CARGO_MANIFEST_DIR");
    let report =
        TestBattery::<0x80>::construct(PathBuf::from(format!("{manifest}/tests/data"))).execute();
    println!("{report}");
    if !report.passed() {
        panic!("One or more failures reported!!");
    }
}

#[test]
fn json_tests_c0() {
    let manifest = env!("CARGO_MANIFEST_DIR");
    let report =
        TestBattery::<0xC0>::construct(PathBuf::from(format!("{manifest}/tests/data"))).execute();
    println!("{report}");
    if !report.passed() {
        panic!("One or more failures reported!!");
    }
}

/// Contains the entire swath of tests for every op code.
struct TestBattery<const START: u8> {
    /// Contains a op code and that operation's test suite
    suites: Vec<(u8, TestSuite)>,
}

impl<const START: u8> TestBattery<START> {
    fn construct(path: PathBuf) -> Self {
        let mut suites = Vec::new();
        let end: u8 = START + 0x3F;
        for i in START..=end {
            let mut path = path.clone();
            path.push(format!("json_test_{i:0>2x}.json"));
            let Ok(file) = std::fs::read_to_string(&path) else {
                println!("Failed to file: {path:?}");
                continue;
            };
            let suite: TestSuite = serde_json::from_str(&file).unwrap();
            suites.push((i, suite));
        }
        assert!(!suites.is_empty());
        Self { suites }
    }

    fn execute(self) -> BatteryReport {
        println!("Suite count: {}", self.suites.len());
        let results = self
            .suites
            .into_iter()
            .map(|(op, suite)| (op, suite.execute(op)))
            .collect();
        BatteryReport { results }
    }
}

/// Contains the data from executing the entire battery of tests.
struct BatteryReport {
    results: Vec<(u8, SuiteReport)>,
}

impl BatteryReport {
    fn passed(&self) -> bool {
        self.results.iter().all(|(_, report)| report.passed())
    }
}

impl Display for BatteryReport {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "------------------------------------------")?;
        writeln!(f, "| OP Code | # Passed | # Failed | Pass % |")?;
        writeln!(f, "|---------|----------|----------|--------|")?;
        let (mut p, mut t) = (0, 0);
        self.results
            .iter()
            .map(|(op, report)| (op, report.counts()))
            // The most tests any report will have 25,600, so pass/fail counts need to be 5 chars
            // wide
            // write!(f, "|         |          |          |        |")?;
            .try_for_each(|(op, (passed, failed, percent))| {
                p += passed;
                t += passed + failed;
                writeln!(
                    f,
                    "|  0x{op:0>2X}   |  {passed: >5}   |  {failed: >5}   | {percent:>5.1}% |"
                )
            })?;
        writeln!(f, "------------------------------------------")?;
        writeln!(
            f,
            "|  TOTAL  |  {p: >5}   |  {: >5}   | {:>5.1}% |",
            t - p,
            (p as f32) / (t as f32) * 100.0
        )?;
        writeln!(f, "------------------------------------------")
    }
}

#[derive(Serialize, Deserialize)]
struct TestSuite(Vec<CpuTest>);

impl TestSuite {
    fn execute(self, op_code: u8) -> SuiteReport {
        let results = self
            .0
            .into_iter()
            .map(|test| test.execute(op_code))
            .collect();
        SuiteReport { results }
    }
}

#[derive(Debug, Default)]
enum Status {
    // This is the default because we try to find a failure. If a failure doesn't exist, it passed
    #[default]
    Passed,
    Failed,
}

struct SuiteReport {
    results: Vec<TestReport>,
}

impl Display for SuiteReport {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.results.iter().try_for_each(|report| {
            writeln!(
                f,
                "{}: {:?} -> {}",
                report.name, report.status, report.reason
            )
        })
    }
}

impl SuiteReport {
    fn counts(&self) -> (usize, usize, f32) {
        let passed = self
            .results
            .iter()
            .filter(|r| matches!(r.status, Status::Passed))
            .count();
        let percent = (passed as f32) / (self.results.len() as f32) * 100.0;
        (passed, self.results.len() - passed, percent)
    }

    fn passed(&self) -> bool {
        self.results
            .iter()
            .all(|r| matches!(r.status, Status::Passed))
    }
}

struct TestReport {
    name: Rc<str>,
    status: Status,
    reason: String,
}

#[derive(Serialize, Deserialize)]
struct CpuTest {
    name: Rc<str>,
    initial: CpuState,
    #[serde(rename = "final")]
    end: CpuState,
    cycles: Vec<Cycle>,
}

impl CpuTest {
    fn execute(self, op_code: u8) -> TestReport {
        let Self {
            name,
            initial: init,
            end,
            cycles,
        } = self;
        let result = std::panic::catch_unwind(|| {
            let (mut cpu, mut mem) = init.build();
            let len = cycles.len() as u8;
            let mut cycles = len;
            while cycles != 0 {
                let op = cpu.read_op(&mem);
                cycles = cycles.saturating_sub(op.length(&cpu) / 4);
                cpu.execute(op, &mut mem);
            }
            end.validate((cpu, mem))
        });
        let (status, reason) = match result {
            Ok((status, reason)) => (status, reason),
            Err(_err) => {
                let reason =
                    format!("A panic occured while testing op 0x{op_code:0>2X} in test {name:?}");
                eprintln!("{reason}");
                (Status::Failed, reason)
            }
        };
        TestReport {
            name,
            status,
            reason,
        }
    }
}

#[derive(Serialize, Deserialize)]
struct CpuState {
    a: u8,
    b: u8,
    c: u8,
    d: u8,
    e: u8,
    f: u8,
    h: u8,
    l: u8,
    pc: u16,
    sp: u16,
    ime: Option<bool>,
    ram: Vec<RegisterState>,
}

impl CpuState {
    fn build(self) -> (Cpu, MemoryMap) {
        let cpu = Cpu {
            a: self.a.into(),
            b: self.b.into(),
            c: self.c.into(),
            d: self.d.into(),
            e: self.e.into(),
            f: Flags::from(self.f),
            h: self.h.into(),
            l: self.l.into(),
            pc: (self.pc - 1).into(),
            sp: self.sp.into(),
            ime: self.ime.unwrap_or_default(),
            ..Default::default()
        };
        let mut rom = MemoryBank::default();
        let mut ram = RamBank::default();
        let mut vram = VRam::default();
        let mut wram = [[0; 0x1000]; 2];
        let mut hram = [0; 0x7F];
        let mut io = IoRegisters::default();
        let mut forbidden = [0; 0x60];
        let mut vram_dma = VramDma::default();
        for RegisterState(addr, val) in self.ram {
            match addr {
                0x0000..=0x7FFF => rom[addr] = val,
                0x8000..=0x9FFF => vram.vram[0][addr - 0x8000] = val,
                0xA000..=0xBFFF => ram[addr - 0xA000] = val,
                0xC000..=0xCFFF => wram[0][addr - 0xC000] = val,
                0xD000..=0xDFFF => wram[1][addr - 0xD000] = val,
                // WRAM echos
                0xE000..=0xEFFF => wram[1][addr - 0xE000] = val,
                0xF000..=0xFDFF => wram[0][addr - 0xF000] = val,
                // OAM
                0xFE00..=0xFE9F => vram.oam[addr - 0xFE00] = val,
                // Forbidden
                0xFEA0..=0xFEFF => forbidden[addr - 0xFEA0] = val,
                // OAM DMA
                0xFF51 => vram_dma.src_hi = val,
                0xFF52 => vram_dma.src_lo = val,
                0xFF53 => vram_dma.dest_hi = val,
                0xFF54 => vram_dma.dest_lo = val,
                0xFF55 => vram_dma.trigger(val),
                // IO
                0xFF00..=0xFF7F => io.write_byte(addr as u16, val),
                // HRAM
                0xFF80..=0xFFFE => hram[addr - 0xFF80] = val,
                _ => panic!("Tried to write to 0x{addr:0>4X} ({addr})"),
            }
        }
        let mem = MemoryMap {
            mbc: MemoryBankController::Direct { rom, ram },
            vram,
            wram,
            hr: hram,
            io,
            forbidden,
            vram_dma,
            ..Default::default()
        };
        (cpu, mem)
    }

    fn validate(self, (known_cpu, known_mem): (Cpu, MemoryMap)) -> (Status, String) {
        let (known_rom, known_ram) = get_mbc_direct_memory(known_mem.mbc);
        let (cpu, mem) = self.build();
        let (rom, ram) = get_mbc_direct_memory(mem.mbc);
        if cpu != known_cpu {
            return (
                Status::Failed,
                format!("CPU Mismatch:\n\tExpected {cpu}\n\tKnown    {}", known_cpu),
            );
        }
        known_rom.into_iter().zip(rom)
            .chain(known_ram.into_iter().zip(ram))
            .enumerate()
            .find_map(|(addr, (known, expected))| {
                (known != expected).then(|| {
                    (
                        Status::Failed,
                        format!("Mismatch value @ 0x{addr:0>4X} (aka ({addr})): Expected {expected}, Known {known}"),
                    )
                })
            })
            .unwrap_or_default()
    }
}

fn get_mbc_direct_memory(mbc: MemoryBankController) -> (MemoryBank<0x8000>, RamBank) {
    let MemoryBankController::Direct { rom, ram } = mbc else {
        panic!("Non-direct MBC used");
    };
    (rom, ram)
}

/// The addr and expected value at that addr in RAM.
#[derive(Serialize, Deserialize)]
struct RegisterState(usize, u8);

type Cycle = Option<(usize, u8, CycleStatus)>;

#[derive(Serialize, Deserialize)]
enum CycleStatus {
    #[serde(rename = "read")]
    Read,
    #[serde(rename = "write")]
    Write,
}
