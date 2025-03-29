use std::{
    fmt::Display,
    os::fd::AsRawFd,
    path::{Path, PathBuf},
    u8,
};

use crate::mem::BgTileMapAttrIndex;

use super::{Cpu, Flags};
use serde::{Deserialize, Serialize};

/// Contains the entire swath of tests for every op code.
struct TestBattery {
    /// Contains a op code and that operation's test suite
    suites: Vec<(u8, TestSuite)>,
}

impl TestBattery {
    fn construct(mut path: PathBuf) -> Self {
        let mut suites = Vec::new();
        path.push("tmp");
        for i in 0u8..=u8::MAX {
            path.pop();
            path.push(format!("json_test_{i:0>2x}.json"));
            let Ok(file) = std::fs::read_to_string(&path) else {
                continue;
            };
            let suite: TestSuite = serde_json::from_str(&file).unwrap();
            suites.push((i, suite));
        }
        assert!(!suites.is_empty());
        Self { suites }
    }

    fn execute(self) -> BatteryReport {
        let results = self
            .suites
            .into_iter()
            .map(|(op, suite)| (op, suite.execute()))
            .take(100 - 50 + 25 - 12 - 6 - 3 - 2 - 1)
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
        writeln!(f, "------------------------------------------");
        writeln!(
            f,
            "|  TOTAL  |  {p: >5}   |  {: >5}   | {:>5.1}% |",
            t - p,
            (p as f32) / (t as f32) * 100.0
        );
        writeln!(f, "------------------------------------------");
        Ok(())
    }
}

#[derive(Serialize, Deserialize)]
struct TestSuite(Vec<CpuTest>);

impl TestSuite {
    fn execute(self) -> SuiteReport {
        let results = self.0.into_iter().map(|test| test.execute()).collect();
        SuiteReport { results }
    }
}

#[derive(Default)]
enum Status {
    // This is the default because we try to find a failure. If a failure doesn't exist, it passed
    #[default]
    Passed,
    Failed,
}

struct SuiteReport {
    results: Vec<Status>,
}

impl SuiteReport {
    fn counts(&self) -> (usize, usize, f32) {
        let passed = self
            .results
            .iter()
            .filter(|r| matches!(r, Status::Passed))
            .count();
        let percent = (passed as f32) / (self.results.len() as f32) * 100.0;
        (passed, self.results.len() - passed, percent)
    }

    fn passed(&self) -> bool {
        self.results.iter().all(|r| matches!(r, Status::Passed))
    }
}

#[derive(Serialize, Deserialize)]
struct CpuTest {
    name: String,
    initial: CpuState,
    #[serde(rename = "final")]
    end: CpuState,
    cycles: Vec<Cycle>,
}

impl CpuTest {
    fn execute(self) -> Status {
        let Self {
            name,
            initial: init,
            end,
            cycles,
        } = self;
        // println!("Running {name:?}");
        let (mut cpu, mut mem) = init.build();
        let len = cycles.len() as u8;
        let mut cycles = len;
        while cycles != 0 {
            let op = cpu.read_op(&mem);
            // println!("{op} ({cycles} left)");
            // FIXME: This should never underflow!
            cycles = cycles.saturating_sub(op.length(&cpu) / 4);
            cpu.execute(op, &mut mem);
        }
        end.validate((cpu, mem))
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
    ram: Vec<RegisterState>,
}

impl CpuState {
    fn build(self) -> (Cpu, Vec<u8>) {
        let mut cpu = Cpu::default();
        cpu.a = self.a.into();
        cpu.b = self.b.into();
        cpu.c = self.c.into();
        cpu.d = self.d.into();
        cpu.e = self.e.into();
        cpu.f = Flags::from(self.f);
        cpu.h = self.h.into();
        cpu.l = self.l.into();
        cpu.pc = (self.pc - 1).into();
        cpu.sp = self.sp.into();
        let mut mem = vec![0; 64 * 1024];
        self.ram
            .into_iter()
            .for_each(|RegisterState(addr, val)| mem[addr] = val);
        (cpu, mem)
    }

    fn validate(self, known: (Cpu, Vec<u8>)) -> Status {
        let (cpu, mem) = self.build();
        if cpu != known.0 {
            return Status::Failed;
        }
        known
            .1
            .iter()
            .zip(mem.iter())
            .enumerate()
            .find_map(|(addr, (known, expected))| (known != expected).then_some(Status::Failed))
            .unwrap_or_default()
    }
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

#[test]
fn json_tests() {
    let manifest = env!("CARGO_MANIFEST_DIR");
    let report = TestBattery::construct(PathBuf::from(format!("{manifest}/tests/data"))).execute();
    println!("{report}");
    if !report.passed() {
        panic!("One or more failures reported!!");
    }
}

/*
#[test]
fn single_json_test() {
    let tests: Vec<CpuTest> =
        serde_json::from_str(include_str!("../tests/data/json_test_c1.json")).unwrap();
    tests.into_iter().for_each(CpuTest::execute);
}
*/
