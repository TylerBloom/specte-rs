use std::fmt::Display;
use std::fmt::Write;
use std::path::PathBuf;
use std::rc::Rc;

use crate::GameboyState;
use crate::ppu::Ppu;

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
            // .filter(|(op, _)| *op == 0x09)
            .map(|(op, suite)| (op, suite.execute(op)))
            // .inspect(|(_, report)| println!("{report}"))
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
            // .filter(|report| matches!(report.status, Status::Failed))
            // .take(1)
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
        // println!("Running test: {name}");
        let result = std::panic::catch_unwind(|| {
            let (mut cpu, mut mem) = init.build();
            // The PPU isn't actually used but needed for the internal ticking in the operation's
            // execution.
            let mut ppu = Ppu::new();
            let mut ops = 0;
            // The end state should place the CPUs in (about) the same PC location, but, should
            // a bug exist, that might not happen. No tests contains more than 100 ops. If that
            // occurs, we break
            while cpu.pc != end.pc && ops < 100 {
                ops += 1;
                let mut op = cpu.read_op();
                // println!("Running instruction: {op}");
                // println!("Init CPU: {cpu}");
                let state = GameboyState {
                    mem: &mut mem,
                    ppu: &mut ppu,
                    cpu: &mut cpu,
                };
                op.execute(state);
                // println!("Post CPU: {cpu}\n");
            }
            cpu.pc -= 1u16;
            // We don't care about the "ghost" registers or the instruction regsiter
            cpu.ir = 0.into();
            cpu.z = 0.into();
            cpu.w = 0.into();
            end.validate((cpu, mem))
        });
        let (status, reason) = match result {
            Ok((status, reason)) => (status, reason),
            Err(_err) => {
                let reason =
                    format!("A panic occured while testing op 0x{op_code:0>2X} in test {name:?}");
                // eprintln!("{reason}");
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
    fn build(self) -> (Cpu, Vec<u8>) {
        let cpu = Cpu {
            a: self.a.into(),
            b: self.b.into(),
            c: self.c.into(),
            d: self.d.into(),
            e: self.e.into(),
            f: Flags::from(self.f),
            h: self.h.into(),
            l: self.l.into(),
            // The test cases have the PC already incremented before the instruction is read (or
            // executed). This corrects that offset.
            pc: (self.pc - 1).into(),
            sp: self.sp.into(),
            ime: self.ime.unwrap_or_default(),
            ..Default::default()
        };
        let mut mem = vec![0; 64 * 1024];
        self.ram
            .into_iter()
            .for_each(|RegisterState(addr, val)| mem[addr] = val);
        (cpu, mem)
    }

    fn validate(self, known: (Cpu, Vec<u8>)) -> (Status, String) {
        let (cpu, mem) = self.build();
        if cpu != known.0 {
            return (
                Status::Failed,
                format!("CPU Mismatch:\n\tExpected {cpu}\n\tKnown    {}", known.0),
            );
        }
        if known.1 == mem {
            (Status::Passed, String::new())
        } else {
            let mismatches: Vec<_> = known
                .1
                .iter()
                .zip(mem.iter())
                .enumerate()
                .filter(|(_, (a, b))| **a != 0 || **b != 0)
                .collect();
            let mut addrs = String::new();
            let mut knowns = String::new();
            let mut expecteds = String::new();
            for (addr, (known, expected)) in mismatches {
                _ = write!(addrs, "0x{addr:0>4X} ");
                _ = write!(knowns, "  0x{known:0>2X} ");
                _ = write!(expecteds, "  0x{expected:0>2X} ");
            }
            (
                Status::Failed,
                format!(
                    "Memory diff (addresses, known, expected):\n{addrs}\n{knowns}\n{expecteds}"
                ),
            )
        }
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
