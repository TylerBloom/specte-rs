use serde::{Deserialize, Serialize};
use spirit::cpu::{Cpu, Flags};

#[derive(Serialize, Deserialize)]
struct CpuTest {
    name: String,
    initial: CpuState,
    #[serde(rename = "final")]
    end: CpuState,
}

impl CpuTest {
    fn execute(self) {
        let Self {
            name,
            initial: init,
            end,
        } = self;
        let end_addr = init.ram.last().unwrap().0;
        let (mut cpu, mut mem) = init.build();
        while (cpu.pc.0 as usize) < end_addr {
            let op = cpu.read_op(&mem);
            cpu.execute(op, &mut mem);
        }
        end.validate((cpu, mem), &name);
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
        let mut mem = vec![0; 64 * 1014];
        self.ram
            .into_iter()
            .for_each(|RegisterState(addr, val)| mem[addr] = val);
        (cpu, mem)
    }

    fn validate(self, known: (Cpu, Vec<u8>), test_name: &str) {
        assert!(self.build() == known, "Test {test_name:?} failed");
    }
}

/// The addr and expected value at that addr in RAM.
#[derive(Serialize, Deserialize)]
struct RegisterState(usize, u8);

#[test]
fn json_tests() {
    // for file in std::fs::read_dir
    let tests: Vec<CpuTest> = serde_json::from_str(include_str!("data/json_test_00.json")).unwrap();
    tests.into_iter().for_each(CpuTest::execute);
}
