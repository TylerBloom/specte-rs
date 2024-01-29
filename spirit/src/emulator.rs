use crate::{cpu::{Cpu, FullRegister}, instruction::Instruction};



/// The main emulator type. Holds the ROM, CPU, etc.
pub struct Emulator {
    rom: Vec<u8>,
    cpu: Cpu,
}

impl Emulator {
    pub fn run(&mut self) -> ! {
        loop {
            let instr = self.read_instr();
            self.execute(instr);
        }
    }

    fn get_head(&self) -> usize {
        self.cpu[FullRegister::PC] as usize
    }

    fn inc_head(&mut self, inc: usize) {
        self.cpu[FullRegister::PC] += inc as u16;
    }

    fn read_instr(&mut self) -> Instruction {
        let (inc, instr) = Instruction::parse(&self.rom[self.get_head()..]);
        self.inc_head(inc);
        instr
    }

    fn execute(&mut self, instr: Instruction) {
        todo!()
    }
}
