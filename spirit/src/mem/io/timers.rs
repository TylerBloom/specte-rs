use std::fmt::Display;

use serde::Deserialize;
use serde::Serialize;

use crate::cpu::check_bit_const;
use crate::utils::Wrapping;

// FIXME: The DIV, DIV counter, and timer control can probably be more directly and effeciently
// modelled a u16...
#[derive(Debug, Clone, Hash, PartialEq, Eq, Serialize, Deserialize)]
pub struct TimerRegisters {
    /// ADDR FF04
    divider_reg: Wrapping<u8>,
    /// The divider reg is incremented every 256 ticks. This tracks those ticks. When the divider
    /// is reset, this counter is reset.
    divider_counter: Wrapping<u8>,
    /// ADDR FF05
    /// The counter that is updated at the frequency specified by the TAC. Overflows trigger resets
    /// to the value in the timer modulo and then an interupt is requested.
    timer_counter: TimerCounter,
    /// ADDR FF06
    /// When the timer counter overflows, it resets to the value in this register.
    timer_modulo: Wrapping<u8>,
    /// ADDR FF07
    timer_control: TimerControl,
}

impl Display for TimerRegisters {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Timers {{")?;
        writeln!(f, "  DIV: 0x{:0>2X}", self.divider_reg.0)?;
        writeln!(f, "  DIV counter: 0x{:0>2X}", self.divider_counter.0)?;
        match self.timer_counter {
            TimerCounter::Loading(count) => writeln!(f, "  TIMA: Loading(0x{count:0>2X})")?,
            TimerCounter::Ready(count) => writeln!(f, "  TIMA: 0x{count:0>2X}")?,
        }
        writeln!(f, "  TMA: 0x{:0>2X}", self.timer_modulo.0)?;
        match self.timer_control {
            TimerControl::Disabled(_) => writeln!(f, "  TAC: Disabled")?,
            TimerControl::Slowest(count) => writeln!(f, "  TAC: Slowest(0x{count:0>2X})")?,
            TimerControl::Slow => writeln!(f, "  TAC: Slow")?,
            TimerControl::Fast => writeln!(f, "  TAC: Fast")?,
            TimerControl::Fastest => writeln!(f, "  TAC: Fastest")?,
        }
        write!(f, "}}")
    }
}

/// When the timer counter overflows, it does not immediately load the timer modulo value. That
/// happens for cycles later. The first variant models this wait.
#[derive(Debug, Clone, Hash, PartialEq, Eq, Serialize, Deserialize)]
enum TimerCounter {
    Loading(u8),
    Ready(Wrapping<u8>),
}

/// Models the timer control (TAC) register, which controls how frequently the timer counter is
/// incremented. There is a variant for each frequency and the disabled state.
///
/// All the variants besides `Disabled` hold a counter that tracks the number of ticks since the
/// last increment.
/// The data in `Disabled` tracks the data that was written to the register when disabled. Without
/// it, the register can not be properly read as part of the memory map.
#[derive(Debug, Clone, Hash, PartialEq, Eq, Serialize, Deserialize)]
pub enum TimerControl {
    Disabled(u8),
    /// The slowest increment speed should inc the timer counter once every 4 div increments. The
    /// inner value tracks the number of dev incs that have passed.
    Slowest(u8),
    /// The slow increment speed should inc the timer counter every time the div increments.
    Slow,
    /// The fast increment speed should inc the timer counter 4 times for every div increment.
    Fast,
    /// The fastest increment speed should inc the timer counter 16 times for every div increment.
    Fastest,
}

impl TimerRegisters {
    pub(super) fn new() -> Self {
        Self {
            divider_reg: Wrapping(0),
            divider_counter: Wrapping(0),
            timer_counter: TimerCounter::Ready(Wrapping(0)),
            timer_modulo: Wrapping(0),
            timer_control: TimerControl::Disabled(0),
        }
    }

    pub(super) fn tick(&mut self) -> bool {
        self.divider_counter += 1;
        if self.divider_counter == 0u8 {
            self.divider_reg += 1;
        }
        if let TimerCounter::Loading(value) = &mut self.timer_counter {
            *value += 1;
            if *value == 4 {
                self.timer_counter = TimerCounter::Ready(self.timer_modulo);
            }
        }
        let inc = match &mut self.timer_control {
            TimerControl::Disabled(_) => false,
            TimerControl::Fastest => self.divider_counter % 16u8 == 0,
            TimerControl::Fast => self.divider_counter % 64u8 == 0,
            TimerControl::Slow => self.divider_counter == 0,
            TimerControl::Slowest(counter) => {
                if self.divider_counter == 0 {
                    *counter += 1;
                }
                let digest = *counter == 4;
                if digest {
                    *counter = 0;
                }
                digest
            }
        };
        if inc { self.inc_timer_counter() } else { false }
    }

    fn inc_timer_counter(&mut self) -> bool {
        let TimerCounter::Ready(value) = &mut self.timer_counter else {
            // We shouldn't ever be here, but we'll just ignore it for now
            return false;
        };
        match value.0.checked_add(1) {
            Some(val) => {
                value.0 = val;
                false
            }
            // `None` indicates ther was an overflow, so we need to request an interupt and set the
            // timer counter to the loading variant.
            None => {
                self.timer_counter = TimerCounter::Loading(0);
                true
            }
        }
    }

    // FIXME: IoRegisters needs to call this method when STOP is called.
    // NOTE: This method is also called when the STOP instruction is called.
    fn reset(&mut self) -> bool {
        // Writting to the div to reset can cause the timer counter to increment.
        let increment = match self.timer_control {
            TimerControl::Disabled(_) => false,
            TimerControl::Fastest => check_bit_const::<3>(self.divider_counter.0),
            TimerControl::Fast => check_bit_const::<5>(self.divider_counter.0),
            TimerControl::Slow => check_bit_const::<7>(self.divider_counter.0),
            // The counter here acts like bits 8 and 9 in the div counter. So, if bit 1 is set,
            // that means bit 9 would have been set in the hardware.
            TimerControl::Slowest(counter) => check_bit_const::<1>(counter),
        };
        let digest = if increment {
            self.inc_timer_counter()
        } else {
            false
        };
        self.divider_reg.0 = 0;
        self.divider_counter.0 = 0;
        self.timer_control.reset();
        digest
    }

    pub(super) fn read_byte(&self, index: u16) -> u8 {
        match index {
            0xFF04 => self.divider_reg.0,
            0xFF05 => match self.timer_counter {
                TimerCounter::Loading(_) => 0,
                TimerCounter::Ready(value) => value.0,
            },
            0xFF06 => self.timer_modulo.0,
            0xFF07 => self.timer_control.as_byte(),
            _ => unreachable!("How did you get here??"),
        }
    }

    pub(super) fn write_byte(&mut self, index: u16, value: u8) -> bool {
        match index {
            0xFF04 => return self.reset(),
            0xFF05 => match &mut self.timer_counter {
                // We discard the value here because, by the time the next operation occurs, this
                // will be overwritten by loading the timer modulo value
                TimerCounter::Loading(_) => {}
                TimerCounter::Ready(val) => val.0 = value,
            },
            0xFF06 => self.timer_modulo.0 = value,
            0xFF07 => self.timer_control = TimerControl::from_byte(value),
            _ => unreachable!("How did you get here??"),
        }
        false
    }
}

impl TimerControl {
    fn from_byte(byte: u8) -> Self {
        // We only care about the first 3 bits.
        match byte & 0b0000_0111 {
            0b101 => Self::Fastest,
            0b110 => Self::Fast,
            0b111 => Self::Slow,
            0b100 => Self::Slowest(0),
            byte => Self::Disabled(byte),
        }
    }

    fn as_byte(&self) -> u8 {
        match self {
            Self::Disabled(byte) => *byte,
            Self::Slowest(_) => 0b000,
            Self::Slow => 0b011,
            Self::Fast => 0b010,
            Self::Fastest => 0b001,
        }
    }

    fn reset(&mut self) {
        if let TimerControl::Slowest(value) = self {
            *value = 0
        }
    }
}

mod test {
    use super::TimerRegisters;

    #[test]
    fn divider_register() {
        let mut regs = TimerRegisters::new();
        assert!((0..64).all(|_| !regs.tick()));
        assert_eq!(regs.divider_reg, 1);
        assert_eq!(regs.divider_counter, 0);
        let mut regs = TimerRegisters::new();
        // We want to tick right up until the register resets. At no point should there be an
        // interupt request since the timer counter is disabled
        let digest = std::iter::repeat_n(0..64u8, u8::MAX as usize)
            .chain(std::iter::once(0..63u8))
            .flatten()
            .all(|_| !regs.tick());
        assert!(digest);
        assert_eq!(regs.divider_reg, 0xFF);
        assert_eq!(regs.divider_counter, 63);
        assert!(!regs.tick());
        assert_eq!(regs.divider_reg, 0);
        assert_eq!(regs.divider_counter, 0);
    }

    /*(
    #[test]
    fn timer_registers_slowest() {
        const TICKS: usize = 0x100;
        let mut regs = TimerRegisters::new();
        regs.timer_control = TimerControl::Slowest(0);
        regs.timer_modulo = 10;
        // Initial tick
        assert!(!regs.tick());
        assert_eq!(regs.timer_counter, 0);
        assert_eq!(regs.timer_modulo, 10);
        assert_eq!(regs.timer_control, TimerControl::Slowest(1));

        // Tick up until counter increments
        assert!((0..TICKS - 2).all(|_| !regs.tick()));
        assert_eq!(regs.timer_counter, 0);
        assert_eq!(regs.timer_modulo, 10);
        assert_eq!(regs.timer_control, TimerControl::Slowest((TICKS - 1) as u8));

        // Tick and the counter should inc
        assert!(!regs.tick());
        assert_eq!(regs.timer_counter, 1);
        assert_eq!(regs.timer_modulo, 10);
        assert_eq!(regs.timer_control, TimerControl::Slowest(0));

        // Tick up until the counter is triggered
        let mut regs = TimerRegisters::new();
        regs.timer_modulo = 10;
        regs.timer_control = TimerControl::Slowest(0);
        let digest = std::iter::repeat_n(0..0x100, 0xFF)
            .flatten()
            .chain(0..TICKS - 1)
            .all(|_| !regs.tick());
        assert!(digest);
        assert_eq!(regs.timer_counter, (TICKS - 1) as u8);
        assert_eq!(regs.timer_modulo, 10);
        assert_eq!(regs.timer_control, TimerControl::Slowest((TICKS - 1) as u8));

        // Tick and verify the reset
        assert!(regs.tick());
        assert_eq!(regs.timer_counter, 10);
        assert_eq!(regs.timer_modulo, 10);
        assert_eq!(regs.timer_control, TimerControl::Slowest(0));
    }

    #[test]
    fn timer_registers_slow() {
        const TICKS: u8 = 64;
        let mut regs = TimerRegisters::new();
        regs.timer_modulo = 10;
        regs.timer_control = TimerControl::Slow;
        // Initial tick
        assert!(!regs.tick());
        assert_eq!(regs.timer_counter, 0);
        assert_eq!(regs.timer_modulo, 10);
        assert_eq!(regs.timer_control, TimerControl::Slow);

        // Tick up until counter increments
        assert!((0..TICKS - 2).all(|_| !regs.tick()));
        assert_eq!(regs.timer_counter, 0);
        assert_eq!(regs.timer_modulo, 10);
        assert_eq!(regs.timer_control, TimerControl::Slow);

        // Tick and the counter should inc
        assert!(!regs.tick());
        assert_eq!(regs.timer_counter, 1);
        assert_eq!(regs.timer_modulo, 10);
        assert_eq!(regs.timer_control, TimerControl::Slow);

        // Tick up until the counter is triggered
        let mut regs = TimerRegisters::new();
        regs.timer_modulo = 10;
        regs.timer_control = TimerControl::Slow;
        let digest = std::iter::repeat_n(0..TICKS, 0xFF)
            .flatten()
            .chain(0..TICKS - 1)
            .all(|_| !regs.tick());
        assert!(digest);
        assert_eq!(regs.timer_counter, 0xFF);
        assert_eq!(regs.timer_modulo, 10);
        assert_eq!(regs.timer_control, TimerControl::Slow);

        // Tick and verify the reset
        assert!(regs.tick());
        assert_eq!(regs.timer_counter, 10);
        assert_eq!(regs.timer_modulo, 10);
        assert_eq!(regs.timer_control, TimerControl::Slow);
    }

    #[test]
    fn timer_registers_fast() {
        const TICKS: u8 = 16;
        let mut regs = TimerRegisters::new();
        regs.timer_modulo = 10;
        regs.timer_control = TimerControl::Fast;
        // Initial tick
        assert!(!regs.tick());
        assert_eq!(regs.timer_counter, 0);
        assert_eq!(regs.timer_modulo, 10);
        assert_eq!(regs.timer_control, TimerControl::Fast);

        // Tick up until counter increments
        assert!((0..TICKS - 2).all(|_| !regs.tick()));
        assert_eq!(regs.timer_counter, 0);
        assert_eq!(regs.timer_modulo, 10);
        assert_eq!(regs.timer_control, TimerControl::Fast);

        // Tick and the counter should inc
        assert!(!regs.tick());
        assert_eq!(regs.timer_counter, 1);
        assert_eq!(regs.timer_modulo, 10);
        assert_eq!(regs.timer_control, TimerControl::Fast);

        // Tick up until the counter is triggered
        let mut regs = TimerRegisters::new();
        regs.timer_control = TimerControl::Fast;
        regs.timer_modulo = 10;
        let digest = std::iter::repeat_n(0..TICKS, 0xFF)
            .flatten()
            .chain(0..TICKS - 1)
            .all(|_| !regs.tick());
        assert!(digest);
        assert_eq!(regs.timer_counter, 0xFF);
        assert_eq!(regs.timer_modulo, 10);
        assert_eq!(regs.timer_control, TimerControl::Fast);

        // Tick and verify the reset
        assert!(regs.tick());
        assert_eq!(regs.timer_counter, 10);
        assert_eq!(regs.timer_modulo, 10);
        assert_eq!(regs.timer_control, TimerControl::Fast);
    }

    #[test]
    fn timer_registers_fastest() {
        const TICKS: u8 = 4;
        let mut regs = TimerRegisters::new();
        regs.timer_modulo = 10;
        regs.timer_control = TimerControl::Fastest;
        // Initial tick
        assert!(!regs.tick());
        assert_eq!(regs.timer_counter, 0);
        assert_eq!(regs.timer_modulo, 10);
        assert_eq!(regs.timer_control, TimerControl::Fastest);

        // Tick up until counter increments
        assert!((0..TICKS - 2).all(|_| !regs.tick()));
        assert_eq!(regs.timer_counter, 0);
        assert_eq!(regs.timer_modulo, 10);
        assert_eq!(regs.timer_control, TimerControl::Fastest);

        // Tick and the counter should inc
        assert!(!regs.tick());
        assert_eq!(regs.timer_counter, 1);
        assert_eq!(regs.timer_modulo, 10);
        assert_eq!(regs.timer_control, TimerControl::Fastest);

        // Tick up until the counter is triggered
        let mut regs = TimerRegisters::new();
        regs.timer_control = TimerControl::Fastest;
        regs.timer_modulo = 10;
        let digest = std::iter::repeat_n(0..TICKS, 0xFF)
            .flatten()
            .chain(0..TICKS - 1)
            .all(|_| !regs.tick());
        assert!(digest);
        assert_eq!(regs.timer_counter, 0xFF);
        assert_eq!(regs.timer_modulo, 10);
        assert_eq!(regs.timer_control, TimerControl::Fastest);

        // Tick and verify the reset
        assert!(regs.tick());
        assert_eq!(regs.timer_counter, 10);
        assert_eq!(regs.timer_modulo, 10);
        assert_eq!(regs.timer_control, TimerControl::Fastest);
    }
    */
}
