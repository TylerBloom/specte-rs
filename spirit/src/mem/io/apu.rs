use serde::Deserialize;
use serde::Serialize;

#[derive(Debug, Default, Clone, Hash, PartialEq, Eq, Serialize, Deserialize)]
pub struct AudioRegisters {
    /// The volume control for the amplifier
    /// This register is at FF24
    pub(crate) master_control_vin_planning: u8,

    /// The control for the panning of each channel
    /// This register is at FF26
    pub(crate) panning: u8,

    /// The master control for audio.
    /// This register is at FF26
    pub(crate) master_control: u8,

    /// The control for the CH1 sweep
    /// This register is at FF10
    pub(crate) ch1_sweep: u8,

    /// The control for the CH1 length timer and duty cycle.
    /// This register is at FF11
    pub(crate) ch1_length_and_duty: u8,

    /// The control for the CH1 volume and envelope.
    /// This register is at FF12
    pub(crate) ch1_vol_and_env: u8,

    /// The CH1 period low bits.
    /// This register is at FF12
    ///
    /// NOTE: This register is write only. Reads return 0xFF.
    pub(crate) ch1_period_low: u8,

    /// The control for the CH1 volume and envelope.
    /// This register is at FF12
    ///
    /// NOTE: Bits 3, 4, and 5 are unused. Also, Bits 0, 1, 2, and 7 are write only.
    pub(crate) ch1_period_and_control: u8,

    /// The control for the CH2 length timer and duty cycle.
    /// This register is at FF16
    pub(crate) ch2_length_and_duty: u8,

    /// The control for the CH2 volume and envelope.
    /// This register is at FF17
    pub(crate) ch2_vol_and_env: u8,

    /// The CH2 period low bits.
    /// This register is at FF18
    ///
    /// NOTE: This register is write only. Reads return 0xFF.
    pub(crate) ch2_period_low: u8,

    /// The control for the CH2 volume and envelope.
    /// This register is at FF19
    ///
    /// NOTE: Bits 3, 4, and 5 are unused. Also, Bits 0, 1, 2, and 7 are write only.
    pub(crate) ch2_period_and_control: u8,

    /// The control for the CH3 DAC. Only one bit is used, bit 7.
    /// This register is at FF1A
    pub(crate) ch3_dac_enable: u8,

    /// The control for the CH3 length timer.
    /// This register is at FF1B
    ///
    /// NOTE: This register is write-only.
    pub(crate) ch3_length_timer: u8,

    /// The control for the CH3 output level. Only the 5th and 6th bits are used.
    /// This register is at FF1C
    pub(crate) ch3_output_level: u8,

    /// The control for the CH3 period control.
    /// This register is at FF1D
    ///
    /// NOTE: This register is write-only.
    pub(crate) ch3_period_low: u8,

    /// The control for the CH3 period control.
    /// This register is at FF1E.
    ///
    /// NOTE: Bits 3, 4, and 5 are unused. Also, Bits 0, 1, 2, and 7 are write only.
    pub(crate) ch3_period_and_control: u8,

    /// FF30-FF3F
    pub(crate) ch3_wave_form: [u8; 0x10],

    /// The control for the CH4 length timer
    /// This register is at FF20
    ///
    /// NOTE: This register is write-only, and the top two bits are not used.
    pub(crate) ch4_length_timer: u8,

    /// The control for the CH4 volume and envelope.
    /// This register is at FF21
    pub(crate) ch4_vol_and_env: u8,

    /// The control for the CH4 frequency and randomness
    /// This register is at FF22
    pub(crate) ch4_freq_and_rand: u8,

    /// The CH4 control
    /// This register is at FF23
    pub(crate) ch4_control: u8,
}

impl AudioRegisters {
    pub fn read_byte(&self, index: u16) -> u8 {
        match index {
            /* Channel 1 */
            0xFF10 => self.ch1_sweep,
            0xFF11 => self.ch1_length_and_duty,
            0xFF12 => self.ch1_vol_and_env,
            // NOTE: self.ch1_period_low is write-only
            0xFF13 => 0xFF,
            // NOTE: Only the 6th bit is readable
            0xFF14 => self.ch1_period_and_control,
            0xFF15 => 0, // Unused register

            /* Channel 2 */
            0xFF16 => self.ch2_length_and_duty,
            0xFF17 => self.ch2_vol_and_env,
            // NOTE: self.ch2_period_low is write-only
            0xFF18 => 0xFF,
            // NOTE: Only the 6th bit is readable
            0xFF19 => self.ch2_period_and_control,

            /* Channel 3 */
            // NOTE: Only the topmost bit is used
            0xFF1A => self.ch3_dac_enable,
            // NOTE: self.ch3_length_timer is write-only
            0xFF1B => 0xFF,
            // NOTE: Only bits 5 and 6 are used
            0xFF1C => self.ch3_output_level,
            // NOTE: self.ch3_period_low is write-only
            0xFF1D => 0xFF,
            // NOTE: Only bit 6 is readable
            0xFF1E => self.ch3_period_and_control,
            0xFF1F => 0, // Unused register

            /* Channel 4 */
            // NOTE: self.ch4_length_timer is write-only
            0xFF20 => 0xFF,
            0xFF21 => self.ch4_vol_and_env,
            0xFF22 => self.ch4_freq_and_rand,
            // NOTE: Only bit 6 is readable
            0xFF23 => self.ch4_control,

            /* Controls */
            0xFF24 => self.master_control_vin_planning,
            0xFF25 => self.panning,
            // NOTE: Bits 4, 5, 6 are not used
            0xFF26 => self.master_control,

            0xFF27..0xFF30 => 0, // Unused registers

            // Wave pattern registers
            n @ 0xFF30..=0xFF3F => self.ch3_wave_form[(n - 0xFF30) as usize],

            /* Oops... */
            idx => unreachable!(
                "APU registers must be between 0xFF10..=0xFF3F. 0x{index:4>0X} is not."
            ),
        }
    }

    pub fn write_byte(&mut self, index: u16, mut val: u8) {
        match index {
            /* Channel 1 */
            // NOTE: The top bit is not used
            0xFF10 => self.ch1_sweep = val & 0b0111_1111,
            0xFF11 => self.ch1_length_and_duty = val,
            0xFF12 => self.ch1_vol_and_env = val,
            0xFF13 => self.ch1_period_low = val,
            // NOTE: Bits 3, 4, 5 are not used
            0xFF14 => self.ch1_period_and_control = val & 0b1100_0111,

            /* Channel 2 */
            0xFF16 => self.ch2_length_and_duty = val,
            0xFF17 => self.ch2_vol_and_env = val,
            0xFF18 => self.ch2_period_low = val & 0b1100_0111,
            // NOTE: Bits 3, 4, 5 are not used
            0xFF19 => self.ch2_period_and_control = val & 0b1100_0111,

            /* Channel 3 */
            // NOTE: Only the topmost bit is used
            0xFF1A => self.ch3_dac_enable = val & 0b1000_0000,
            0xFF1B => self.ch3_length_timer = val,
            // NOTE: Only bits 5 and 6 are usd
            0xFF1C => self.ch3_output_level = val & 0b0110_0000,
            0xFF1D => self.ch3_period_low = val,
            // NOTE: Bits 3, 4, 5 are not used
            0xFF1E => self.ch3_period_and_control = val & 0b1100_0111,

            /* Channel 4 */
            // NOTE: The top 2 bits are not used
            0xFF20 => self.ch4_length_timer = val & 0b0011_1111,
            0xFF21 => self.ch4_vol_and_env = val,
            0xFF22 => self.ch4_freq_and_rand = val,
            // NOTE: Only the top 2 bits are used
            0xFF23 => self.ch4_control = val & 0b1100_0000,

            /* Wave pattern registers */
            n @ 0xFF30..=0xFF3F => self.ch3_wave_form[(n - 0xFF30) as usize] = val,

            /* Controls */
            // NOTE: Only the topmost bit is writeable
            0xFF24 => self.master_control_vin_planning = val,
            0xFF25 => self.panning = val,
            0xFF26 => self.master_control = val & 0b1000_0000,
            /* Oops... */
            idx => {} // unreachable!("There was an attemped read from an unused bit @ 0x{idx:0>4x}"),
        }
    }

    /// Performs the necessary logic, taking into consideration the current set of the master
    /// control states, state of the channel (when triggered, how much progress has been made, etc),
    /// and outputs the next byte in the channel's wave form.
    pub fn read_channel_one(&self) -> u8 {
        todo!()
    }
}
