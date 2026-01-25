use std::fmt::Write;

use ratatui::Frame;
use ratatui::layout::Alignment;
use ratatui::layout::Constraint;
use ratatui::layout::Direction;
use ratatui::layout::Layout;
use ratatui::layout::Rect;
use ratatui::text::Text;
use ratatui::widgets::Block;
use ratatui::widgets::Paragraph;

use rboy::device::Device;
use spirit::mem::MemoryBankController;
use spirit::mem::MemoryLike;

use crate::state::InnerAppState;

pub fn render_mem(state: &InnerAppState, frame: &mut Frame, area: Rect) {
    let block = Block::bordered()
        .title(" Memory ")
        .title_alignment(ratatui::layout::Alignment::Center);

    let lines = area.height - 2;
    let mut buffer = vec![0; 16 * lines as usize];
    (state.mem_start..)
        .zip(buffer.iter_mut())
        .for_each(|(i, byte)| *byte = state.gb.gb().mem.read_byte(i));

    render_byte_slice(state.mem_start, &buffer, frame, area, block);
}

pub fn render_ram(state: &InnerAppState, frame: &mut Frame, area: Rect) {
    let block = Block::bordered()
        .title(" MBC RAM ")
        .title_alignment(ratatui::layout::Alignment::Center);

    let ram = match &state.gb.gb().mem.mbc {
        MemoryBankController::Direct { ram, .. } => ram.0.as_slice(),
        MemoryBankController::MBC1(mbc) => mbc.ram(),
        MemoryBankController::MBC2(_mbc) => todo!(),
        MemoryBankController::MBC3(mbc) => mbc.rom[0].0.as_slice(),
        MemoryBankController::MBC5(_mbc) => todo!(),
    };

    render_byte_slice(0xA000, ram, frame, area, block);
}

pub fn render_byte_slice(
    mem_start: u16,
    bytes: &[u8],
    frame: &mut Frame,
    area: Rect,
    block: Block<'_>,
) {
    let lines = area.height - 2;
    let mut data = String::from("  ADDR | 00 01 02 03 04 05 06 07 08 09 0A 0B 0C 0D 0E 0F\n");
    data.push_str("-------|------------------------------------------------\n");
    for i in 0..lines {
        let start = i * 16;
        let end = (i + 1) * 16;
        _ = write!(data, "0x{:0>4X} |", mem_start + start);
        _ = (start..end).try_for_each(|i| write!(data, " {:0>2X}", bytes[i as usize]));
        data.push('\n');
    }
    let para = Paragraph::new(data).block(block);

    frame.render_widget(para, area);
}

pub fn render_cpu(state: &InnerAppState, frame: &mut Frame, area: Rect) {
    let cpu = state.gb.gb().cpu();
    let block = Block::bordered()
        .title(" CPU ")
        .title_alignment(ratatui::layout::Alignment::Center);
    let para = Paragraph::new(Text::from_iter([
        format!("A : 0x{:0>2X}", cpu.a),
        format!(
            "F : Z={} N={} H={} C={}",
            cpu.f.z as u8, cpu.f.n as u8, cpu.f.h as u8, cpu.f.c as u8,
        ),
        format!("BC: 0x{:0>2X} 0x{:0>2X}", cpu.b, cpu.c),
        format!("DE: 0x{:0>2X} 0x{:0>2X}", cpu.d, cpu.e),
        format!("HL: 0x{:0>2X} 0x{:0>2X}", cpu.h, cpu.l),
        format!("PC: 0x{:0>4X}", cpu.pc),
        format!("SP: 0x{:0>4X}", cpu.sp),
        format!("IME: {}", cpu.ime),
    ]))
    .block(block);
    frame.render_widget(para, area);
}

pub fn render_oam_dma(state: &InnerAppState, frame: &mut Frame, area: Rect) {
    let block = Block::bordered()
        .title(" OAM DMA ")
        .title_alignment(ratatui::layout::Alignment::Center);

    let dma = &state.gb.gb().mem.oam_dma;
    frame.render_widget(Paragraph::new(format!("{dma}")).block(block), area);
}

#[allow(dead_code)]
pub fn render_mbc(state: &InnerAppState, frame: &mut Frame, area: Rect) {
    let block = Block::bordered()
        .title(" Timers ")
        .title_alignment(ratatui::layout::Alignment::Center);
    let para = match &state.gb.gb().mem.mbc {
        MemoryBankController::Direct { .. } => "Just data...".into(),
        MemoryBankController::MBC1(mbc) => format!("{mbc}"),
        MemoryBankController::MBC2(_mbc) => todo!(),
        MemoryBankController::MBC3(_mbc) => todo!(),
        MemoryBankController::MBC5(_mbc) => todo!(),
    };

    frame.render_widget(Paragraph::new(para).block(block), area);
}

pub fn render_interrupts(state: &InnerAppState, frame: &mut Frame, area: Rect) {
    let block = Block::bordered()
        .title(" Interrupts ")
        .title_alignment(ratatui::layout::Alignment::Center);
    let para = Paragraph::new(Text::from_iter([
        format!("Enabled   : 0b{:0>8b}", state.gb.gb().mem.ie), // , mem.ie),
        format!(
            "Requested : 0b{:0>8b}",
            state.gb.gb().mem.io().interrupt_flags
        ), // , mem.io.interrupt_flags),
    ]))
    .block(block);
    frame.render_widget(para, area);
}

#[allow(dead_code)]
pub fn render_stack(state: &InnerAppState, frame: &mut Frame, area: Rect) {
    let block = Block::bordered()
        .title(" Stack ")
        .title_alignment(ratatui::layout::Alignment::Center);
    let mem = &state.gb.gb().mem;
    let text = format!(
        "0x{:0>2X}{:0>2X} 0x{:0>2X}{:0>2X}",
        mem.read_byte(0xFFFE),
        mem.read_byte(0xFFFD),
        mem.read_byte(0xFFFC),
        mem.read_byte(0xFFFB)
    );
    let para = Paragraph::new(Text::from(text)).block(block);
    frame.render_widget(para, area);
}

pub fn render_rboy(frame: &mut Frame, area: Rect, device: &Device) {
    let right = Layout::default()
        .direction(Direction::Vertical)
        .constraints([
            Constraint::Length(10),
            Constraint::Length(20),
            Constraint::Length(4),
            Constraint::Fill(1),
        ])
        .split(area);
    let block = Block::bordered()
        .title(" RBoy CPU ")
        .title_alignment(Alignment::Center);

    let cpu = device.cpu.reg;
    let para = Paragraph::new(Text::from_iter([
        format!("A : 0x{:0>2X}", cpu.a),
        format!("F : 0b{:0>4b}", cpu.f >> 4,),
        format!("BC: 0x{:0>2X} 0x{:0>2X}", cpu.b, cpu.c),
        format!("DE: 0x{:0>2X} 0x{:0>2X}", cpu.d, cpu.e),
        format!("HL: 0x{:0>2X} 0x{:0>2X}", cpu.h, cpu.l),
        format!("PC: 0x{:0>4X}", cpu.pc),
        format!("SP: 0x{:0>4X}", cpu.sp),
        format!("IME: {}", device.cpu.ime),
    ]));
    frame.render_widget(para.block(block), area);
}
