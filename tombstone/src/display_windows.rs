use std::fmt::Write;

use ratatui::text::Text;
use ratatui::widgets::Paragraph;
use ratatui::{Frame, layout::Rect, widgets::Block};

use spirit::cpu::Cpu;
use spirit::mem::{MemoryBankController, MemoryLike, MemoryMap, OamDma};

pub fn render_mem(mem_start: u16, frame: &mut Frame, area: Rect, mem: &MemoryMap) {
    let block = Block::bordered()
        .title(" Memory ")
        .title_alignment(ratatui::layout::Alignment::Center);

    // let mem_start = state.mem_start & 0xFFF0;
    let lines = area.height - 2;
    let mut buffer = vec![0; 16 * lines as usize];
    (mem_start..).zip(buffer.iter_mut()).for_each(|(i, byte)| {
        *byte = std::panic::catch_unwind(|| mem.read_byte(i)).unwrap_or_default()
    });

    render_byte_slice(frame, area, block, mem_start, &buffer);
}

pub fn render_ram(frame: &mut Frame, area: Rect, mbc: &MemoryBankController) {
    let block = Block::bordered()
        .title(" MBC RAM ")
        .title_alignment(ratatui::layout::Alignment::Center);

    let ram = match mbc {
        MemoryBankController::Direct { ram, .. } => ram.as_slice(),
        MemoryBankController::MBC1(mbc) => mbc.ram(),
        MemoryBankController::MBC2(_mbc) => todo!(),
        MemoryBankController::MBC3(_mbc) => todo!(),
        MemoryBankController::MBC5(_mbc) => todo!(),
    };

    render_byte_slice(frame, area, block, 0xA000, ram);
}

pub fn render_byte_slice(
    frame: &mut Frame,
    area: Rect,
    block: Block<'_>,
    mem_start: u16,
    bytes: &[u8],
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

pub fn render_cpu(frame: &mut Frame, area: Rect, cpu: &Cpu) {
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

pub fn render_oam_dma(frame: &mut Frame, area: Rect, dma: &OamDma) {
    let block = Block::bordered()
        .title(" OAM DMA ")
        .title_alignment(ratatui::layout::Alignment::Center);

    frame.render_widget(Paragraph::new(format!("{dma}")).block(block), area);
}

#[allow(dead_code)]
pub fn render_mbc(frame: &mut Frame, area: Rect, mbc: &MemoryBankController) {
    let block = Block::bordered()
        .title(" Timers ")
        .title_alignment(ratatui::layout::Alignment::Center);
    let para = match mbc {
        MemoryBankController::Direct { .. } => "Just data...".into(),
        MemoryBankController::MBC1(mbc) => format!("{mbc}"),
        MemoryBankController::MBC2(_mbc) => todo!(),
        MemoryBankController::MBC3(_mbc) => todo!(),
        MemoryBankController::MBC5(_mbc) => todo!(),
    };

    frame.render_widget(Paragraph::new(para).block(block), area);
}

pub fn render_interrupts(frame: &mut Frame, area: Rect, mem: &MemoryMap) {
    let block = Block::bordered()
        .title(" Interrupts ")
        .title_alignment(ratatui::layout::Alignment::Center);
    let para = Paragraph::new(Text::from_iter([
        format!("Enabled   : 0b{:0>8b}", mem.ie), // , mem.ie),
        format!("Requested : 0b{:0>8b}", mem.io().interrupt_flags), // , mem.io.interrupt_flags),
    ]))
    .block(block);
    frame.render_widget(para, area);
}

#[allow(dead_code)]
pub fn render_stack(frame: &mut Frame, area: Rect, mem: &MemoryMap) {
    let block = Block::bordered()
        .title(" Stack ")
        .title_alignment(ratatui::layout::Alignment::Center);
    // let iter = [].into_iter();
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
