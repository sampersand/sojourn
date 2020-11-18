#[macro_use]
extern crate tracing;
extern crate static_assertions as sa;

pub type Word = i64;
pub const WORD_SIZE: usize = std::mem::size_of::<Word>();

pub mod instruction;
pub use instruction::Instruction;
mod bytecode;
mod code;
mod registers;
// pub mod fmt;

pub use bytecode::Bytecode;
use code::Code;
use registers::Registers;
use std::convert::TryFrom;
use std::io::{self, Read};

bitflags::bitflags! {
	#[derive(Default)]
	struct Flags: u8 {
		const ZERO = 0b001;
		const POS  = 0b010;
		const NEG  = 0b100;
		const CMP  = 0b111;
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct SojournVm {
	code: Code,	
	flags: Flags,
	stack: Vec<Word>,
	data: Vec<Vec<u8>>,
	registers: Registers
}

impl SojournVm {
	pub fn read_from(mut inp: impl Read) -> io::Result<Self> {
		let mut buf = [0; 8];
		inp.read_exact(&mut buf)?;

		if buf != *b"sojourn1" {
			return Err(io::Error::new(io::ErrorKind::InvalidData, "unknown file type"));
		}

		inp.read_exact(&mut buf)?;
		let textlen = u64::from_le_bytes(buf);

		inp.read_exact(&mut buf)?;
		let datalen = u64::from_le_bytes(buf);

		let mut text = vec![0; textlen as usize];
		inp.read_exact(&mut text)?;

		let mut u32buf = [0; 4];
		inp.read_exact(&mut u32buf)?;
		let datums = u32::from_le_bytes(u32buf);
		let mut data = Vec::with_capacity(datums as usize);

		for _ in 0..datums {
			let mut chrbuf = [0; 1];
			inp.read_exact(&mut chrbuf)?;
			let datumlen = 
				if chrbuf == [255] {
					inp.read_exact(&mut u32buf)?;
					u32::from_le_bytes(u32buf) as usize
				} else {
					chrbuf[0] as usize
				};
			let mut datum = vec![0; datumlen];
			inp.read_exact(&mut datum)?;
			data.push(datum);
		}

		Ok(Self::new(text, data))
	}

	pub fn new(text: Vec<u8>, data: Vec<Vec<u8>>) -> Self {
		Self {
			code: Code::new(text),
			flags: Default::default(),
			stack: Default::default(),
			registers: Default::default(),
			data
		}
	}

	fn step(&mut self) {
		let instr_byte = self.code.next_byte();

		match Bytecode::try_from(instr_byte) {
			Ok(instr) => instr.run(self),
			Err(_) => panic!("Unknown instruction byte: 0x{:02x}\nvm: {:#?}", instr_byte, self)
		}
	}

	pub fn run(&mut self) {
		while !self.code.is_at_or_past_end() {
			self.step();
		}
	}
}
