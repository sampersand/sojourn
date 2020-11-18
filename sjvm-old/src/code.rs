use crate::{Word, WORD_SIZE};
use std::fmt::{self, Debug, Formatter};

#[derive(Default, Clone, PartialEq, Eq, Hash)]
pub struct Code {
	bytecode: Vec<u8>,
	ip: usize
}

impl Debug for Code {
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		struct DebugBytecode<'a>(&'a [u8]);
		impl Debug for DebugBytecode<'_> {
			fn fmt(&self, f: &mut Formatter) -> fmt::Result {
				if self.0.is_empty() {
					return write!(f, "[]");
				}

				write!(f, "[{:02x}", self.0[0])?;
				for byte in &self.0[1..] {
					write!(f, ", {:02x}", byte)?;
				}
				write!(f, "]")
			}
		}

		f.debug_struct("Code")
			.field(&"ip", &self.ip)
			.field(&"bytecode", &DebugBytecode(&self.bytecode))
			.finish()
	}
}


impl Code {
	pub fn new(bytecode: Vec<u8>) -> Self {
		Self { bytecode, ip: 0 }
	}

	pub fn is_at_or_past_end(&self) -> bool {
		self.ip >= self.bytecode.len()
	}

	pub fn ip_as_word(&self) -> Word {
		self.ip as u64 as Word
	}

	pub fn set_ip_from_word(&mut self, ip: Word){
		self.ip = ip as u64 as usize;
	}

	pub fn offset_ip(&mut self, amount: isize) {
		if amount < 0 {
			self.ip -= amount.abs() as usize;
		} else {
			self.ip += amount.abs() as usize;
		}
	}

	pub fn next_byte(&mut self) -> u8 {
		let byte = self.bytecode[self.ip];
		self.ip += 1;
		byte
	}

	pub fn next_short(&mut self) -> u16 {
		let short = u16::from_le_bytes([self.bytecode[self.ip], self.bytecode[self.ip + 1]]);
		self.ip += std::mem::size_of::<u16>();
		short
	}

	pub fn next_sshort(&mut self) -> i16 {
		let short = i16::from_le_bytes([self.bytecode[self.ip], self.bytecode[self.ip + 1]]);
		self.ip += std::mem::size_of::<i16>();
		short
	}

	pub fn next_word(&mut self) -> Word {
		assert!(self.ip + WORD_SIZE < self.bytecode.len(),
			"tried to peek a word when not enough space remains!\ncode: {:#?}", self);
		// SAFETY: we just ensured that `self.bytecode` is large enough, so the pointer
		// should be valid.
		let word = 
			unsafe {
				let ptr = self.bytecode.as_ptr().offset(self.ip as isize) as *const [u8; WORD_SIZE];
				Word::from_le_bytes(*ptr)
			};

		self.ip += WORD_SIZE;
		word
	}
	pub fn next_sword(&mut self) -> i64 {
		assert!(self.ip + WORD_SIZE < self.bytecode.len(),
			"tried to peek a word when not enough space remains!\ncode: {:#?}", self);
		// SAFETY: we just ensured that `self.bytecode` is large enough, so the pointer
		// should be valid.
		let word = 
			unsafe {
				let ptr = self.bytecode.as_ptr().offset(self.ip as isize) as *const [u8; WORD_SIZE];
				i64::from_le_bytes(*ptr)
			};

		self.ip += WORD_SIZE;
		word
	}
}

// impl Index<u8> for Registers {
// 	type Output = Word;
// 	fn index(&self, idx: u8) -> &Self::Output {
// 		&self.0[idx as usize]
// 	}
// }

// impl IndexMut<u8> for Registers {
// 	fn index_mut(&mut self, idx: u8) -> &mut Self::Output {
// 		&mut self.0[idx as usize]
// 	}
// }
