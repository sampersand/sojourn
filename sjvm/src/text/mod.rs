// use std::io::{Read, Seek, SeekFrom};
// use crate::UWord;


pub mod pointer;
pub mod text;
pub use text::Text;

// /// The trait that represents the actual program code.
// pub trait TextTrait : Read + Seek + std::fmt::Debug {
// 	/// Gets the current "instruction pointer," ie the current stream pos.
// 	#[inline]
// 	fn ip(&self) -> UWord {
// 		self.seek(SeekFrom::Current(0)).expect("cant") as UWord
// 	}

// 	/// Gets the current "instruction pointer."
// 	#[inline]
// 	fn set_ip(&mut self, ip: UWord) {
// 		self.seek(SeekFrom::Start(ip as u64))
// 	}

// 	/// Checks to see if we're at EOF.
// 	#[inline]
// 	fn is_eof(&self) -> bool {
// 		self.ip() == self.seek(SeekFrom::End(0)) as UWord
// 	}
// }

// use crate::UWord;
// use std::io::{self, Read, Seek, SeekFrom, Cursor};
// use std::fmt::{self, Debug, Formatter};

// /// This struct houses the bytecode for SojournVm, and is used to keep track of execution flow.
// #[derive(Default, Clone, PartialEq, Eq)]
// pub struct Text {
// 	cursor: Cursor<Vec<u8>>
// }

// impl Text {
// 	/// Create a new [`ByeCode`] from the given iterator.
// 	pub fn new(bytecode: impl IntoIterator<Item=u8>) -> Self {
// 		Self { cursor: Cursor::new(bytecode.into_iter().collect()) }
// 	}

// 	/// Gets the current instruction pointer's position.
// 	#[inline]
// 	pub fn ip(&self) -> UWord {
// 		self.cursor.position()
// 	}

// 	/// Sets the instruction pointer's position.
// 	#[inline]
// 	pub fn set_ip(&mut self, new_ip: UWord) {
// 		self.cursor.set_position(new_ip);
// 	}

// 	/// Gets the bytecode.
// 	#[inline]
// 	pub fn bytes(&self) -> &[u8] {
// 		self.cursor.get_ref()
// 	}

// 	/// Checks to see if we're at eof.
// 	pub fn is_eof(&self) -> bool {
// 		self.ip() >= self.bytes().len() as UWord
// 	}
// }

// impl Debug for Text {
// 	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
// 		let bytes = self.bytes();

// 		if bytes.is_empty() {
// 			return write!(f, "[]");
// 		}

// 		write!(f, "[{:02x}", bytes[0])?;

// 		for byte in &bytes[1..] {
// 			write!(f, ", {:02x}", byte)?
// 		}

// 		write!(f, "]")
// 	}
// }

// impl Read for Text {
// 	#[inline]
// 	fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
// 		self.cursor.read(buf)
// 	}

// 	#[inline]
// 	fn read_exact(&mut self, buf: &mut [u8]) -> io::Result<()> {
// 		self.cursor.read_exact(buf)
// 	}
// }

// impl Seek for Text {
// 	#[inline]
// 	fn seek(&mut self, pos: SeekFrom) -> io::Result<u64> {
// 		self.cursor.seek(pos)
// 	}
// }
