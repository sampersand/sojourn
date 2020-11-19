use std::io::{self, Read, Seek, SeekFrom, Cursor};
use std::fmt::{self, Debug, Formatter};

/// This struct houses the bytecode for SojournVm, and is used to keep track of execution flow.
#[derive(Default, Clone, PartialEq, Eq)]
pub struct ByteCode {
	cursor: Cursor<Vec<u8>>
}

impl ByteCode {
	/// Create a new [`ByeCode`] from the given iterator.
	pub fn new(bytecode: impl IntoIterator<Item=u8>) -> Self {
		Self { cursor: Cursor::new(bytecode.into_iter().collect()) }
	}

	/// Gets the current instruction pointer's position.
	#[inline]
	pub fn ip(&self) -> u64 {
		self.cursor.position()
	}

	/// Sets the instruction pointer's position.
	#[inline]
	pub fn set_ip(&mut self, new_ip: u64) {
		self.cursor.set_position(new_ip);
	}

	/// Gets the bytecode.
	#[inline]
	pub fn bytes(&self) -> &[u8] {
		self.cursor.get_ref()
	}

	/// Checks to see if we're at eof.
	pub fn is_eof(&self) -> bool {
		self.ip() >= self.bytes().len() as u64
	}
}

impl Debug for ByteCode {
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		let bytes = self.bytes();

		if bytes.is_empty() {
			return write!(f, "[]");
		}

		write!(f, "[{:02x}", bytes[0])?;

		for byte in &bytes[1..] {
			write!(f, ", {:02x}", byte)?
		}

		write!(f, "]")
	}
}

impl Read for ByteCode {
	#[inline]
	fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
		self.cursor.read(buf)
	}

	#[inline]
	fn read_exact(&mut self, buf: &mut [u8]) -> io::Result<()> {
		self.cursor.read_exact(buf)
	}
}

impl Seek for ByteCode {
	#[inline]
	fn seek(&mut self, pos: SeekFrom) -> io::Result<u64> {
		self.cursor.seek(pos)
	}
}
