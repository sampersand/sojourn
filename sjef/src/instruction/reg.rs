use std::io::{self, Read, Write};
use read_from::{ReadFrom, WriteTo};
use std::fmt::{self, Display, LowerHex, UpperHex, Binary, Formatter};

/// A register, used when emitting bytecode.
#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Reg(u8);

impl Reg {
	/// The register that will hold return values.
	pub const RETURN_REG: Self = Self::new(0);

	/// Creates a new register for the given index.
	#[inline]
	pub const fn new(idx: u8) -> Self {
		Self(idx)
	}
}

impl From<Reg> for u8 {
	#[inline]
	fn from(reg: Reg) -> Self {
		reg.0
	}
}

impl ReadFrom for Reg {
	type Error = io::Error;

	#[instrument(level="trace", skip(inp))]
	fn read_from<R: Read>(inp: R) -> Result<Self, Self::Error> {
		match u8::read_from(inp) {
			Ok(byte) => {
				let this = Self::new(byte);
				trace!(self = ?this, "parsed register");
				Ok(this)
			},
			Err(err) => {
				debug!("unable to parse register: {:?}", err);
				Err(err)
			}
		}
	}
}

impl WriteTo for Reg {
	type Error = io::Error;

	#[instrument(level="trace", skip(out))]
	fn write_to<W: Write>(&self, out: W) -> Result<usize, Self::Error> {
		self.0.write_to(out)
	}
}

impl Display for Reg {
	#[inline]
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		write!(f, "r{}", self.0)
	}
}

impl UpperHex for Reg {
	#[inline]
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		write!(f, "{:02X}", self.0)
	}
}

impl LowerHex for Reg {
	#[inline]
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		write!(f, "{:02x}", self.0)
	}
}

impl Binary for Reg {
	#[inline]
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		write!(f, "{:08b}", self.0)
	}
}
