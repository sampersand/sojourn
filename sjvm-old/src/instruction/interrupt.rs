use std::io::{self, Read, Write};
use read_from::{ReadFrom, WriteTo, LittleEndian};
use std::fmt::{self, Display, LowerHex, UpperHex, Binary, Formatter};

/// The type of interrupt to perform.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Interrupt {
	/// Exit with the given code.
	Exit(i32)
}

impl Interrupt {
	const EXIT_TAG: u8 = 0;

	pub fn byte_len(&self) -> usize {
		use std::mem::size_of;

		size_of::<u8>() +
			match self {
				Self::Exit(_) => size_of::<i32>()
			}
	}
}

/// What problems can occur whilst [reading](Interrupt::read_from) an interrupt.
#[derive(Debug)]
pub enum ReadError {
	UnknownInterrupt(u8),
	Io(io::Error)
}

impl Display for ReadError {
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		match self {
			Self::UnknownInterrupt(byte) => write!(f, "unknown interrupt: {:02x}", byte),
			Self::Io(err) => Display::fmt(err, f)
		}
	}
}

impl std::error::Error for ReadError {
	fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
		match self {
			Self::UnknownInterrupt(_) => None,
			Self::Io(err) => Some(err)
		}
	}
}

impl ReadFrom for Interrupt {
	type Error = ReadError;

	#[instrument(level="trace", skip(inp))]
	fn read_from<R: Read>(mut inp: R) -> Result<Self, Self::Error> {
		match u8::read_from(&mut inp) {
			Ok(Self::EXIT_TAG) => match LittleEndian::read_from(&mut inp) {
				Ok(code) => {
					let this = Self::Exit(code.0);
					trace!(self = ?this, "parsed interrupt");
					Ok(this)
				},
				Err(err) => {
					debug!(?err, "unable to read exit code");
					Err(ReadError::Io(err))
				}
			},
			Ok(byte) => {
				debug!(?byte, "unknown interrupt tag");
				Err(ReadError::UnknownInterrupt(byte))
			},
			Err(err) => {
				debug!(?err, "unable to read interrupt");
				Err(ReadError::Io(err))
			}
		}
	}
}

impl WriteTo for Interrupt {
	type Error = io::Error;

	#[instrument(level="trace", skip(out))]
	fn write_to<W: Write>(&self, mut out: W) -> Result<usize, Self::Error> {
		match *self {
			Self::Exit(num) => {
				let amnt = Self::EXIT_TAG.write_to(&mut out)? + LittleEndian(num).write_to(&mut out)?;
				trace!(?amnt, ?self, "wrote interrupt");
				Ok(amnt)
			}
		}
	}
}

impl Display for Interrupt {
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		match self {
			Self::Exit(code) => write!(f, "exit, {}", code)
		}
	}
}

impl UpperHex for Interrupt {
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		match self {
			// TODO: exit code formatted nicely
			
			Self::Exit(code) => write!(f, "{:02X} {:08X}", Self::EXIT_TAG, code)
		}
	}
}

impl LowerHex for Interrupt {
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		match self {
			// TODO: exit code formatted nicely
			
			Self::Exit(code) => write!(f, "{:02x} {:08x}", Self::EXIT_TAG, code)
		}
	}
}

impl Binary for Interrupt {
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		match self {
			Self::Exit(code) => write!(f, "{:08b} {:064b}", Self::EXIT_TAG, code)
		}
	}
}
