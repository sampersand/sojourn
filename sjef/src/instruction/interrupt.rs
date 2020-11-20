use crate::instruction::Reg;
use std::io::{self, Read, Write};
use read_from::{ReadFrom, WriteTo, LittleEndian};
use std::fmt::{self, Display, LowerHex, UpperHex, Binary, Formatter};

/// The type of interrupt to perform.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub enum Interrupt {
	/// Exit with the given code.
	Exit(i32),

	/// Allocates at least the given amount of memory, storing the pointed into `reg`.
	Malloc(Reg, u32),

	/// Frees the memory pointed to by `reg`.
	Free(Reg),

	/// Rellocates the memory pointed to by `reg` to at least the new amount.
	Realloc(Reg, u32)
}

type TagType = u8;

impl Interrupt {
	const EXIT_TAG: TagType = 0;
	const MALLOC_TAG: TagType = 1;
	const FREE_TAG: TagType = 2;
	const REALLOC_TAG: TagType = 3;

	fn tag(&self) -> TagType {
		match self {
			Self::Exit(_) => Self::EXIT_TAG,
			Self::Malloc(_, _) => Self::MALLOC_TAG,
			Self::Free(_) => Self::FREE_TAG,
			Self::Realloc(_, _) => Self::REALLOC_TAG,
		}
	}

	/// Gets the length of this interrupt, in bytes.
	pub fn byte_len(&self) -> usize {
		use std::mem::{size_of, size_of_val};

		size_of::<TagType>() +
			match self {
				Self::Exit(code) => size_of_val(&code),
				Self::Free(reg) => size_of_val(&reg),
				Self::Malloc(reg, size)
					| Self::Realloc(reg, size) => size_of_val(&reg) + size_of_val(&size),
			}
	}
}

/// What problems can occur whilst [reading](Interrupt::read_from) an interrupt.
#[derive(Debug)]
pub enum ReadError {
	/// An unknown interrupt type was given.
	UnknownInterrupt(u8),

	/// A problem with i/o occurred.
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

	fn read_from<R: Read>(mut inp: R) -> Result<Self, Self::Error> {
		let interrupt = 
			match TagType::read_from(&mut inp) {
				Ok(Self::EXIT_TAG) =>
					match LittleEndian::read_from(&mut inp) {
						Ok(code) => Self::Exit(code.0),
						Err(err) => {
							warn!(?err, "unable to read exit code");
							return Err(ReadError::Io(err));
						}
					},
				Ok(Self::MALLOC_TAG) =>
					match Reg::read_from(&mut inp) {
						Ok(reg) => 
							match LittleEndian::read_from(&mut inp) {
								Ok(size) => Self::Malloc(reg, size.0),
								Err(err) => {
									warn!(?err, "unable to read malloc size");
									return Err(ReadError::Io(err));
								}
							},
						Err(err) => {
							warn!(?err, "unable to read malloc register");
							return Err(ReadError::Io(err));
						}
					},
				Ok(Self::FREE_TAG) =>
					match Reg::read_from(&mut inp) {
						Ok(reg) => Self::Free(reg),
						Err(err) => {
							warn!(?err, "unable to read free register");
							return Err(ReadError::Io(err));
						}
					},
				Ok(Self::REALLOC_TAG) =>
					match Reg::read_from(&mut inp) {
						Ok(reg) => 
							match LittleEndian::read_from(&mut inp) {
								Ok(size) => Self::Realloc(reg, size.0),
								Err(err) => {
									warn!(?err, "unable to read realloc size");
									return Err(ReadError::Io(err));
								}
							},
						Err(err) => {
							warn!(?err, "unable to read realloc register");
							return Err(ReadError::Io(err));
						}
					},
				Ok(byte) => {
					warn!(?byte, "unknown interrupt tag");
					return Err(ReadError::UnknownInterrupt(byte));
				},
				Err(err) => {
					warn!(?err, "unable to read interrupt");
					return Err(ReadError::Io(err));
				}
			};

		trace!(self = ?interrupt, "read interrupt");
		Ok(interrupt)
	}
}

impl WriteTo for Interrupt {
	type Error = io::Error;

	fn write_to<W: Write>(&self, mut out: W) -> Result<usize, Self::Error> {
		Ok(self.tag().write_to(&mut out)? +
			match *self {
				Self::Exit(code) => LittleEndian(code).write_to(&mut out)?,
				Self::Free(reg) => reg.write_to(&mut out)?,
				Self::Malloc(reg, size)
					| Self::Realloc(reg, size) => reg.write_to(&mut out)? + LittleEndian(size).write_to(&mut out)?,
			})
	}
}

impl Display for Interrupt {
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		match self {
			Self::Exit(code) => write!(f, "exit ${}", code),
			Self::Malloc(reg, size) => write!(f, "malloc {}, ${}", reg, size),
			Self::Free(reg) => write!(f, "free {}", reg),
			Self::Realloc(reg, size) => write!(f, "realloc {}, ${}", reg, size),
		}
	}
}

impl UpperHex for Interrupt {
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		write!(f, "{:02X} ", self.tag())?;

		match *self {
			Self::Exit(code) =>
				write!(f, "{:02X} {:02X} {:02X} {:02X}",
					((code as u32) & 0xff_00_00_00) >> 0o30,
					((code as u32) & 0x00_ff_00_00) >> 0o20,
					((code as u32) & 0x00_00_ff_00) >> 0o10,
					((code as u32) & 0x00_00_00_ff) >> 0o00),
			Self::Free(reg) => UpperHex::fmt(&reg, f),
			Self::Malloc(reg, size)
				| Self::Realloc(reg, size) =>
			{
				UpperHex::fmt(&reg, f)?;
				write!(f, " {:02X} {:02X} {:02X} {:02X}",
					(size & 0xff_00_00_00) >> 0o30,
					(size & 0x00_ff_00_00) >> 0o20,
					(size & 0x00_00_ff_00) >> 0o10,
					(size & 0x00_00_00_ff) >> 0o00)
			},
		}
	}
}

impl LowerHex for Interrupt {
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		write!(f, "{:02x} ", self.tag())?;

		match *self {
			Self::Exit(code) =>
				write!(f, "{:02x} {:02x} {:02x} {:02x}",
					((code as u32) & 0xff_00_00_00) >> 0o30,
					((code as u32) & 0x00_ff_00_00) >> 0o20,
					((code as u32) & 0x00_00_ff_00) >> 0o10,
					((code as u32) & 0x00_00_00_ff) >> 0o00),
			Self::Free(reg) => LowerHex::fmt(&reg, f),
			Self::Malloc(reg, size)
				| Self::Realloc(reg, size) =>
			{
				LowerHex::fmt(&reg, f)?;
				write!(f, " {:02x} {:02x} {:02x} {:02x}",
					(size & 0xff_00_00_00) >> 0o30,
					(size & 0x00_ff_00_00) >> 0o20,
					(size & 0x00_00_ff_00) >> 0o10,
					(size & 0x00_00_00_ff) >> 0o00)
			},
		}
	}
}

impl Binary for Interrupt {
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		write!(f, "{:08b} ", self.tag())?;

		match *self {
			Self::Exit(code) =>
				write!(f, "{:08b} {:08b} {:08b} {:08b}",
					((code as u32) & 0xff_00_00_00) >> 0o30,
					((code as u32) & 0x00_ff_00_00) >> 0o20,
					((code as u32) & 0x00_00_ff_00) >> 0o10,
					((code as u32) & 0x00_00_00_ff) >> 0o00),
			Self::Free(reg) => Binary::fmt(&reg, f),
			Self::Malloc(reg, size)
				| Self::Realloc(reg, size) =>
			{
				Binary::fmt(&reg, f)?;
				write!(f, " {:08b} {:08b} {:08b} {:08b}",
					(size & 0xff_00_00_00) >> 0o30,
					(size & 0x00_ff_00_00) >> 0o20,
					(size & 0x00_00_ff_00) >> 0o10,
					(size & 0x00_00_00_ff) >> 0o00)
			},
		}
	}
}
