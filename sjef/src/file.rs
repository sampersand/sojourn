use crate::{Text, Data, text, data};
use std::io::{self, Read, Write};
use read_from::{ReadFrom, WriteTo, LittleEndian};
use std::fmt::{self, Display, Formatter};

/// A Sojourn file.
#[derive(Debug, Clone, PartialEq, Eq)]
#[non_exhaustive]
pub struct File {
	pub version: [u8; 8],
	pub text: Text,
	pub data: Data
}

/// Problems that can occur whilst reading a file.
#[derive(Debug)]
pub enum FileReadError {
	/// An unknown version was given.
	UnknownVersion([u8; 8]),

	/// The text section couldn't be read.
	Text(text::ReadError),

	/// The data section couldn't be read.
	Data(data::ReadError),

	/// An i/o problem occurred outside of one of the sections.
	Io(io::Error)
}

impl Display for FileReadError {
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		match self {
			Self::UnknownVersion(version) => write!(f, "unknown sojourn bytecode version: {:?}", version),
			Self::Text(err) => Display::fmt(err, f),
			Self::Data(err) => Display::fmt(err, f),
			Self::Io(err) => Display::fmt(err, f),
		}
	}
}

impl std::error::Error for FileReadError {
	fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
		match self {
			Self::UnknownVersion(_) => None,
			Self::Text(err) => Some(err),
			Self::Data(err) => Some(err),
			Self::Io(err) => Some(err),
		}
	}
}

impl ReadFrom for File {
	type Error = FileReadError;

	fn read_from<R: Read>(mut inp: R) -> Result<Self, Self::Error> {
		const VERSION: [u8; 8] = *b"sojourn1";
		let version =
			match <[u8; 8]>::read_from(&mut inp) {
				Ok(VERSION) => {
					trace!(version = ?VERSION, "read file version");
					VERSION
				},
				Ok(version) => {
					debug!(?version, "unknown file version");
					return Err(FileReadError::UnknownVersion(version));
				},
				Err(err) => {
					debug!(?err, "unable to read file version");
					return Err(FileReadError::Io(err));
				}
			};

		let textlen =
			match LittleEndian::<u64>::read_from(&mut inp) {
				Ok(LittleEndian(len)) => {
					trace!(?len, "read 'text' length");
					len
				},
				Err(err) => {
					debug!(?err, "unable to read text length");
					return Err(FileReadError::Io(err));
				}
			};

		let datalen =
			match LittleEndian::<u64>::read_from(&mut inp) {
				Ok(LittleEndian(len)) => {
					trace!(?len, "read 'data' length");
					len
				},
				Err(err) => {
					debug!(?err, "unable to read data length");
					return Err(FileReadError::Io(err));
				}
			};

		let text = Text::read_from(inp.by_ref().take(textlen)).map_err(FileReadError::Text)?;
		let data = Data::read_from(inp.by_ref().take(datalen)).map_err(FileReadError::Data)?;


		Ok(Self { version, text, data })
	}
}

impl WriteTo for File {
	type Error = io::Error;

	fn write_to<W: Write>(&self, mut out: W) -> Result<usize, Self::Error> {
		use std::mem::{size_of_val, size_of};
		self.version.write_to(&mut out)?;

		let mut buf = Vec::new();
		let textlen = self.text.write_to(&mut buf)?;
		let datalen = self.data.write_to(&mut buf)?;
		LittleEndian(textlen as u64).write_to(&mut out)?;
		LittleEndian(datalen as u64).write_to(&mut out)?;

		out.write_all(&buf)?;
		Ok(size_of_val(&self.version) + size_of::<u64>() * 2 + datalen + textlen)
	}
}


impl Display for File {
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		write!(f, "version={}\n==[text]==\n{}\n==[data]==\n{}",
			String::from_utf8_lossy(&self.version),
			self.text, self.data)
	}
}
