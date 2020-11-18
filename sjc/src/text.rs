use crate::Instruction;
use std::io::{self, Read, Write};
use read_from::{ReadFrom, WriteTo};
use std::fmt::{self, Display, Formatter};
pub use crate::instruction::ReadError;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Text {
	instrs: Vec<Instruction>
}

impl ReadFrom for Text {
	type Error = ReadError;

	#[instrument(level="trace", skip(inp))]
	fn read_from<R: Read>(mut inp: R) -> Result<Self, Self::Error> {
		let mut instrs = Vec::new();

		// TODO: maybe use a bufread or something here to ensure we now when we hit EOF.
		loop {
			match Instruction::read_from(&mut inp) {
				Ok(instr) => {
					trace!(?instr, "read instruction correctly");
					instrs.push(instr);
				},
				Err(ReadError::Io(err)) if err.kind() == io::ErrorKind::UnexpectedEof => {
					debug!(?err, "broke because error was eof");
					break
				},
				Err(other) => return Err(other)
			};
		}

		Ok(Self { instrs })
	}
}

impl WriteTo for Text {
	type Error = io::Error;

	#[instrument(level="trace", skip(out))]
	fn write_to<W: Write>(&self, mut out: W) -> Result<usize, Self::Error> {
		let mut amnt = 0;

		for instr in &self.instrs {
			amnt += instr.write_to(&mut out)?;
		}

		Ok(amnt)
	}
}

impl Display for Text {
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		let mut bytelen = 0usize;

		for instr in &self.instrs {
			let offset = instr.get_offset();

			write!(f, "{0:08x} : {1:<30x} {1:<2$}", bytelen, instr, if offset.is_some() { 30 } else { 0 })?;

			if let Some(offset) = offset {
				write!(f, "(= {:08x})", bytelen as isize + offset as isize)?;
			}

			writeln!(f)?;
			bytelen += instr.byte_len();
		}

		Ok(())
	}
}
