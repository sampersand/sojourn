use std::io::{self, Read, Write};
use read_from::{ReadFrom, WriteTo, LittleEndian};
use crate::utils::read_vec;
use std::ops::Index;
use std::fmt::{self, Display, Formatter};

/// The data section within Sojourn.
#[derive(Debug, Default, Clone, PartialEq, Eq, Hash)]
pub struct Data {
	fields: Vec<Vec<u8>>
}

impl Data {
	/// Create an empty data section.
	pub const fn new() -> Self {
		Self { fields: Vec::new() } 
	}

	/// Creates a new data section with the given starting capacity.
	pub fn with_capacity(cap: usize) -> Self {
		Self { fields: Vec::with_capacity(cap) }
	}

	/// Adds a field and gets its index.
	#[must_use="the result is the index of the given item"]
	pub fn insert(&mut self, field: impl Into<Vec<u8>>) -> usize {
		// TODO: duplicate fields.
		self.fields.push(field.into());
		self.fields.len() - 1
	}

	/// Gets a field from the data section, returning `None` if ti doesn't exist
	pub fn get(&self, id: usize) -> Option<&[u8]> {
		self.fields.get(id).map(|x| &**x)
	}

	/// Attempts to get the index of an element, if it exists in `self`.
	pub fn index_of(&self, ele: &[u8]) -> Option<usize> {
		for (idx, field) in self.fields.iter().enumerate() {
			if ele == field.as_slice() {
				return Some(idx)
			}
		}
		None
	}

	/// Extends `self` by the given data.
	pub fn extend(&mut self, data: impl IntoIterator<Item=Vec<u8>>) {
		self.fields.extend(data)
	}
}

impl Index<usize> for Data {
	type Output = [u8];

	/// The same as [`Data::get`], except it `panic`s if the element at the index doesn't exist.
	fn index(&self, amnt: usize) -> &Self::Output {
		self.get(amnt).expect("indexed when the id doesn't exist!")
	}
}

/// Errors that can occur whilst reading [`Data`].
pub type ReadError = io::Error;

impl ReadFrom for Data {
	type Error = ReadError;

	fn read_from<R: Read>(mut inp: R) -> Result<Self, Self::Error> {
		let datalen =
			match LittleEndian::<u32>::read_from(&mut inp) {
				Ok(len) => len.0 as usize,
				Err(err) => {
					debug!(?err, "unable to data length");
					return Err(err);
				}
			};

		let mut data = Self::with_capacity(datalen);

		for _ in 0..datalen {
			let len =
				u8::read_from(&mut inp)
					.and_then(|x|
						if x == 0xff {
							LittleEndian::<u32>::read_from(&mut inp).map(|x| x.0 as usize)
						} else {
							Ok(x as usize)
						}
					).map_err(|err| {
						debug!(?err, "unable to read string length.");
						err
					})?;
			match read_vec(len, &mut inp) {
				Ok(field) => {
					trace!(?field, "successfully read field.");
					let _ = data.insert(field);
				},
				Err(err) => {
					debug!(?len, ?err, "unable to read string contents.");
					return Err(err);
				}
			}
		}

		Ok(data)
	}
}

impl WriteTo for Data {
	type Error = io::Error;

	fn write_to<W: Write>(&self, mut out: W) -> Result<usize, Self::Error> {
		let mut amnt = LittleEndian(self.fields.len() as u32).write_to(&mut out)?;

		for field in &self.fields {
			amnt += LittleEndian(field.len() as u32).write_to(&mut out)?;
			amnt += field.len();
			out.write_all(&field)?;
		}

		Ok(amnt)
	}
}

impl Display for Data {
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		let indent = (self.fields.len() as f64).log10() as usize + 1;

		for (idx, field) in self.fields.iter().enumerate() {
			writeln!(f, "{0:3$}. {1:?} (={2:?})", idx, String::from_utf8_lossy(field), &field, indent)?
		}

		Ok(())
	}
}
