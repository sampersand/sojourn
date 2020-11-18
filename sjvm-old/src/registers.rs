
use std::ops::{Index, IndexMut};
use std::fmt::{self, Debug, Formatter};
use crate::Word;

const NUM_REGISTERS: usize = 16 ; //u8::MAX as usize;

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Registers([Word; NUM_REGISTERS]);


impl Debug for Registers {
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		// we don't want `f.alternate` to be used, so ignore them all.
		write!(f, "[{}", self.0[0])?;
		for word in &self.0[1..] {
			write!(f, ", {}", word)?;
		}
		write!(f, "]")
	}
}
impl Default for Registers {
	fn default() -> Self {
		Self([0; NUM_REGISTERS])
	}
}

impl Index<u8> for Registers {
	type Output = Word;
	fn index(&self, idx: u8) -> &Self::Output {
		&self.0[idx as usize]
	}
}

impl IndexMut<u8> for Registers {
	fn index_mut(&mut self, idx: u8) -> &mut Self::Output {
		&mut self.0[idx as usize]
	}
}
