use crate::Register;
use sjef::instruction::Reg;
use std::ops::{Index, IndexMut};
use std::fmt::{self, Debug, Formatter};

const NUM_REGISTERS: usize = u8::MAX as usize;

/// The list of normal registers in a VM has in Sojourn.
/// 
/// This doesn't include special registers, such as the instruction / stack pointer.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Registers([Register; NUM_REGISTERS]);

impl Default for Registers {
	#[inline]
	fn default() -> Self {
		Self::new()
	}
}

impl Registers {
	/// Get an empty list of registers.
	#[inline]
	pub const fn new() -> Self {
		Self([Register::const_default(); NUM_REGISTERS])
	}
}

impl Index<Reg> for Registers {
	type Output = Register;

	/// Gets the `nth` register.
	///
	/// # Panics
	/// This function will never panic, as all `u8`s are valid register indexes.
	#[inline]
	fn index(&self, nth: Reg) -> &Self::Output {
		sa::const_assert_eq!(NUM_REGISTERS, u8::MAX as usize);

		// SAFETY: via the `const_assert`, we're guaranteed that any `u8` is a valid register.
		unsafe {
			&*self.0.as_ptr().offset(u8::from(nth) as isize)
		}
	}
}

impl IndexMut<Reg> for Registers {
	/// Gets the `nth` register.
	///
	/// # Panics
	/// This function will never panic, as all `u8`s are valid register indexes.
	fn index_mut(&mut self, nth: Reg) -> &mut Self::Output {
		sa::const_assert_eq!(NUM_REGISTERS, u8::MAX as usize);

		// SAFETY: via the `const_assert`, we're guaranteed that any `u8` is a valid register.
		unsafe {
			&mut *self.0.as_mut_ptr().offset(u8::from(nth) as isize)
		}
	}
}

impl Debug for Registers {
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		write!(f, "[{}", self.0[0])?;

		for register in &self.0[1..] {
			write!(f, ", {}", register)?;
		}

		write!(f, "]")
	}
}
