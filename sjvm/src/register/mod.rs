use crate::Word;
use std::fmt::{Debug, Display, Binary, Octal, Pointer, UpperHex, LowerHex};
use std::ops::{
	AddAssign, SubAssign, MulAssign, DivAssign, RemAssign,
	BitAndAssign, BitOrAssign, BitXorAssign, ShlAssign, ShrAssign
};

/// A trait representing a register within [Sojourn's VM](crate::SojournVm).
///
/// Only the `Assign` ops are supported, as it doesn't make much sense to create a register out of thin air.
pub trait RegisterTrait : Sized +
	Default + Clone + Copy + PartialEq + Eq + PartialOrd + Ord + std::hash::Hash +
	Debug + Display + Binary + Octal + Pointer + UpperHex + LowerHex +
	AddAssign + SubAssign + MulAssign + DivAssign + RemAssign +
	BitAndAssign + BitOrAssign + BitXorAssign + ShlAssign + ShrAssign
{
	/// Creates a new register, initialized with the given [`Word`].
	#[must_use]
	fn new(word: Word) -> Self;

	/// Loads the [`Word`] stored in this register.
	#[must_use]
	fn load(&self) -> Word;

	/// Stores the given [`Word`] into this register.
	fn store(&mut self, word: Word);

	/// Negates the register, in place.
	fn neg_assign(&mut self);

	/// Inverts the register, btiwise, in place
	fn inv_assign(&mut self);

	/// Inverts the register, logically, in place.
	///
	/// Note that this means that `0` becomes `1`, and everything else becomes `0`.
	fn not_assign(&mut self);


	/// Finish using the register, marking it as uninitialized again.
	fn finished(&mut self);
}

/// Debug-mode registers
pub mod debug;

/// Registers which don't check the 
pub mod release;

cfg_if! {
	if #[cfg(any(feature="regcheck", debug_assertions))] {
		pub use debug::*;
	} else {
		pub use release::*;
	}
}
