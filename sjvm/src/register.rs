#![allow(unused)]
use crate::Word;
use std::fmt::{self, Display, Binary, Octal, Pointer, UpperHex, LowerHex, Formatter, Alignment};
use std::ops::{
	AddAssign, SubAssign, MulAssign, DivAssign, RemAssign,
	BitAndAssign, BitOrAssign, BitXorAssign, ShlAssign, ShrAssign
};

/// A register within [Sojourn's VM](crate::SojournVm).
///
/// Only the `Assign` ops are supported, as it doesn't make much sense to create a register out of thin air.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Register(Word);

impl Default for Register {
	#[inline]
	fn default() -> Self {
		Self::const_default()
	}
}

impl Register {
	/// Creates a a default register, in a const context.
	#[inline]
	pub const fn const_default() -> Self {
		Self::new(-1)
	}

	/// Creates a new register, initialized with the given [`Weord`].
	#[inline]
	pub const fn new(word: Word) -> Self {
		Self(word)
	}

	/// Gets the [`Word`] contained within this register.
	#[inline]
	#[must_use = "getting the value does nothing on its own."]
	pub const fn load(&self) -> Word {
		self.0
	}

	/// Sets the [`Word`] contained within this register.
	pub fn store(&mut self, new: Word) {
		self.swap(new);
	}

	/// Sets the [`Word`] contained within this register, returning the previous value.
	#[must_use="If you don't need the returned value, use 'store'."]
	pub fn swap(&mut self, mut new: Word) -> Word {
		std::mem::swap(&mut self.0, &mut new);
		new
	}

	/// Checks to see if the register is zero..
	#[inline]
	pub const fn is_zero(&self) -> bool {
		self.0 == 0
	}
}

impl AddAssign for Register {
	/// Adds `rhs` to `self`.
	///
	/// # Examples
	/// ```rust
	/// use sjvm::Register;
	///
	/// let mut reg = Register::new(20);
	/// let rhs = Regiser::new(12);
	/// 
	/// reg += rhs;
	///
	/// assert_eq!(reg.load(), 32);
	/// ```
	#[inline]
	fn add_assign(&mut self, rhs: Self) {
		self.0 += rhs.0
	}
}

impl SubAssign for Register {
	/// Subtracts `rhs` from `self`.
	///
	/// # Examples
	/// ```rust
	/// use sjvm::Register;
	///
	/// let mut reg = Register::new(20);
	/// let rhs = Regiser::new(12);
	/// 
	/// reg -= rhs;
	///
	/// assert_eq!(reg.load(), 8);
	/// ```
	#[inline]
	fn sub_assign(&mut self, rhs: Self) {
		self.0 -= rhs.0
	}
}

impl MulAssign for Register {
	/// Multiplies `self` by `rhs`.
	///
	/// # Examples
	/// ```rust
	/// use sjvm::Register;
	///
	/// let mut reg = Register::new(20);
	/// let rhs = Regiser::new(12);
	/// 
	/// reg *= rhs;
	///
	/// assert_eq!(reg.load(), 240);
	/// ```
	#[inline]
	fn mul_assign(&mut self, rhs: Self) {
		self.0 *= rhs.0;
	}
}

impl DivAssign for Register {
	/// Divides `self` by `rhs`, flooring the result.
	///
	/// # Panics
	/// Panics if `rhs` is zero.
	///
	/// # Examples
	/// ```rust
	/// use sjvm::Register;
	///
	/// let mut reg = Register::new(20);
	/// let rhs = Regiser::new(12);
	/// 
	/// reg /= rhs;
	///
	/// assert_eq!(reg.load(), 1);
	/// ```
	/// 
	/// Division by zero will panic:
	/// ```should_panic
	/// use sjvm::Register;
	///
	/// let mut reg = Register::new(20);
	/// let rhs = Regiser::new(0);
	/// 
	/// reg /= rhs; // this fails!
	/// ```
	#[inline]
	fn div_assign(&mut self, rhs: Self) {
		assert!(!rhs.is_zero(), "division by zero!");

		self.0 /= rhs.0;
	}
}

impl RemAssign for Register {
	/// Modulos `self` by `rhs`.
	///
	/// # Panics
	/// Panics if `rhs` is zero.
	///
	/// # Examples
	/// ```rust
	/// use sjvm::Register;
	///
	/// let mut reg = Register::new(20);
	/// let rhs = Regiser::new(13);
	/// 
	/// reg %= rhs;
	///
	/// assert_eq!(reg.load(), 7);
	/// ```
	/// 
	/// Division by zero will panic:
	/// ```should_panic
	/// use sjvm::Register;
	///
	/// let mut reg = Register::new(20);
	/// let rhs = Regiser::new(0);
	/// 
	/// reg %= rhs; // this fails!
	/// ```
	#[inline]
	fn rem_assign(&mut self, rhs: Self) {
		assert!(!rhs.is_zero(), "division by zero!");

		self.0 %= rhs.0;
	}
}

impl BitAndAssign for Register {
	/// Sets `self` to the bitwise AND of `self` and `rhs`.
	///
	/// # Examples
	/// ```rust
	/// use sjvm::Register;
	///
	/// let mut reg = Register::new(20);
	/// let rhs = Regiser::new(13);
	/// 
	/// reg &= rhs;
	///
	/// assert_eq!(reg.load(), 4);
	/// ```
	#[inline]
	fn bitand_assign(&mut self, rhs: Self) {
		self.0 &= rhs.0;
	}
}

impl BitOrAssign for Register {
	/// Sets `self` to the bitwise OR of `self` and `rhs`.
	///
	/// # Examples
	/// ```rust
	/// use sjvm::Register;
	///
	/// let mut reg = Register::new(20);
	/// let rhs = Regiser::new(13);
	/// 
	/// reg |= rhs;
	///
	/// assert_eq!(reg.load(), 29);
	/// ```
	#[inline]
	fn bitor_assign(&mut self, rhs: Self) {
		self.0 |= rhs.0;
	}
}

impl BitXorAssign for Register {
	/// Sets `self` to the bitwise XOR of `self` and `rhs`.
	///
	/// # Examples
	/// ```rust
	/// use sjvm::Register;
	///
	/// let mut reg = Register::new(20);
	/// let rhs = Regiser::new(13);
	/// 
	/// reg ^= rhs;
	///
	/// assert_eq!(reg.load(), 25);
	/// ```
	#[inline]
	fn bitxor_assign(&mut self, rhs: Self) {
		self.0 ^= rhs.0;
	}
}

impl ShlAssign for Register {
	/// Sets `self` to `self` shifted left by `rhs` bits.
	///
	/// # Examples
	/// ```rust
	/// use sjvm::Register;
	///
	/// let mut reg = Register::new(20);
	/// let rhs = Regiser::new(13);
	/// 
	/// reg <<= rhs;
	///
	/// assert_eq!(reg.load(), 163840);
	/// ```
	#[inline]
	fn shl_assign(&mut self, rhs: Self) {
		self.0 <<= rhs.0;
	}
}

impl ShrAssign for Register {
	/// Sets `self` to `self` shifted right by `rhs` bits, sign-extending.
	///
	/// # Examples
	/// ```rust
	/// use sjvm::Register;
	///
	/// let mut reg = Register::new(49);
	/// let rhs = Regiser::new(2);
	/// 
	/// reg >>= rhs;
	///
	/// assert_eq!(reg.load(), 12);
	/// ```
	#[inline]
	fn shr_assign(&mut self, rhs: Self) {
		self.0 >>= rhs.0;
	}
}

impl Register {
	/// Negates the register, in place.
	///
	/// # Examples
	/// ```rust
	/// use sjvm::Register;
	///
	/// let mut reg = Register::new(49);
	/// reg.neg_assign();
	/// assert_eq!(reg.load(), -49);
	/// ```
	#[inline]
	pub fn neg_assign(&mut self) {
		self.0 = -self.0;
	}

	/// Sets `self` to the bitwise inverse of itself.
	///
	/// # Examples
	/// ```rust
	/// use sjvm::Register;
	///
	/// let mut reg = Register::new(49);
	/// reg.inv_assign();
	/// assert_eq!(reg.load(), -50);
	/// ```
	#[inline]
	pub fn inv_assign(&mut self) {
		self.0 = -self.0;
	}

	/// Sets `self` to the logical negation of itself: `0` becomes `1`, and everything else becomes `0`.
	///
	/// # Examples
	/// ```rust
	/// use sjvm::Register;
	///
	/// let mut reg = Register::new(49);
	/// reg.not_assign();
	/// assert_eq!(reg.load(), 0);
	/// ```
	#[inline]
	pub fn not_assign(&mut self) {
		self.0 = !self.is_zero() as i64;
	}
}

impl Display for Register {
	#[inline]
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		Display::fmt(&self.0, f)
	}
}

impl Pointer for Register {
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		if f.alternate()  {
			Pointer::fmt(&(self.0 as *const ()), f)
		} else {
			f.pad(&format!("0x{:x}", self))
		}
	}
}


impl LowerHex for Register {
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		if f.alternate() {
			LowerHex::fmt(&self.0, f)
		} else {
			f.pad(&format!("{:016x}", self.0))
		}
	}
}

impl UpperHex for Register {
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		if f.alternate() {
			UpperHex::fmt(&self.0, f)
		} else {
			f.pad(&format!("{:016X}", self.0))
		}
	}
}

impl Octal for Register {
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		if f.alternate() {
			Octal::fmt(&self.0, f)
		} else {
			f.pad(&format!("{:022o}", self.0))
		}
	}
}

impl Binary for Register {
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		if f.alternate() {
			Binary::fmt(&self.0, f)
		} else {
			f.pad(&format!("{:064b}", self.0))
		}
	}
}
