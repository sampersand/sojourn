#![allow(unused)]
use crate::{Word, MaybeUninit};
use std::fmt::{self, Display, Binary, Octal, Pointer, UpperHex, LowerHex, Formatter, Alignment};
use std::ops::{
	AddAssign, SubAssign, MulAssign, DivAssign, RemAssign,
	BitAndAssign, BitOrAssign, BitXorAssign, ShlAssign, ShrAssign
};

/// A register within [Sojourn's VM](crate::SojournVm).
///
/// Only the `Assign` ops are supported, as it doesn't make much sense to create a register out of thin air.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct Register(Inner);

#[cfg(feature = "regcheck")]
type Inner = crate::MaybeUninit<Word>;

#[cfg(not(feature = "regcheck"))]
type Inner = Word;

impl Default for Register {
	#[inline]
	fn default() -> Self {
		Self::const_default()
	}
}

impl Register {
	/// Creates a a default register, but simply in a const context.
	///
	/// # Example
	/// ```rust
	/// use sjvm::Register;
	///
	/// assert_eq!(Register::const_default(), Register::default());
	/// ```
	#[inline]
	pub const fn const_default() -> Self {
		#[cfg(feature="regcheck")]
		{ Self(MaybeUninit::const_uninit()) }

		#[cfg(not(feature="regcheck"))]
		{ Self() }
	}

	/// Creates a new register, initialized with the given [`Word`].
	/// ```rust
	/// use sjvm::Register;
	///
	/// assert_eq!(Register::new(1234).load(), 1234);
	/// ```
	#[inline]
	pub const fn new(word: Word) -> Self {
		Self(MaybeUninit::new(word))
	}

	/// Gets the [`Word`] contained within this register.
	///
	/// It is undefined sojourn behaviour to load an uninitialized [`Register`] (ie a default one). In debug builds,
	/// this function will panic. In release builds, uninitialized registers have undefined contents.
	///
	/// ```rust
	/// use sjvm::Register;
	///
	/// assert_eq!(Register::new(1234).load(), 1234);
	/// ```
	#[inline(always)]
	#[must_use = "getting the value does nothing on its own."]
	pub fn load(&self) -> Word {
		*self.0.get()
	}

	/// Sets the [`Word`] contained within this register.
	///
	/// ```rust
	/// use sjvm::Register;
	///
	/// let mut reg = Register::default();
	/// reg.store(1234);
	///
	/// assert_eq!(reg.load(), 1234);
	/// ```
	#[inline]
	pub fn store(&mut self, word: Word) {
		self.0.set(word);
	}

	/// Checks to see if the register is zero.
	/// ```rust
	/// use sjvm::Register;
	///
	/// assert!(Register::new(0).is_zero());
	/// assert!(!Register::new(-1).is_zero());
	/// ```
	#[inline]
	pub fn is_zero(&self) -> bool {
		self.load() == 0
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
	/// let rhs = Register::new(12);
	/// 
	/// reg += rhs;
	///
	/// assert_eq!(reg.load(), 32);
	/// ```
	#[inline]
	fn add_assign(&mut self, rhs: Self) {
		self.store(self.load() + rhs.load());
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
	/// let rhs = Register::new(12);
	/// 
	/// reg -= rhs;
	///
	/// assert_eq!(reg.load(), 8);
	/// ```
	#[inline]
	fn sub_assign(&mut self, rhs: Self) {
		self.store(self.load() - rhs.load());
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
	/// let rhs = Register::new(12);
	/// 
	/// reg *= rhs;
	///
	/// assert_eq!(reg.load(), 240);
	/// ```
	#[inline]
	fn mul_assign(&mut self, rhs: Self) {
		self.store(self.load() * rhs.load());
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
	/// let rhs = Register::new(12);
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
	/// let rhs = Register::new(0);
	/// 
	/// reg /= rhs; // this fails!
	/// ```
	#[inline]
	fn div_assign(&mut self, rhs: Self) {
		assert!(!rhs.is_zero(), "division by zero!");

		self.store(self.load() / rhs.load());
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
	/// let rhs = Register::new(13);
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
	/// let rhs = Register::new(0);
	/// 
	/// reg %= rhs; // this fails!
	/// ```
	#[inline]
	fn rem_assign(&mut self, rhs: Self) {
		assert!(!rhs.is_zero(), "division by zero!");

		self.store(self.load() % rhs.load());
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
	/// let rhs = Register::new(13);
	/// 
	/// reg &= rhs;
	///
	/// assert_eq!(reg.load(), 4);
	/// ```
	#[inline]
	fn bitand_assign(&mut self, rhs: Self) {
		self.store(self.load() & rhs.load());
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
	/// let rhs = Register::new(13);
	/// 
	/// reg |= rhs;
	///
	/// assert_eq!(reg.load(), 29);
	/// ```
	#[inline]
	fn bitor_assign(&mut self, rhs: Self) {
		self.store(self.load() | rhs.load());
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
	/// let rhs = Register::new(13);
	/// 
	/// reg ^= rhs;
	///
	/// assert_eq!(reg.load(), 25);
	/// ```
	#[inline]
	fn bitxor_assign(&mut self, rhs: Self) {
		self.store(self.load() ^ rhs.load());
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
	/// let rhs = Register::new(13);
	/// 
	/// reg <<= rhs;
	///
	/// assert_eq!(reg.load(), 163840);
	/// ```
	#[inline]
	fn shl_assign(&mut self, rhs: Self) {
		self.store(self.load() << rhs.load());
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
	/// let rhs = Register::new(2);
	/// 
	/// reg >>= rhs;
	///
	/// assert_eq!(reg.load(), 12);
	/// ```
	#[inline]
	fn shr_assign(&mut self, rhs: Self) {
		self.store(self.load() >> rhs.load());
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
		self.store(-self.load());
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
		self.store(!self.load());
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
		self.store(self.is_zero() as Word);
	}
}

impl Display for Register {
	#[inline]
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		if !self.0.is_init() { return write!(f, "<??>"); }
		Display::fmt(&self.load(), f)
	}
}

impl Pointer for Register {
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		if !self.0.is_init() { return write!(f, "<??>"); }

		if f.alternate()  {
			Pointer::fmt(&(self.load() as *const ()), f)
		} else {
			f.pad(&format!("0x{:x}", self))
		}
	}
}


impl LowerHex for Register {
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		if !self.0.is_init() { return write!(f, "<??>"); }

		if f.alternate() {
			LowerHex::fmt(&self.load(), f)
		} else {
			f.pad(&format!("{:016x}", self.load()))
		}
	}
}

impl UpperHex for Register {
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		if !self.0.is_init() { return write!(f, "<??>"); }

		if f.alternate() {
			UpperHex::fmt(&self.load(), f)
		} else {
			f.pad(&format!("{:016X}", self.load()))
		}
	}
}

impl Octal for Register {
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		if !self.0.is_init() { return write!(f, "<??>"); }

		if f.alternate() {
			Octal::fmt(&self.load(), f)
		} else {
			f.pad(&format!("{:022o}", self.load()))
		}
	}
}

impl Binary for Register {
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		if !self.0.is_init() { return write!(f, "<??>"); }

		if f.alternate() {
			Binary::fmt(&self.load(), f)
		} else {
			f.pad(&format!("{:064b}", self.load()))
		}
	}
}
