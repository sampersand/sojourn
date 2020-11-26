use crate::Word;
use super::RegisterTrait;

/// A debug-mode register within [Sojourn's VM](crate::SojournVm).
///
/// # Panics
/// For every function other than [`Register::store`] and [`Register::try_load`], if the 
/// contained value is uninitialized, the register will panic.
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Register(Option<Word>);

impl RegisterTrait for Register {
	fn new(word: Word) -> Self {
		Self(Some(word))
	}

	fn load(&self) -> Word {
		self.0.expect("attempted to load from an uninitialized register!")
	}

	fn store(&mut self, word: Word) {
		self.0 = Some(word);
	}

	fn neg_assign(&mut self) {
		self.store(-self.load());
	}

	fn inv_assign(&mut self) {
		self.store(!self.load());
	}

	fn not_assign(&mut self) {
		self.store(self.is_zero() as Word);
	}

	/// Mark the register as uninitialized again.
	///
	/// # Panics
	/// This will panic if used on an uninitialized register.
	fn finished(&mut self) {
		self.0.take().expect("finished an uniniitalized register!");
	}
}

impl Register {
	/// Whether or not this register's zero.
	pub fn is_zero(&self) -> bool {
		self.load() == 0
	}

	/// Try to load the value from this register, returning `None` if we're uninitialized
	pub fn try_load(&self) -> Option<Word> {
		self.0
	}
}

macro_rules! impl_traits {
	($($trait:ident $doc:literal $fn:ident $op:tt $($rhs:ident)?;)*) => {
		$(
			impl std::ops::$trait for Register {
				#[doc=$doc]
				fn $fn(&mut self, rhs: Self) {
					$(assert!(!rhs.is_zero(), concat!(stringify!($rhs), " is zero!"));)?
					self.store(self.load() $op rhs.load());
				}
			}
		)*
	};
}

impl_traits! {
	AddAssign "Adds `rhs` to `self`." add_assign +;
	SubAssign "Subtracts `rhs` from `self`." sub_assign -;
	MulAssign "Multiplies `self` by `rhs`." mul_assign *;
	DivAssign "Divides `self` by `rhs`.\n\n# Panics\nPanics if `rhs` is zero" div_assign / rhs;
	RemAssign "Modulos `self` by `rhs`.\n\n# Panics\nPanics if `rhs` is zero" rem_assign % rhs;
	BitAndAssign "Sets `self` to the bitwise AND of `self` and `rhs`." bitand_assign &;
	BitOrAssign "Sets `self` to the bitwise AND OR `self` and `rhs`." bitor_assign |;
	BitXorAssign "Sets `self` to the bitwise XOR OR `self` and `rhs`." bitxor_assign ^;
	ShlAssign "Sets `self` to `self` shifted left by `rhs` bits." shl_assign <<;
	ShrAssign "Sets `self` to `self` shifted right by `rhs` bits, sign-extending." shr_assign >>;
}

macro_rules! impl_formats {
	($($trait:ident $([$($cast:tt)*])?)*) => {
		$(
			impl std::fmt::$trait for Register {
				fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
					if let Some(word) = self.try_load() {
						std::fmt::$trait::fmt(&(word $($($cast)*)?), f)
					} else {
						write!(f, "<??>")
					}
				}
			}
		)*
	};
}

impl_formats!(Display Pointer [as *const ()] UpperHex LowerHex Octal Binary);
