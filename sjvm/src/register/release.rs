use crate::Word;
use super::RegisterTrait;

/// A release-mode register within [Sojourn's VM](crate::SojournVm).
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct Register(Word);

impl RegisterTrait for Register {
	#[inline(always)]
	fn new(word: Word) -> Self {
		Self(word)
	}

	#[inline(always)]
	fn load(&self) -> Word {
		self.0
	}

	#[inline(always)]
	fn store(&mut self, word: Word) {
		self.0 = word;
	}

	#[inline(always)]
	fn neg_assign(&mut self) {
		self.0 = -self.0;
	}

	#[inline(always)]
	fn inv_assign(&mut self) {
		self.0 = !self.0;
	}

	#[inline(always)]
	fn not_assign(&mut self) {
		self.0 = (self.0 == 0) as Word;
	}

	#[inline(always)]
	fn finished(&mut self) { /* do nothing */ }
}


macro_rules! impl_traits {
	($($trait:ident $doc:literal $fn:ident $op:tt;)*) => {
		$(
			impl std::ops::$trait for Register {
				#[doc=$doc]
				#[inline(always)]
				fn $fn(&mut self, rhs: Self) {
					self.0 $op rhs.0;
				}
			}
		)*
	};
}

impl_traits! {
	AddAssign "Adds `rhs` to `self`." add_assign +=;
	SubAssign "Subtracts `rhs` from `self`." sub_assign -=;
	MulAssign "Multiplies `self` by `rhs`." mul_assign *=;
	DivAssign "Divides `self` by `rhs`." div_assign /=;
	RemAssign "Modulos `self` by `rhs`." rem_assign %=;
	BitAndAssign "Sets `self` to the bitwise AND of `self` and `rhs`." bitand_assign &=;
	BitOrAssign "Sets `self` to the bitwise AND OR `self` and `rhs`." bitor_assign |=;
	BitXorAssign "Sets `self` to the bitwise XOR OR `self` and `rhs`." bitxor_assign ^=;
	ShlAssign "Sets `self` to `self` shifted left by `rhs` bits." shl_assign <<=;
	ShrAssign "Sets `self` to `self` shifted right by `rhs` bits, sign-extending." shr_assign >>=;
}

macro_rules! impl_formats {
	($($trait:ident $([$($cast:tt)*])?)*) => {
		$(
			impl std::fmt::$trait for Register {
				#[inline]
				fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
					std::fmt::$trait::fmt(&(self.0 $($($cast)*)?), f)
				}
			}
		)*
	};
}

impl_formats!(Display Pointer [as *const ()] UpperHex LowerHex Octal Binary);

