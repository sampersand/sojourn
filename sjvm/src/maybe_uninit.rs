	use std::fmt::{self, Debug, Formatter};

/// A type that's used to house a value that's possibly uninitialized
///
/// This is intended to be used as a debugging aid. In release mode, an undefined value will be returned if an
/// uninitialized value is read.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct MaybeUninit<T>(Inner<T>);

#[cfg(debug_assertions)]
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(align(1))]
enum Inner<T> { Uninit, Init(T) }

#[cfg(not(debug_assertions))]
#[derive(Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
struct Inner<T>(T);

impl<T: Debug> Debug for MaybeUninit<T> {
	#[cfg_attr(not(debug_assertions), inline(always))]
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		#[cfg(debug_assertions)]
		{
			if let Inner::Init(value) = &self.0 {
				Debug::fmt(value, f)
			} else {
				write!(f, "<uninit>")
			}
		}

		#[cfg(not(debug_assertions))]
		{ Debug::fmt(&(self.0).0, f) }
	}
}

impl<T: Default> Default for MaybeUninit<T> {
	/// Creates a new, uninitialized [`MaybeUninit`].
	#[cfg_attr(debug_assertions, inline)]
	#[cfg_attr(not(debug_assertions), inline(always))]
	fn default() -> Self {
		#[cfg(debug_assertions)]
		{ Self(Inner::Uninit) }

		#[cfg(not(debug_assertions))]
		{ Self(Inner(T::default())) }
	}
}

impl<T> MaybeUninit<T> {
	/// Creates a new, uninitialized [`MaybeUninit`] in a constant context.
	///
	/// If debug assertions are disabled, a value must be supplied.
	#[cfg(debug_assertions)]
	#[inline]
	pub const fn const_uninit() -> Self {
		Self(Inner::Uninit)
	}

	/// Creates a new, uninitialized [`MaybeUninit`] in a constant context.
	///
	/// If debug assertions are disabled, the given value is used as a default.
	#[cfg(not(debug_assertions))]
	#[inline(always)]
	pub const fn const_uninit_release(value: T) -> Self {
		Self(Inner(value))
	}

	/// Creates a new, initialized [`MaybeUninit`].
	#[cfg_attr(debug_assertions, inline)]
	#[cfg_attr(not(debug_assertions), inline(always))]
	pub const fn new(value: T) -> Self {
		#[cfg(debug_assertions)]
		{ Self(Inner::Init(value)) }

		#[cfg(not(debug_assertions))]
		{ Self(Inner(value)) }
	}

	#[cfg_attr(debug_assertions, inline)]
	#[cfg_attr(not(debug_assertions), inline(always))]
	pub const fn is_init(&self) -> bool {
		#[cfg(debug_assertions)]
		{ matches!(self, Self(Inner::Init(_))) }
		#[cfg(not(debug_assertions))]
		{ true }
	}

	/// Gets a reference to the enclosed `T`.
	///
	/// # Panics
	/// When `debug_assertions` are enabled, this function will panic when called on an uninitialized value. When not
	/// enabled, a garbage value is returned.
	#[cfg_attr(debug_assertions, inline)]
	#[cfg_attr(not(debug_assertions), inline(always))]
	pub fn get(&self) -> &T {
		#[cfg(debug_assertions)]
		{
			if let Inner::Init(value) = &self.0 {
				value
			} else {
				panic!("attempted to fetch an uninitialized value!");
			}
		}

		#[cfg(not(debug_assertions))]
		{ &(self.0).0 }
	}

	/// Gets a mutable reference to the enclosed `T`.
	///
	/// # Panics
	/// When `debug_assertions` are enabled, this function will panic when called on an uninitialized value. When not
	/// enabled, a garbage value is returned.
	#[cfg_attr(debug_assertions, inline)]
	#[cfg_attr(not(debug_assertions), inline(always))]
	pub fn get_mut(&mut self) -> &mut T {
		#[cfg(debug_assertions)]
		{
			if let Inner::Init(value) = &mut self.0 {
				value
			} else {
				panic!("attempted to fetch an uninitialized value!");
			}
		}

		#[cfg(not(debug_assertions))]
		{ &mut (self.0).0 }
	}

	/// Stores the value `T` into self.
	/// enabled, a garbage value is returned.
	#[cfg_attr(debug_assertions, inline)]
	#[cfg_attr(not(debug_assertions), inline(always))]
	pub fn set(&mut self, value: T) {
		#[cfg(debug_assertions)]
		{ self.0 = Inner::Init(value); }

		#[cfg(not(debug_assertions))]
		{ self.0 = Inner(value); }
	}
}
