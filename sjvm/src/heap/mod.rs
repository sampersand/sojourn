use crate::{Word, UWord};

/// The trait that represents a heap within Sojourn.
pub trait HeapTrait : std::fmt::Debug + Sized {
	type Pointer: PointerTrait;

	/// Creates a new, empty heap.
	fn new() -> Self;

	/// Creates a new heap, possibly with the suggest capacity.
	///
	/// Note that the capacity's only a suggestion: It's valid (and is the default to) to define this simply as `new`.
	#[inline]
	fn with_capacity(capacity: usize) -> Self {
		let _ = capacity;

		Self::new()
	}

	/// Get a new pointer to memory.
	///
	/// If `size` is zero, a null pointer should be returned.
	fn malloc(&mut self, size: usize) -> Self::Pointer;


	/// Frees the memory pointer to by `ptr`, which should be `size` bytes in length.
	///
	/// # Safety
	/// It's up to the caller to ensure that `ptr` is a valid pointer that was returned from [`Self::malloc`] or 
	/// [`Self::realloc`] and that it hasn't been freed already.
	unsafe fn free(&mut self, ptr: Self::Pointer);

	/// Reallocates the memory pointer to by `ptr`, which should be `size` bytes in length.
	///
	/// If `ptr` is null or `new_size` is zero, this is equivalent to calling [`Self::free`] and returning a null
	/// pointer.
	///
	/// # Safety
	/// It's up to the caller to ensure that `ptr` is a valid pointer that was returned from [`Self::malloc`] or 
	/// [`Self::realloc`].
	unsafe fn realloc(&mut self, ptr: Self::Pointer, new_size: usize) -> Self::Pointer;


	/// Dereferences the pointer, returning the word it points to.
	///
	/// # Safety
	/// It's up to the caller to ensure that `ptr` is a valid pointer that was returned from [`HeapTrait::malloc`] or 
	/// [`HeapTrait::realloc`], and that it's aligned for [`Word`]-dereferencing.
	unsafe fn get_word(&self, ptr: Self::Pointer) -> Word;

	/// Dereferences the pointer, returning the word it points to.
	///
	/// # Safety
	/// It's up to the caller to ensure that `ptr` is a valid pointer that was returned from [`HeapTrait::malloc`] or 
	/// [`HeapTrait::realloc`], and that it's aligned for [`Word`]-dereferencing.
	unsafe fn get_byte(&self, ptr: Self::Pointer) -> u8;

	/// Stores the word at the value the pointer points to.
	///
	/// # Safety
	/// It's up to the caller to ensure that `ptr` is a valid pointer that was returned from [`HeapTrait::malloc`] or 
	/// [`HeapTrait::realloc`], and that it's aligned for [`Word`]-dereferencing.
	unsafe fn set_word(&mut self, ptr: Self::Pointer, word: Word);

	/// Stores the byte at the value the pointer points to.
	///
	/// # Safety
	/// It's up to the caller to ensure that `ptr` is a valid pointer that was returned from [`HeapTrait::malloc`] or 
	/// [`HeapTrait::realloc`].
	unsafe fn set_byte(&mut self, ptr: Self::Pointer, byte: u8);
}

/// A pointer that can be used to index into a [`HeapTrait`].
pub trait PointerTrait : Sized +
	std::fmt::Debug + std::fmt::Pointer +
	Default + Clone + Copy +
	PartialEq + Eq + PartialOrd + Ord + std::hash::Hash
{
	/// Checks to see if the pointer's null.
	///
	/// The default implementation simply checks to see if we're equal to `Self::default()`.
	#[inline]
	fn is_null(&self) -> bool {
		*self == Self::default()
	}

	/// Offsets this pointer by the given amount.
	///
	/// # SAFETY: Todo... (lol). (Same reqs as the `std::pointer::offset`.)
	unsafe fn offset(self, amnt: isize) -> Self;

	/// Converts this pointer into a [`UWord`](crate::UWord).
	fn into_uword(self) -> UWord;

	/// Creates a pointer from the given [`UWord`].
	///
	/// # Safety
	/// While creating this pointer from a word isn't unsafe in-and-of itself, using may be if the given `uword` isn't
	/// a valid pointer.
	fn from_uword(uword: UWord) -> Self;
}

/// A self-hosted heap, which is useful for catching memory-based bugs. It's fairly slow though.
pub mod debug;

/// Effectively just a wrapper around [`std::alloc`].
///
/// There's a small tweak, though, because we need to be able to keep track of the allocation size within the pointer.
/// So every allocation has a hidden additional `size_of::<usize>()` bytes before the pointer that's returned.
pub mod release;

cfg_if! {
	if #[cfg(any(feature="memcheck", debug_assertions))] {
		pub use debug::*;
	} else {
		pub use release::*;
	}
}
