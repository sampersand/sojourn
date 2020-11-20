use crate::{Byte, Word};
use std::alloc::Layout;
use std::mem::{align_of, size_of};
use std::fmt::{self, Formatter};

/// The release-mode representation of the heap.
///
/// In release mode we simply use the system allocator, so this is a ZST.
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Heap;

/// A release-mode pointer. This is simply a transparent wrapper around a `*mut Byte`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct Pointer(*mut Byte);

type SizeTag = usize;

impl Default for Pointer {
	#[inline]
	fn default() -> Self {
		Self(std::ptr::null::<Byte>() as *mut Byte)
	}
}

impl fmt::Pointer for Pointer {
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		fmt::Pointer::fmt(&self.0, f)
	}
}

impl Pointer {
	/// Checks to see if the pointer's null.
	#[inline(always)]
	pub fn is_null(&self) -> bool {
		self.0.is_null()
	}

	/// Offsets this pointer by the given amount.
	///
	/// # SAFETY: Todo... (lol)
	#[inline(always)]
	pub unsafe fn offset(self, amnt: isize) -> Self {
		Self(self.0.offset(amnt))
	}

	/// Converts this pointer into a [`Word`].
	#[inline(always)]
	pub fn into_word(self) -> Word {
		self.0 as usize as Word
	}

	/// Creates a pointer from the given [`Word`].
	///
	/// # Safety
	/// While creating this pointer from a word isn't unsafe in-and-of itself, using it will be.
	#[inline(always)]
	pub fn from_word(word: Word) -> Self {
		Self(word as usize as *mut Byte)
	}

	#[inline]
	unsafe fn compose(ptr: *mut u8, size: usize) -> Self {
		*(ptr as *mut SizeTag) = size as SizeTag;
		Self((ptr as *mut SizeTag).offset(1) as *mut Byte)
	}

	#[inline]
	unsafe fn decompose(self) -> (*mut u8, usize) {
		let begin = (self.0 as *mut SizeTag).offset(-1);
		(begin as *mut u8, *begin as usize)
	}
}


fn layout_for(size: usize) -> Layout {
	// Ensure that bytes can have an alignment of 1, which means that they can also be aligned to `Word`.
	sa::assert_eq_align!(Byte, u8);
	sa::assert_eq_size!(Byte, u8);

	// sanity checks for Word alignment.
	sa::const_assert_ne!(align_of::<SizeTag>(), 0);
	sa::const_assert!(align_of::<SizeTag>().is_power_of_two());

	assert!(size <= isize::MAX as usize, "too much memory requested! (requested {:016x})", size);

	// SAFETY:
	// - `align_of<Word>() != 0` : verified above.
	// - `align_of<Word>().is_power_of_two()` : verified above.
	// - and size is small enough, because we check to make sure it's less than `isize`.
	unsafe {
		Layout::from_size_align_unchecked(gen_layout_size(size), align_of::<SizeTag>())
	}
}

#[inline]
const fn gen_layout_size(size: usize) -> usize {
	size_of::<SizeTag>() + size * size_of::<Byte>()
}


#[cold]
fn alloc_error(size: usize) -> ! {
	std::alloc::handle_alloc_error(layout_for(size));
}

impl Heap {
	/// Creates a new [`Heap`]. This is a no-op in release mode.
	#[inline]
	pub fn new() -> Self {
		Self
	}

	/// Creates a new [`Heap`], ignoring the given capacity. This is a no-op in release mode.
	#[inline]
	pub fn with_capacity(_: usize) -> Self {
		Self
	}

	/// Get a new pointer to memory.
	///
	/// If `size` is zero, a null pointer will be returned.
	pub fn malloc(&mut self, size: usize) -> Pointer {
		if size == 0 {
			return Pointer::default();
		}

		// SAFETY:
		// We already checked for size == 0, and so we know layout cannot be zero-sized.
		// we also know that the layout's valid, as `layout_for` guarantees it.
		// NB: we use `alloc_zeroed` so we won't have uninitialized memory.
		let raw_ptr = unsafe {
			std::alloc::alloc_zeroed(layout_for(size))
		};

		if raw_ptr.is_null() {
			alloc_error(size);
		}

		// SAFETY: we just created the raw pointer for `size`, so we know this is valid.
		unsafe {
			Pointer::compose(raw_ptr, size)
		}
	}

	/// Frees the memory pointer to by `ptr`, which should be `size` bytes in length.
	///
	/// # Safety
	/// It's up to the caller to ensure that `ptr` is a valid pointer that was returned from [`Heap::malloc`] or 
	/// [`Heap::realloc`] and that it hasn't been freed already.
	pub unsafe fn free(&mut self, ptr: Pointer) {
		if ptr.is_null() {
			return;
		}

		let (raw_ptr, size) = ptr.decompose();

		std::alloc::dealloc(raw_ptr, layout_for(size));
	}

	/// Reallocates the memory pointer to by `ptr`, which should be `size` bytes in length.
	///
	/// If `ptr` is null or `new_size` is zero, this is equivalent to calling [`Heap::free`] and returning a null
	/// pointer.
	///
	/// # Safety
	/// It's up to the caller to ensure that `ptr` is a valid pointer that was returned from [`Heap::malloc`] or 
	/// [`Heap::realloc`].
	pub unsafe fn realloc(&mut self, ptr: Pointer, new_size: usize) -> Pointer {
		if ptr.is_null() || new_size == 0 {
			self.free(ptr);
			return Pointer::default();			
		}

		// Safety: the caller guarantees that `ptr` is a valid, active pointer. Thus, decomposing it should work.
		let (old_ptr, old_size) = ptr.decompose();

		let new_ptr = std::alloc::realloc(old_ptr, layout_for(old_size), gen_layout_size(new_size));

		if new_ptr.is_null() {
			alloc_error(new_size);
		}

		Pointer::compose(new_ptr, new_size)
	}

	/// Dereferences the pointer, returning the word it points to.
	///
	/// # Safety
	/// It's up to the caller to ensure that `ptr` is a valid pointer that was returned from [`Heap::malloc`] or 
	/// [`Heap::realloc`], and that it's aligned for [`Word`]-dereferencing.
	#[inline(always)]
	pub unsafe fn get_word(&self, ptr: Pointer) -> Word {
		*(ptr.0 as *const Word)
	}

	/// Dereferences the pointer, returning the word it points to.
	///
	/// # Safety
	/// It's up to the caller to ensure that `ptr` is a valid pointer that was returned from [`Heap::malloc`] or 
	/// [`Heap::realloc`], and that it's aligned for [`Word`]-dereferencing.
	#[inline(always)]
	pub unsafe fn get_byte(&self, ptr: Pointer) -> u8 {
		*(ptr.0 as *mut u8)
	}

	/// Stores the word at the value the pointer points to.
	///
	/// # Safety
	/// It's up to the caller to ensure that `ptr` is a valid pointer that was returned from [`Heap::malloc`] or 
	/// [`Heap::realloc`], and that it's aligned for [`Word`]-dereferencing.
	#[inline(always)]
	pub unsafe fn set_word(&mut self, ptr: Pointer, word: Word) {
		*(ptr.0 as *mut Word) = word;
	}

	/// Stores the byte at the value the pointer points to.
	///
	/// # Safety
	/// It's up to the caller to ensure that `ptr` is a valid pointer that was returned from [`Heap::malloc`] or 
	/// [`Heap::realloc`].
	#[inline(always)]
	pub unsafe fn set_byte(&mut self, ptr: Pointer, byte: u8) {
		(*(ptr.0 as *mut Byte)).set(byte);
	}
}
