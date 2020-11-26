use crate::{Word, UWord};
use std::alloc::Layout;
use std::mem::{align_of, size_of};
use std::fmt::{self, Formatter};
use super::{HeapTrait, PointerTrait};

/// The release-mode representation of the heap.
///
/// In release mode we simply use the system allocator, so this is a ZST.
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Heap;

/// A release-mode pointer. This is simply a transparent wrapper around a `*mut u8`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct Pointer(*mut u8);

type SizeTag = usize;

impl Default for Pointer {
	#[inline(always)]
	fn default() -> Self {
		Self(std::ptr::null::<u8>() as *mut u8)
	}
}

impl fmt::Pointer for Pointer {
	#[inline]
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		fmt::Pointer::fmt(&self.0, f)
	}
}

impl PointerTrait for Pointer {
	#[inline(always)]
	fn is_null(&self) -> bool {
		self.0.is_null()
	}

	#[inline(always)]
	unsafe fn offset(self, amnt: isize) -> Self {
		Self(self.0.offset(amnt))
	}

	#[inline(always)]
	fn into_uword(self) -> UWord {
		self.0 as usize as UWord
	}

	#[inline(always)]
	fn from_uword(word: UWord) -> Self {
		Self(word as usize as *mut u8)
	}
}

impl Pointer {
	#[inline]
	unsafe fn compose(ptr: *mut u8, size: usize) -> Self {
		*(ptr as *mut SizeTag) = size as SizeTag;
		Self((ptr as *mut SizeTag).offset(1) as *mut u8)
	}

	#[inline]
	unsafe fn decompose(self) -> (*mut u8, usize) {
		let begin = (self.0 as *mut SizeTag).offset(-1);
		(begin as *mut u8, *begin as usize)
	}
}

fn layout_for(size: usize) -> Layout {
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
	size_of::<SizeTag>() + size * size_of::<u8>()
}

#[cold]
fn alloc_error(size: usize) -> ! {
	std::alloc::handle_alloc_error(layout_for(size));
}

impl HeapTrait for Heap {
	type Pointer = Pointer;

	#[inline(always)]
	fn new() -> Self {
		Self
	}

	fn malloc(&mut self, size: usize) -> Self::Pointer {
		if size == 0 {
			return Default::default();
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

	unsafe fn free(&mut self, ptr: Self::Pointer) {
		if ptr.is_null() {
			return;
		}

		let (raw_ptr, size) = ptr.decompose();

		std::alloc::dealloc(raw_ptr, layout_for(size));
	}

	unsafe fn realloc(&mut self, ptr: Self::Pointer, new_size: usize) -> Self::Pointer {
		if ptr.is_null() || new_size == 0 {
			self.free(ptr);
			return Default::default();			
		}

		// Safety: the caller guarantees that `ptr` is a valid, active pointer. Thus, decomposing it should work.
		let (old_ptr, old_size) = ptr.decompose();

		let new_ptr = std::alloc::realloc(old_ptr, layout_for(old_size), gen_layout_size(new_size));

		if new_ptr.is_null() {
			alloc_error(new_size);
		}

		Pointer::compose(new_ptr, new_size)
	}

	#[inline(always)]
	unsafe fn get_word(&self, ptr: Pointer) -> Word {
		debug_assert!(!ptr.is_null(), "attempted to deref a null pointer");

		std::ptr::read_unaligned(ptr.0 as *mut Word)
	}

	#[inline(always)]
	unsafe fn get_byte(&self, ptr: Pointer) -> u8 {
		debug_assert!(!ptr.is_null(), "attempted to deref a null pointer");

		*ptr.0
	}

	#[inline(always)]
	unsafe fn set_word(&mut self, ptr: Pointer, word: Word) {
		debug_assert!(!ptr.is_null(), "attempted to deref a null pointer");

		std::ptr::write_unaligned(ptr.0 as *mut Word, word);
	}

	#[inline(always)]
	unsafe fn set_byte(&mut self, ptr: Pointer, byte: u8) {
		debug_assert!(!ptr.is_null(), "attempted to deref a null pointer");

		*ptr.0 = byte;
	}
}
