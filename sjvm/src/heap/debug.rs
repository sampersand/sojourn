use crate::{Byte, Word};
use std::collections::HashMap;
use std::fmt::{self, Formatter};
use std::ops::RangeInclusive;
use std::mem::size_of;

/// The debug-mode representation of the heap.
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Heap {
	map: HashMap<Pointer, Vec<Byte>>,
	ranges: Vec<RangeInclusive<Pointer>>
}

/// A debug-mode pointer.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Pointer(usize);

impl fmt::Pointer for Pointer {
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		fmt::Pointer::fmt(&(self.0 as *const ()), f)
	}
}

impl Pointer {
	/// Checks to see if the pointer's null.
	#[inline]
	pub fn is_null(&self) -> bool {
		self.0 == 0
	}

	/// Offsets this pointer by the given amount.
	///
	/// # SAFETY: Todo... (lol)
	pub unsafe fn offset(self, amnt: isize) -> Self {
		if amnt < 0 {
			Self(self.0 - amnt.abs() as usize)
		} else {
			Self(self.0 + amnt.abs() as usize)
		}
	}

	/// Converts this pointer into a [`Word`].
	#[inline]
	pub fn into_word(self) -> Word {
		self.0 as Word
	}

	/// Creates a pointer from the given [`Word`].
	///
	/// # Safety
	/// While creating this pointer from a word isn't unsafe in-and-of itself, using it will be.
	#[inline]
	pub fn from_word(word: Word) -> Self {
		Self(word as usize)
	}

	fn for_size(len: usize) -> Self {
		use std::sync::atomic::{AtomicUsize, Ordering::Relaxed};
		static POINTER_INCR: AtomicUsize = AtomicUsize::new(1);

		// NOTE: This _will_ fail if we ever wrap around a `usize`'s length. But I don't suspect that'll happen any time
		// soon, so this is good enough. (If we get to that point, this entire file can/should be redesigned anyways.)
		Self(POINTER_INCR.fetch_add(len, Relaxed))
	}
}

impl Heap {
	/// Creates a new [`Heap`].
	pub fn new() -> Self {
		Self {
			map: HashMap::new(),
			ranges: Vec::new()
		}
	}

	/// Creates a new [`Heap`] with the given starting capacity.
	pub fn with_capacity(capacity: usize) -> Self {
		Self {
			map: HashMap::with_capacity(capacity),
			ranges: Vec::new()
		}
	}

	/// Get a new pointer to memory.
	///
	/// If `size` is zero, a null pointer will be returned.
	pub fn malloc(&mut self, size: usize) -> Pointer {
		if size == 0 {
			return Pointer::default();
		}

		let pointer = Pointer::for_size(size);
		self.insert(pointer, vec![Default::default(); size], size);
		pointer
	}

	fn insert(&mut self, pointer: Pointer, vec: Vec<Byte>, size: usize) {
		self.map.insert(pointer, vec);
		self.ranges.push(pointer..=Pointer(pointer.0 + size));
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

		self.remove(ptr);
	}

	fn remove(&mut self, ptr: Pointer) -> Vec<Byte> {
		let value =
			if let Some(vec) = self.map.remove(&ptr) {
				vec
			} else {
				panic!("Double free detected! ptr={:p}\nheap={:?}", ptr, self)
			};

		for i in 0..self.ranges.len() {
			if self.ranges[i].contains(&ptr) {
				self.ranges.swap_remove(i);
				return value;
			}
		}

		panic!("internal error: ptr ({:p}) removed, but wasn't in ranges.\nheap={:?}", ptr, self);
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

		// Note we always reallocate the pointer so we can catch any memory bugs relating to the old pointer.
		let mut vec = self.remove(ptr);
		vec.resize_with(new_size, Default::default);
		let new_ptr = Pointer::for_size(new_size);

		self.insert(new_ptr, vec, new_size);
		new_ptr
	}

	/// Dereferences the pointer, returning the word it points to.
	///
	/// # Safety
	/// It's up to the caller to ensure that `ptr` is a valid pointer that was returned from [`Heap::malloc`] or 
	/// [`Heap::realloc`], and that it's aligned for [`Word`]-dereferencing.
	pub unsafe fn get_word(&self, ptr: Pointer) -> Word {
		for range in &self.ranges {
			if range.contains(&ptr) {
				let vec = &self.map[range.start()];

				let idx = ptr.0 - range.start().0;
				assert!(idx < vec.len(),
					"Attempted to get a word in not-fully-mapped memory at {:p} (range={:?})\nheap={:?}", ptr, range, self);

				let mut bytes = [0; size_of::<Word>()];

				for i in 0..size_of::<Word>() {
					bytes[i] = *vec[idx + i].get();
				}

				return Word::from_le_bytes(bytes);
			}
		}
		
		panic!("Attempted to get a word in unmapped memory at {:p}\nheap={:?}", ptr, self);
	}

	/// Dereferences the pointer, returning the word it points to.
	///
	/// # Safety
	/// It's up to the caller to ensure that `ptr` is a valid pointer that was returned from [`Heap::malloc`] or 
	/// [`Heap::realloc`], and that it's aligned for [`Word`]-dereferencing.
	pub unsafe fn get_byte(&self, ptr: Pointer) -> u8 {
		for range in &self.ranges {
			if range.contains(&ptr) {
				let vec = &self.map[range.start()];
				let idx = ptr.0 - range.start().0;
				assert!(idx < vec.len(), "Attempted get a byte in not-fully-mapped memory at {:p} (range={:?})\nheap={:?}",
					ptr, range, self);

				return *vec[idx].get();
			}
		}

		panic!("Attempted to get a byte in unmapped memory at {:p}\nheap={:?}", ptr, self);
	}

	/// Stores the word at the value the pointer points to.
	///
	/// # Safety
	/// It's up to the caller to ensure that `ptr` is a valid pointer that was returned from [`Heap::malloc`] or 
	/// [`Heap::realloc`], and that it's aligned for [`Word`]-dereferencing.
	pub unsafe fn set_word(&mut self, ptr: Pointer, word: Word) {
		for range in &self.ranges {
			if range.contains(&ptr) {
				let vec = &mut self.map.get_mut(range.start()).expect("internal error: ranges/heap don't align");

				let idx = ptr.0 - range.start().0;
				assert!(idx < vec.len(),
					"Attempted to store a word in not-fully-mapped memory at {:p} (range={:?})\nheap={:?}", ptr, range,
					self);

				let bytes = word.to_le_bytes();

				for i in 0..size_of::<Word>() {
					vec[idx + i].set(bytes[i]);
				}

				return;
			}
		}

		panic!("Attempted to store a word in unmapped memory at {:p}\nheap={:?}", ptr, self);
	}

	/// Stores the byte at the value the pointer points to.
	///
	/// # Safety
	/// It's up to the caller to ensure that `ptr` is a valid pointer that was returned from [`Heap::malloc`] or 
	/// [`Heap::realloc`].
	pub unsafe fn set_byte(&mut self, ptr: Pointer, byte: u8) {
		for range in &self.ranges {
			if range.contains(&ptr) {
				let vec = &mut self.map.get_mut(range.start()).expect("internal error: ranges/heap don't align");
				let idx = ptr.0 - range.start().0;
				assert!(idx < vec.len(),
					"Attempted store a byte in not-fully-mapped memory at {:p} (range={:?})\nheap={:?}", ptr, range, self);

				vec[idx].set(byte);

				return;
			}
		}

		panic!("Attempted to store a byte in unmapped memory at {:p}\nheap={:?}", ptr, self);
	}
}
