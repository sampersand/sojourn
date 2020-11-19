use crate::Word;
use std::ops::{Index, IndexMut};

/// The stack that's used within [sojourn's vm](crate:SojournVm).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Stack {
	stack: Vec<Word>
}

impl Stack {
	/// Creates an empty stack
	#[inline]
	pub const fn new() -> Self {
		Self { stack: Vec::new() }
	}

	/// Creates a stack with the specified capacity
	#[inline]
	pub fn with_capacity(cap: usize) -> Self {
		Self { stack: Vec::with_capacity(cap) }
	}

	/// Gets the current stack pointer.
	#[inline]
	#[must_use]
	pub fn sp(&self) -> usize {
		self.stack.len()
	}

	/// Pushes a word onto the stack.
	#[inline]
	pub fn push(&mut self, word: Word) {
		trace!(?self, ?word, "pushed word onto stack");
		self.stack.push(word)
	}

	/// Tries to pop a word off the stack, returning `None` if the stack is empty.
	#[inline]
	pub fn pop(&mut self) -> Option<Word> {
		let ret = self.stack.pop();
		trace!(?self, ?ret, "tried popping word off the stack");
		ret
	}
}

impl AsRef<[Word]> for Stack {
	#[inline]
	fn as_ref(&self) -> &[Word] {
		self.stack.as_ref()
	}
}

impl AsMut<[Word]> for Stack {
	#[inline]
	fn as_mut(&mut self) -> &mut [Word] {
		self.stack.as_mut()
	}
}

impl AsMut<Vec<Word>> for Stack {
	#[inline]
	fn as_mut(&mut self) -> &mut Vec<Word> {
		&mut self.stack
	}
}

impl Index<usize> for Stack {
	type Output = Word;
	
	/// Gets a reference to the `nth` element, where `0` is the most-recently [`push`](Self::push)ed word.
	#[inline]
	fn index(&self, nth: usize) -> &Self::Output {
		&self.stack[nth]
	}
}

impl IndexMut<usize> for Stack {	
	/// Gets a mutable reference to the `nth` element, where `0` is the most-recently [`push`](Self::push)ed word.
	#[inline]
	fn index_mut(&mut self, nth: usize) -> &mut Self::Output {
		&mut self.stack[nth]
	}
}
