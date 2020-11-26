use crate::{UWord, Word};
use std::io::{self, Read, Seek, SeekFrom};
// use super::TextTrait;

/// An implementation of [`TextTrait`] with pointers.
pub struct Text {
	ptr: *const u8,
	len: usize,
	ip: UWord
}

impl Text {
	#[inline(always)]
	pub fn ip(&self) -> UWord {
		self.ip
	}

	#[inline(always)]
	pub fn set_ip(&mut self, ip: UWord){
		self.ip = ip;
	}

	#[inline(always)]
	pub fn is_eof(&self) -> bool {
		(self.len as UWord) <= self.ip
	}

	/// Seeks to the given offset.
	pub fn offset_ip(&mut self, amnt: Word) {
		self.ip += amnt;
	}
}
