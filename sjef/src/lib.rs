//! This crate houses all details regarding Sojourn's file format, including [`Instruction`]s.

#[macro_use]
extern crate tracing;

extern crate static_assertions as sa;

mod text;
mod data;
mod file;
pub mod instruction;

pub use text::Text;
pub use data::Data;
pub use file::File;
pub use instruction::Instruction;

/// The size of a word in sojourn.
pub type Word = i64;

/// The size of an unsigned word in sojourn.
pub type UWord = u64;

sa::assert_eq_align!(Word, UWord);
sa::assert_eq_size!(Word, UWord);
