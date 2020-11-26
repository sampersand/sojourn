#[macro_use]
extern crate cfg_if;

extern crate static_assertions as sa;

#[macro_use]
extern crate tracing;

#[doc(inline)]
pub use sjef::{Word, UWord};

#[cfg_attr(not(feature="checked-uword-conversions"), inline(always))]
fn uw(word: Word) -> UWord {
	#[cfg(feature="checked-uword-conversions")]
	{ ::std::convert::TryInto::<crate::UWord>::try_into(word).expect("unable to convert to a uword!") }
	#[cfg(not(feature="checked-uword-conversions"))]
	{ word as UWord }
}

mod stack;
mod registers;
mod sojournvm;
pub mod text;
pub mod register;
pub mod heap;
 
pub use heap::{HeapTrait, PointerTrait, Heap, Pointer};
pub use stack::Stack;
pub use register::{RegisterTrait, Register};
pub use text::Text;
pub use registers::Registers;
pub use sojournvm::SojournVm;
