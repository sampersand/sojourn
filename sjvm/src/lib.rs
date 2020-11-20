extern crate static_assertions as sa;

#[macro_use]
extern crate tracing;

#[doc(inline)]
pub use sjef::Word;

pub type Byte = MaybeUninit<u8>;

mod maybe_uninit;
mod heap;
mod stack;
mod register;
mod registers;
mod bytecode;
mod sojournvm;
 
 // TODO: sojourn vm, but it can take a seek type.
pub use maybe_uninit::MaybeUninit;
pub use heap::{Heap, Pointer};
pub use stack::Stack;
pub use register::Register;
pub use bytecode::ByteCode;
pub use registers::Registers;
pub use sojournvm::SojournVm;
