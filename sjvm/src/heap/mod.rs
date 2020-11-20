#[cfg_attr(feature="mock-heap", path="debug.rs")]
#[cfg_attr(not(feature="mock-heap"), path="release.rs")]
mod heap;

pub use heap::*;
