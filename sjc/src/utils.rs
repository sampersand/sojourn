use std::io::{self, Read};

pub fn read_vec(len: usize, mut inp: impl Read) -> io::Result<Vec<u8>> {
	let mut v = vec![0; len];
	inp.read_exact(&mut v).and(Ok(v))
}
