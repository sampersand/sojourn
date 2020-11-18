use crate::{SojournVm, Flags, Word};
use std::convert::TryFrom;

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Bytecode {
	/*  General  */

	/// `Nop()`: Do nothing.
	Nop,

	/// `Push(reg)`: `push reg`.
	Push,

	/// `PushI(i64)`: `push i64`.
	PushI,

	/// `PushBSx(i8)`: `push i8`.
	PushBSx,

	/// `Pop(reg)`: `reg <- pop()`.
	Pop,

	/// `Load(dst, reg)`: `dst <- mem[reg]`.
	Load,

	/// `Store(src, reg)`: `mem[reg] <- src`.
	Store,

	/// `Mov(dst, src)`: `dst <- src`.
	Mov,

	/// `MovW(dst, i64)`: `dst <- i64`.
	MovW,

	/// `MovBSx(dst, sbyte)`: `dst <- sbyte as i64`
	MovBSx,

	/// `Debug()`: Dump the current state of the vm
	Debug,

	/// `Interrupt(what)`: Do something special depending on `what`
	Interrupt,

	/*  Math ops  */
	/// `Inc(reg)`: `reg <- reg + 1`
	Inc,

	/// `Dec(reg)`: `reg <- reg - 1`
	Dec,

	/// `Add(dst, rhs)`: `dst <- dst + rhs`.
	Add,

	/// `Sub(dst, rhs)`: `dst <- dst - rhs`.
	Sub,

	/// `Mul(dst, rhs)`: `dst <- dst * rhs`.
	Mul,

	/// `Div(dst, rhs)`: `dst <- dst / rhs`.
	Div,

	/// `Mod(dst, rhs)`: `dst <- dst % rhs`.
	Mod,

	/// `And(dst, rhs)`: `dst <- dst & rhs`.
	And,

	/// `Or(dst, rhs)`:  `dst <- dst | rhs`.
	Or,

	/// `Xor(dst, rhs)`: `dst <- dst ^ rhs`.
	Xor,

	/// `Lsh(dst, rhs)`: `dst <- dst << rhs`.
	Lsh,

	/// `Rsh(dst, rhs)`: `dst <- dst >> rhs`.
	Rsh,

	/// `Neg(dst)`: `dst <- –dst`.
	Neg,

	/// `Inv(dst)`: `dst <- ~dst`.
	Inv,

	/// `Not(dst)`: `dst <- !dst`, where `0` -> `1`, and everything else goes to zero.
	Not,

	// SAR/SOL? ROR/L?

	/*  Jumping stuff  */
	/// `Cmp(reg)`: Sets `flags` to `ZERO`, `POS`, or `NEG`, depending on what `reg` is.
	Cmp,

	/// `Jeq(i16)`: If flags are `ZERO`, then `ip <- ip + i16`.
	Jeq,

	/// `Jne(i16)`: If flags aren't `ZERO`, then  `ip <- ip + i16`.
	Jne,

	/// `Jlt(i16)`: If flags are `NEG`, then  `ip <- ip + i16`.
	Jlt,

	/// `Jle(i16)`: If flags aren't `POS`, then  `ip <- ip + i16`.
	Jle,

	/// `Jgt(i16)`: If flags are `POS`, then  `ip <- ip + i16`.
	Jgt,

	/// `Jge(i16)`: If flags aren't `NEG`, then  `ip <- ip + i16`.
	Jge,

	/// `Jmp(i16)`: `ip <- ip + i16`.
	Jmp,

	/// `Call(i16)`: `push ip ; jmp i16`
	Call,

	/// `CallF(i64)`: `push ip ; jmpw sword` (but jmpw doesn't exist lol)
	CallF,

	/// `Ret()`: `pop ip`
	Ret,

	/// `Lfs(dst, u8)`: `dst <- stack[stack.size() - 1 - u8]`.
	Lfs,

	/// Extensions that can be used later
	Extension,

}

impl Bytecode {
	/// The last Bytecode instruction.
	const LAST_BYTECODE_INSTRUCTION: Self = Self::Lfs;
}

#[derive(Debug)]
pub struct UnknownBytecode;

impl TryFrom<u8> for Bytecode {
	type Error = UnknownBytecode;

	fn try_from(byte: u8) -> Result<Self, Self::Error> {
		if byte > Self::LAST_BYTECODE_INSTRUCTION as u8 {
			Err(UnknownBytecode)
		} else {
			// SAFETY: Since all instructions are contiguous, assuming `LAST_OPCODE_INSTRUCTION` truly
			// does point to the last instruction, every `u8` here will be valid.
			unsafe {
				Ok(std::mem::transmute(byte))
			}
		}
	}
}

impl Bytecode {
	pub fn run(self, vm: &mut SojournVm) {
		// dbg!(self);
		match self {
			Self::Nop => {},
			Self::Push => vm.stack.push(vm.registers[vm.code.next_byte()]),
			Self::PushI => vm.stack.push(vm.code.next_word()),
			Self::PushBSx => vm.stack.push(vm.code.next_byte() as Word),
			Self::Pop => vm.registers[vm.code.next_byte()] = vm.stack.pop().expect("popped from empty stack!"),
			Self::Load => todo!(),
			Self::Store => todo!(),
			Self::Mov => {
				let dst = vm.code.next_byte();
				let src = vm.registers[vm.code.next_byte()];
				vm.registers[dst] = src;
			},
			Self::MovW => {
				let dst = vm.code.next_byte();
				vm.registers[dst] = vm.code.next_word();
			},
			Self::MovBSx => {
				let dst = vm.code.next_byte();
				vm.registers[dst] = vm.code.next_byte() as i8 as Word;
			},

			Self::Debug => { dbg!(&vm); },
			Self::Interrupt => {
				match vm.code.next_byte() {
					0 => std::process::exit(vm.code.next_short() as i32),
					1 => {
						let value = &vm.data[vm.registers[vm.code.next_byte()] as usize];
						print!("{}", String::from_utf8_lossy(value))
					},
					_ => todo!("trap")
				}
			}

			// math ops
			Self::Add => {
				let dst = vm.code.next_byte(); 
				let src = vm.registers[vm.code.next_byte()];
				vm.registers[dst] += src;
			}
			Self::Sub => {
				let dst = vm.code.next_byte(); 
				let src = vm.registers[vm.code.next_byte()];
				vm.registers[dst] -= src;
			}
			Self::Mul => {
				let dst = vm.code.next_byte(); 
				let src = vm.registers[vm.code.next_byte()];
				vm.registers[dst] *= src;
			}
			Self::Div => {
				let dst = vm.code.next_byte(); 
				let src = vm.registers[vm.code.next_byte()];
				vm.registers[dst] /= src;
			}
			Self::Mod => {
				let dst = vm.code.next_byte(); 
				let src = vm.registers[vm.code.next_byte()];
				vm.registers[dst] %= src;
			}
			Self::And => {
				let dst = vm.code.next_byte(); 
				let src = vm.registers[vm.code.next_byte()];
				vm.registers[dst] &= src;
			}
			Self::Or => {
				let dst = vm.code.next_byte(); 
				let src = vm.registers[vm.code.next_byte()];
				vm.registers[dst] |= src;
			}
			Self::Xor => {
				let dst = vm.code.next_byte(); 
				let src = vm.registers[vm.code.next_byte()];
				vm.registers[dst] ^= src;
			}
			Self::Lsh => {
				let dst = vm.code.next_byte(); 
				let src = vm.registers[vm.code.next_byte()];
				vm.registers[dst] <<= src;
			}
			Self::Rsh => {
				let dst = vm.code.next_byte(); 
				let src = vm.registers[vm.code.next_byte()];
				vm.registers[dst] >>= src;
			}
			Self::Neg => {
				let dst = vm.code.next_byte(); 
				vm.registers[dst] = -vm.registers[dst];
			}
			Self::Inv => {
				let dst = vm.code.next_byte(); 
				vm.registers[dst] = !vm.registers[dst];
			}
			Self::Not => {
				let dst = vm.code.next_byte(); 
				vm.registers[dst] = !(vm.registers[dst] != 0) as i64;
			},

			// jumping
			Self::Cmp => {
				vm.flags &= Flags::CMP;

				match vm.registers[vm.code.next_byte()].cmp(&0) {
					std::cmp::Ordering::Less => vm.flags |= Flags::NEG,
					std::cmp::Ordering::Equal => vm.flags |= Flags::ZERO,
					std::cmp::Ordering::Greater => vm.flags |= Flags::POS,
				}
			},

			Self::Jeq => {
				let dst = vm.code.next_short() as i16;
				if vm.flags.contains(Flags::ZERO) {
					vm.code.offset_ip(dst as isize - 3);
				}
			},
			Self::Jne => {
				let dst = vm.code.next_short() as i16;
				if !vm.flags.contains(Flags::ZERO) {
					vm.code.offset_ip(dst as isize - 3);
				}
			},
			Self::Jlt => {
				let dst = vm.code.next_short() as i16;
				if vm.flags.contains(Flags::NEG) {
					vm.code.offset_ip(dst as isize - 3);
				}
			},
			Self::Jle => {
				let dst = vm.code.next_short() as i16;
				if !vm.flags.contains(Flags::POS) {
					vm.code.offset_ip(dst as isize - 3);
				}
			},
			Self::Jgt => {
				let dst = vm.code.next_short() as i16;
				if vm.flags.contains(Flags::POS) {
					vm.code.offset_ip(dst as isize - 3);
				}
			},
			Self::Jge => {
				let dst = vm.code.next_short() as i16;
				if !vm.flags.contains(Flags::NEG) {
					vm.code.offset_ip(dst as isize - 3);
				}
			},
			Self::Jmp => {
				let dst = vm.code.next_short() as i16;
				vm.code.offset_ip(dst as isize - 3);
			},

			Self::Inc => vm.registers[vm.code.next_byte()] += 1,
			Self::Dec => vm.registers[vm.code.next_byte()] -= 1,

			Self::Call => {
				let dst = vm.code.next_sshort();
				vm.stack.push(vm.code.ip_as_word());
				vm.code.offset_ip(dst as isize - 1 - std::mem::size_of_val(&dst) as isize);
			},
			Self::CallF => {
				let dst = vm.code.next_sword();
				vm.stack.push(vm.code.ip_as_word());
				vm.code.offset_ip(dst as isize - 1 - std::mem::size_of_val(&dst) as isize);
			},
			Self::Ret => {
				let retpos = vm.stack.pop().unwrap();
				vm.code.set_ip_from_word(retpos);
			},
			Self::Lfs => {
				let dst = vm.code.next_byte();
				let which = vm.code.next_byte() as usize;
				vm.registers[dst] = vm.stack[vm.stack.len() - 2 - which]; // - 2 b/c len + ret stack ptrv
			},
			Self::Extension => todo!("extensions")
		}
	}
}

