use crate::Word;
use read_from::{ReadFrom, WriteTo, LittleEndian};
use std::io::{self, Read, Write};
use std::fmt::{self, Display, LowerHex, UpperHex, Binary, Formatter};

pub mod reg;
pub mod interrupt;

pub use interrupt::Interrupt;
pub use reg::Reg;

/// A sojourn bytecode instruction
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Instruction {
	/*** General ***/

	/// Do nothing.
	Nop(),

	/// Pushes the register's contents onto the stack.
	///
	/// ```
	/// stack[sp++] <- reg
	/// ```
	Push(Reg),

	/// Push the word onto the stack.
	///
	/// ```
	/// stack[sp++] <- word
	PushI(Word),

	/// Push the byte onto the stack, sign-extending it if it's negative.
	///
	/// ```
	/// stack[s++] <- i8 as Word (sign-extended)
	/// ```
	PushBSx(i8),

	/// Pops the last value on the stack into the register.
	///
	/// ```
	/// reg <- stack[--sp]
	/// ```
	Pop(Reg),

	/// Loads the contents from memory pointed to by the second register into the first.
	///
	/// ```
	/// reg1 <- memory[reg2]
	/// ```
	Load(Reg, Reg),

	/// Stores the second register's value into the memory location pointed to by the first register.
	///
	/// ```
	/// memory[reg1] <- reg2
	/// ```
	Store(Reg, Reg),

	/// Copies the second register's contents into the first.
	///
	/// ```
	/// reg1 <- reg2
	/// ```
	Mov(Reg, Reg),

	/// Moves the word into the register.
	///
	/// ```
	/// reg <- word
	/// ```
	MovW(Reg, Word),

	/// Moves the signed byte into the register, sign-extending if the byte is negative.
	///
	/// ```
	/// reg1 <- i8 as Word (sign-extended)
	/// ```
	MovBSx(Reg, i8),

	/// Dump the current state of the VM.
	Dbg(),

	/// Interrupt the VM, asking it to do something special based on the type given.
	///
	/// ```
	/// <see Interrupt for more details> 
	/// ```
	Int(Interrupt),

	/*** Math ***/

	/// Increments the register's contents by one.
	///
	/// ```
	/// reg <- reg + 1
	/// ```
	Inc(Reg),

	/// Decrements the register's contents by one.
	///
	/// ```
	/// reg <- reg - 1
	/// ```
	Dec(Reg),

	/// Adds the second register's contents to the first's.
	///
	/// ```
	/// reg1 <- reg1 + reg2
	/// ```
	Add(Reg, Reg),

	/// Subtracts the second register's contents from the first's.
	///
	/// ```
	/// reg1 <- reg1 - reg2
	/// ```
	Sub(Reg, Reg),

	/// Multiplies the second register's contents by the first's.
	///
	/// ```
	/// reg1 <- reg1 * reg2
	/// ```
	Mul(Reg, Reg),

	/// Divides the second register's contents from the first's.
	///
	/// ```
	/// reg1 <- reg1 / reg2
	/// ```
	///
	/// # Panics
	/// If the second register is zero, this will panic.
	/// (In the future, this may change to a flag being set.)
	Div(Reg, Reg),

	/// Sets the first register's contents equal to it mod the second
	///
	/// ```
	/// reg1 <- reg1 % reg2
	/// ```
	///
	/// # Panics
	/// If the second register is zero, this will panic.
	/// (In the future, this may change to a flag being set.)
	Mod(Reg, Reg),

	/// Bitwise ANDs the second register's contents and the first's, storing the result in the first register.
	///
	/// ```
	/// reg1 <- reg1 & reg2
	/// ```
	And(Reg, Reg),

	/// Bitwise ORs the second register's contents and the first's, storing the result in the first register.
	///
	/// ```
	/// reg1 <- reg1 | reg2
	/// ```
	Or(Reg, Reg),

	/// Bitwise XORs the second register's contents and the first's, storing the result in the first register.
	///
	/// ```
	/// reg1 <- reg1 ^ reg2
	/// ```
	Xor(Reg, Reg),

	/// Shifts the first register left by the second register's contents.
	///
	/// ```
	/// reg1 <- reg1 << reg2
	/// ```
	Lsh(Reg, Reg),

	/// Shifts the first register right by the second register's contents.
	/// # TODO: does this sign-extend?
	///
	/// ```
	/// reg1 <- reg1 >> reg2
	/// ```
	Rsh(Reg, Reg),

	/// Negates the register.
	///
	/// ```
	/// reg <- -reg
	/// ```
	Neg(Reg),

	/// Bitwise inverts the register.
	///
	/// ```
	/// reg <- ~reg (= -reg - 1)
	/// ```
	Inv(Reg),

	/// Logical negation of the register: Zero becomes one, and everything else becomes zero.
	///
	/// ```
	/// reg <- <reg == 0 ? 1 : 0>
	/// ```
	Not(Reg),

	/*** Control Flow ***/

	/// Sets `cmpflags` to `ZERO`, `POS`, or `NEG`, depending on if the register's contents are zero, positive, or
	/// negative.
	///
	/// ```
	/// cmpflags <- <cmp reg>
	/// ```
	Cmp(Reg),

	/// If `cmpflags` are `ZERO`, then change the instruction pointer by the given offset.
	///
	/// ```
	/// ip <- ip + <cmpflags == ZERO ? offset : 0>
	/// ```
	Jeq(i16),

	/// If `cmpflags` aren't `ZERO`, then change the instruction pointer by the given offset.
	///
	/// ```
	/// ip <- ip + <cmpflags != ZERO ? offset : 0>
	/// ```
	Jne(i16),

	/// If `cmpflags` are `NEG`, then change the instruction pointer by the given offset.
	///
	/// ```
	/// ip <- ip + <cmpflags == NEG ? offset : 0>
	/// ```
	Jlt(i16),

	/// If `cmpflags` aren't `POS`, then change the instruction pointer by the given offset.
	///
	/// ```
	/// ip <- ip + <cmpflags != POS ? offset : 0>
	/// ```
	Jle(i16),

	/// If `cmpflags` are `POS`, then change the instruction pointer by the given offset.
	///
	/// ```
	/// ip <- ip + <cmpflags == POS ? offset : 0>
	/// ```
	Jgt(i16),

	/// If `cmpflags` aren't `NEG`, then change the instruction pointer by the given offset.
	///
	/// ```
	/// ip <- ip + <cmpflags == NEG ? offset : 0>
	/// ```
	Jge(i16),

	/// Change the instruction pointer by the given offset.
	///
	/// ```
	/// ip <- ip + offset
	/// ```
	Jmp(i16),

	/// Calls the function at the given offset, pushing the instruction pointer onto the stack.
	///
	/// Before calling the function, all arguments should be pushed on the stack, and afterwards should be [`Pop`](
	/// Instruction::Pop)ped off. Arguments can be accessed via [`Lfs`](Instruction::Lfs).
	///
	/// For example, for `square(3, 4)`, you'd have `pushbsx 3; pushbsx 4; call <square's offset from here>'
	///
	/// ```
	/// stack[sp++] <- ip : ip <- ip + offset
	/// ```
	Call(i16),

	/// The exact same as [`Call`](Instruction::Call), except it takes an `Word` instead of an `Word` for far jumps.
	CallF(Word),

	/// Pops an absolute address off the stack and returns to it.
	///
	/// Note that the return value should be housed in [`Reg::RETURN_REG`].
	///
	/// ```
	/// ip <- stack[--sp]
	/// ```
	Ret(),

	/// Loads a value from the stack, without removing it.
	///
	/// This starts counting at the second from the top, as the first value is the instruction pointer.
	///
	/// ```
	/// reg <- stack[sp - 2 - idx]
	///
	Lfs(Reg, u8),

	/// An instruction that allows for extensions to the bytecode to be added later.
	Ext(u8),
}

type OpCodeRepr = u8;

/// The opcodes that are used to identify instructions.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct OpCode(OpCodeRepr);

macro_rules! declare_opcodes {
	($($instr:ident)*) => {
		#[allow(non_upper_case_globals)]
		impl OpCode {
			declare_opcodes!(@0; $($instr)* __MAX);

			const Ext: Self = Self(-1 as _);

		}

		impl From<&Instruction> for OpCode {
			fn from(instr: &Instruction) -> Self {
				sa::const_assert_ne!(OpCode::Ext.0, OpCode::__MAX.0);

				match instr {
					$(Instruction::$instr(..) => Self::$instr,)*
					Instruction::Ext(..) => Self::Ext
				}
			}
		}

		impl WriteTo for OpCode {
			type Error = io::Error;
			fn write_to<W: Write>(&self, mut out: W) -> Result<usize, Self::Error> {
				LittleEndian(self.0)
					.write_to(&mut out)
					.and(Ok(std::mem::size_of::<Self>()))
			}
		}

		impl ReadFrom for OpCode {
			type Error = ReadError;

			fn read_from<R: Read>(inp: R) -> Result<Self, Self::Error> {
				match LittleEndian::read_from(inp) {
					Ok(LittleEndian(val)) if val < Self::__MAX.0 => {
						let this = Self(val);
						trace!(self = ?this, "parsed opcode byte");
						Ok(this)
					},
					Ok(LittleEndian(val)) => {
						debug!(?val, "unknown opcode");
						Err(ReadError::UnknownOpCode(val))
					} 
					Err(err) => {
						debug!(?err, "unable to parse opcode");
						Err(ReadError::Io(err))
					} 
				}

			}
		}
	};
	(@$pos:expr; ) => {};
	(@$pos:expr; $instr:ident $($rest:tt)*) => {
		const $instr: Self = Self($pos);
		declare_opcodes!(@$pos + 1; $($rest)*);
	};
}

declare_opcodes!{ 
	Nop Push PushI PushBSx Pop Load Store Mov MovW MovBSx Dbg Int
	Inc Dec Add Sub Mul Div Mod And Or Xor Lsh Rsh Neg Inv Not
	Cmp Jeq Jne Jlt Jle Jgt Jge Jmp Call CallF Ret Lfs
}

impl Instruction {
	/// Whether or not this instruction takes an offset. Used for disassembling.
	pub fn get_offset(&self) -> Option<i16> {
		match *self {
			Self::Jeq(offset)
				| Self::Jne(offset)
				| Self::Jlt(offset)
				| Self::Jle(offset)
				| Self::Jgt(offset)
				| Self::Jge(offset)
				| Self::Jmp(offset)
				| Self::Call(offset) => Some(offset),
			_ => None
		}
	}

	/// How many bytes it takes to represent this instruction
	pub fn byte_len(&self) -> usize {
		use std::mem::{size_of};
		size_of::<OpCode>() +
			match self {
				Self::Nop() 
					| Self::Dbg()
					| Self::Ret() => 0,
				Self::Push(_)
					| Self::Pop(_)
					| Self::Inc(_)
					| Self::Dec(_)
					| Self::Neg(_)
					| Self::Inv(_)
					| Self::Not(_)
					| Self::Cmp(_) => 1,
				Self::Load(_, _)
					| Self::Store(_, _)
					| Self::Mov(_, _)
					| Self::Add(_, _)
					| Self::Sub(_, _)
					| Self::Mul(_, _)
					| Self::Div(_, _)
					| Self::Mod(_, _)
					| Self::And(_, _)
					| Self::Or(_, _)
					| Self::Xor(_, _)
					| Self::Lsh(_, _)
					| Self::Rsh(_, _) => size_of::<Reg>() * 2,
				Self::PushBSx(_) => size_of::<i8>(),
				Self::Jeq(_)
					| Self::Jne(_)
					| Self::Jlt(_)
					| Self::Jle(_)
					| Self::Jgt(_)
					| Self::Jge(_)
					| Self::Jmp(_)
					| Self::Call(_) => size_of::<i16>(),
				Self::PushI(_)
					| Self::CallF(_) => size_of::<Word>(),

				Self::MovW(_, _) => size_of::<Reg>() + size_of::<Word>(),
				Self::MovBSx(_, _) => size_of::<Reg>() + size_of::<i8>(),
				Self::Lfs(_, _) => size_of::<Reg>() + size_of::<u8>(),
				Self::Int(it) => it.byte_len(),
				Self::Ext(_) => panic!("'ext' is not currently supported"),
			}
	}
}

/// The errors that can occur whilst [reading](Instruction::read_from) an [`Instruction`].
#[derive(Debug)]
pub enum ReadError {
	/// The value doesn't correspond to a known opcode.
	UnknownOpCode(OpCodeRepr),

	/// There was a problem parsing an interrupt
	BadInterrupt(interrupt::ReadError),

	/// An I/O error happened.
	Io(io::Error)
}

impl From<io::Error> for ReadError {
	#[inline]
	fn from(err: io::Error) -> Self {
		Self::Io(err)
	}
}

impl Display for ReadError {
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		match self {
			Self::UnknownOpCode(op) => write!(f, "unknown opcode: {:02x}", op),
			Self::BadInterrupt(err) => Display::fmt(err, f),
			Self::Io(err) => Display::fmt(err, f)
		}
	}
}

impl std::error::Error for ReadError {
	fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
		match self {
			Self::UnknownOpCode(_) => None,
			Self::BadInterrupt(err) => Some(err),
			Self::Io(err) => Some(err),
		}
	}
}

impl ReadFrom for Instruction {
	type Error = ReadError;

	#[instrument(level="trace", skip(inp))]
	fn read_from<R: Read>(mut inp: R) -> Result<Self, Self::Error> {
		let opcode = OpCode::read_from(&mut inp)?;
		trace!(?opcode, "read opcode");

		let this = 
			match opcode {
				OpCode::Nop => Self::Nop(),
				OpCode::Push => Self::Push(Reg::read_from(&mut inp)?),
				OpCode::PushI => Self::PushI(LittleEndian::read_from(&mut inp)?.0),
				OpCode::PushBSx => Self::PushBSx(i8::read_from(&mut inp)?),
				OpCode::Pop => Self::Pop(Reg::read_from(&mut inp)?),
				OpCode::Load => Self::Load(Reg::read_from(&mut inp)?, Reg::read_from(&mut inp)?),
				OpCode::Store => Self::Store(Reg::read_from(&mut inp)?, Reg::read_from(&mut inp)?),
				OpCode::Mov => Self::Mov(Reg::read_from(&mut inp)?, Reg::read_from(&mut inp)?),
				OpCode::MovW => Self::MovW(Reg::read_from(&mut inp)?, LittleEndian::read_from(&mut inp)?.0),
				OpCode::MovBSx => Self::MovBSx(Reg::read_from(&mut inp)?, LittleEndian::read_from(&mut inp)?.0),
				OpCode::Dbg => Self::Dbg(),
				OpCode::Int => Self::Int(Interrupt::read_from(&mut inp).map_err(ReadError::BadInterrupt)?),
				OpCode::Inc => Self::Inc(Reg::read_from(&mut inp)?),
				OpCode::Dec => Self::Dec(Reg::read_from(&mut inp)?),
				OpCode::Add => Self::Add(Reg::read_from(&mut inp)?, Reg::read_from(&mut inp)?),
				OpCode::Sub => Self::Sub(Reg::read_from(&mut inp)?, Reg::read_from(&mut inp)?),
				OpCode::Mul => Self::Mul(Reg::read_from(&mut inp)?, Reg::read_from(&mut inp)?),
				OpCode::Div => Self::Div(Reg::read_from(&mut inp)?, Reg::read_from(&mut inp)?),
				OpCode::Mod => Self::Mod(Reg::read_from(&mut inp)?, Reg::read_from(&mut inp)?),
				OpCode::And => Self::And(Reg::read_from(&mut inp)?, Reg::read_from(&mut inp)?),
				OpCode::Or => Self::Or(Reg::read_from(&mut inp)?, Reg::read_from(&mut inp)?),
				OpCode::Xor => Self::Xor(Reg::read_from(&mut inp)?, Reg::read_from(&mut inp)?),
				OpCode::Lsh => Self::Lsh(Reg::read_from(&mut inp)?, Reg::read_from(&mut inp)?),
				OpCode::Rsh => Self::Rsh(Reg::read_from(&mut inp)?, Reg::read_from(&mut inp)?),
				OpCode::Neg => Self::Neg(Reg::read_from(&mut inp)?),
				OpCode::Inv => Self::Inv(Reg::read_from(&mut inp)?),
				OpCode::Not => Self::Not(Reg::read_from(&mut inp)?),
				OpCode::Cmp => Self::Cmp(Reg::read_from(&mut inp)?),
				OpCode::Jeq => Self::Jeq(LittleEndian::read_from(&mut inp)?.0),
				OpCode::Jne => Self::Jne(LittleEndian::read_from(&mut inp)?.0),
				OpCode::Jlt => Self::Jlt(LittleEndian::read_from(&mut inp)?.0),
				OpCode::Jle => Self::Jle(LittleEndian::read_from(&mut inp)?.0),
				OpCode::Jgt => Self::Jgt(LittleEndian::read_from(&mut inp)?.0),
				OpCode::Jge => Self::Jge(LittleEndian::read_from(&mut inp)?.0),
				OpCode::Jmp => Self::Jmp(LittleEndian::read_from(&mut inp)?.0),
				OpCode::Call => Self::Call(LittleEndian::read_from(&mut inp)?.0),
				OpCode::CallF => Self::CallF(LittleEndian::read_from(&mut inp)?.0),
				OpCode::Ret => Self::Ret(),
				OpCode::Lfs => Self::Lfs(Reg::read_from(&mut inp)?, u8::read_from(&mut inp)?),
				OpCode::Ext => panic!("ext is not implemented yet"),
				other => unreachable!("internal error: opcode {:?} somehow encountered", other)
			};

		trace!(self = ?this, "read an instruction in");
		Ok(this)
	}
}

impl WriteTo for Instruction {
	type Error = io::Error;

	fn write_to<W: Write>(&self, mut out: W) -> Result<usize, Self::Error> {
		Ok(OpCode::from(self).write_to(&mut out)? +
			match *self {
				Self::Nop() 
					| Self::Dbg()
					| Self::Ret() => 0,
				Self::Push(reg)
					| Self::Pop(reg)
					| Self::Inc(reg)
					| Self::Dec(reg)
					| Self::Neg(reg)
					| Self::Inv(reg)
					| Self::Not(reg)
					| Self::Cmp(reg) => reg.write_to(&mut out)?,
				Self::Load(reg1, reg2)
					| Self::Store(reg1, reg2)
					| Self::Mov(reg1, reg2)
					| Self::Add(reg1, reg2)
					| Self::Sub(reg1, reg2)
					| Self::Mul(reg1, reg2)
					| Self::Div(reg1, reg2)
					| Self::Mod(reg1, reg2)
					| Self::And(reg1, reg2)
					| Self::Or(reg1, reg2)
					| Self::Xor(reg1, reg2)
					| Self::Lsh(reg1, reg2)
					| Self::Rsh(reg1, reg2) => reg1.write_to(&mut out)? + reg2.write_to(&mut out)?,

				Self::PushBSx(sbyte) => sbyte.write_to(&mut out)?,
				Self::Jeq(offset)
					| Self::Jne(offset)
					| Self::Jlt(offset)
					| Self::Jle(offset)
					| Self::Jgt(offset)
					| Self::Jge(offset)
					| Self::Jmp(offset)
					| Self::Call(offset) => LittleEndian(offset).write_to(&mut out)?,
				Self::PushI(word)
					| Self::CallF(word) => LittleEndian(word).write_to(&mut out)?,

				Self::MovW(reg, word) => reg.write_to(&mut out)? + LittleEndian(word).write_to(&mut out)?,
				Self::MovBSx(reg, sbyte) => reg.write_to(&mut out)? + sbyte.write_to(&mut out)?,
				Self::Lfs(reg, byte) => reg.write_to(&mut out)? + byte.write_to(&mut out)?,
				Self::Int(it) => it.write_to(&mut out)?,
				Self::Ext(_) => panic!("'ext' is not currently supported"),
			})
	}
}

impl Display for Instruction {
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		use std::fmt::Write;
		let mut buf = String::new();

		match self {
			Self::Nop()              => write!(buf, "nop"),
			Self::Push(reg)          => write!(buf, "push {}", reg),
			Self::PushI(word)        => write!(buf, "pushi $0x{:08x}", word),
			Self::PushBSx(sbyte)     => write!(buf, "pushbsx $0x{:02x}", sbyte),
			Self::Pop(reg)           => write!(buf, "pop {}", reg),
			Self::Load(reg1, reg2)   => write!(buf, "load {}, {}", reg1, reg2),
			Self::Store(reg1, reg2)  => write!(buf, "store {}, {}", reg1, reg2),
			Self::Mov(reg1, reg2)    => write!(buf, "mov {}, {}", reg1, reg2),
			Self::MovW(reg, word)    => write!(buf, "movw {}, $0x{:08x}", reg, word),
			Self::MovBSx(reg, sbyte) => write!(buf, "movbsx {}, $0x{:02x}", reg, sbyte),
			Self::Dbg()              => write!(buf, "dbg"),
			Self::Int(int)           => write!(buf, "int {}", int),
			Self::Inc(reg)           => write!(buf, "inc {}", reg),
			Self::Dec(reg)           => write!(buf, "dec {}", reg),
			Self::Add(reg1, reg2)    => write!(buf, "add {}, {}", reg1, reg2),
			Self::Sub(reg1, reg2)    => write!(buf, "sub {}, {}", reg1, reg2),
			Self::Mul(reg1, reg2)    => write!(buf, "mul {}, {}", reg1, reg2),
			Self::Div(reg1, reg2)    => write!(buf, "div {}, {}", reg1, reg2),
			Self::Mod(reg1, reg2)    => write!(buf, "mod {}, {}", reg1, reg2),
			Self::And(reg1, reg2)    => write!(buf, "and {}, {}", reg1, reg2),
			Self::Or(reg1, reg2)     => write!(buf, "or {}, {}", reg1, reg2),
			Self::Xor(reg1, reg2)    => write!(buf, "xor {}, {}", reg1, reg2),
			Self::Lsh(reg1, reg2)    => write!(buf, "lsh {}, {}", reg1, reg2),
			Self::Rsh(reg1, reg2)    => write!(buf, "rsh {}, {}", reg1, reg2),
			Self::Neg(reg)           => write!(buf, "neg {}", reg),
			Self::Inv(reg)           => write!(buf, "inv {}", reg),
			Self::Not(reg)           => write!(buf, "not {}", reg),
			Self::Cmp(reg)           => write!(buf, "cmp {}", reg),
			Self::Jeq(offset)        => write!(buf, "jeq ${}", offset),
			Self::Jne(offset)        => write!(buf, "jne ${}", offset),
			Self::Jlt(offset)        => write!(buf, "jlt ${}", offset),
			Self::Jle(offset)        => write!(buf, "jle ${}", offset),
			Self::Jgt(offset)        => write!(buf, "jgt ${}", offset),
			Self::Jge(offset)        => write!(buf, "jge ${}", offset),
			Self::Jmp(offset)        => write!(buf, "jmp ${}", offset),
			Self::Call(offset)       => write!(buf, "call ${}", offset),
			Self::CallF(offset)      => write!(buf, "callf ${}", offset),
			Self::Ret()              => write!(buf, "ret"),
			Self::Lfs(reg, num)      => write!(buf, "lfs {}, ${}", reg, num),
			Self::Ext(_)             => unimplemented!("ext")
		}?;

		f.pad(&buf)
	}
}

macro_rules! write_word {
	($buf:expr, $fmt:literal, $u64:expr) => {{
		let word = $u64;
		write!($buf, concat!($fmt, $fmt, $fmt, $fmt, $fmt, $fmt, $fmt, $fmt),
			((word as u64) & 0xff_00_00_00_00_00_00_00) >> 0o70,
			((word as u64) & 0x00_ff_00_00_00_00_00_00) >> 0o60,
			((word as u64) & 0x00_00_ff_00_00_00_00_00) >> 0o50,
			((word as u64) & 0x00_00_00_ff_00_00_00_00) >> 0o40,
			((word as u64) & 0x00_00_00_00_ff_00_00_00) >> 0o30,
			((word as u64) & 0x00_00_00_00_00_ff_00_00) >> 0o20,
			((word as u64) & 0x00_00_00_00_00_00_ff_00) >> 0o10,
			((word as u64) & 0x00_00_00_00_00_00_00_ff) >> 0o00)
	}};
}

impl LowerHex for Instruction {
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		use std::fmt::Write;
		let mut buf = String::new();

		write!(buf, "{:02x}", OpCode::from(self).0)?;

		match self {
			Self::Nop() 
				| Self::Dbg()
				| Self::Ret() => {},
			Self::Push(reg)
				| Self::Pop(reg)
				| Self::Inc(reg)
				| Self::Dec(reg)
				| Self::Neg(reg)
				| Self::Inv(reg)
				| Self::Not(reg)
				| Self::Cmp(reg) => write!(buf, " {:02x}", reg)?,
			Self::Load(reg1, reg2)
				| Self::Store(reg1, reg2)
				| Self::Mov(reg1, reg2)
				| Self::Add(reg1, reg2)
				| Self::Sub(reg1, reg2)
				| Self::Mul(reg1, reg2)
				| Self::Div(reg1, reg2)
				| Self::Mod(reg1, reg2)
				| Self::And(reg1, reg2)
				| Self::Or(reg1, reg2)
				| Self::Xor(reg1, reg2)
				| Self::Lsh(reg1, reg2)
				| Self::Rsh(reg1, reg2) => write!(buf, " {:02x} {:02x}", reg1, reg2)?,

			Self::PushBSx(sbyte) => write!(buf, " {:02x}", sbyte)?,
			Self::Jeq(offset)
				| Self::Jne(offset)
				| Self::Jlt(offset)
				| Self::Jle(offset)
				| Self::Jgt(offset)
				| Self::Jge(offset)
				| Self::Jmp(offset)
				| Self::Call(offset) => write!(buf, " {:02x} {:02x}", (*offset as u16) >> 8, (*offset as u16) & 0xff)?,
			Self::PushI(word)
				| Self::CallF(word) => write_word!(buf, " {:02x}", *word)?,

			Self::MovW(reg, word) => { write!(buf, " {:02x}", reg)?; write_word!(buf, " {:02x}", *word)? },
			Self::MovBSx(reg, sbyte) => write!(buf, " {:02x} {:02x}", reg, sbyte)?,
			Self::Lfs(reg, byte) => write!(buf, " {:02x} {:02x}", reg, byte)?,
			Self::Int(int) => write!(buf, " {:x}", int)?,
			Self::Ext(_) => panic!("'ext' is not currently supported"),
		}

		f.pad(&buf)
	}
}

impl UpperHex for Instruction {
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		use std::fmt::Write;
		let mut buf = String::new();

		write!(buf, "{:02X}", OpCode::from(self).0)?;

		match self {
			Self::Nop() 
				| Self::Dbg()
				| Self::Ret() => {},
			Self::Push(reg)
				| Self::Pop(reg)
				| Self::Inc(reg)
				| Self::Dec(reg)
				| Self::Neg(reg)
				| Self::Inv(reg)
				| Self::Not(reg)
				| Self::Cmp(reg) => write!(buf, " {:02X}", reg)?,
			Self::Load(reg1, reg2)
				| Self::Store(reg1, reg2)
				| Self::Mov(reg1, reg2)
				| Self::Add(reg1, reg2)
				| Self::Sub(reg1, reg2)
				| Self::Mul(reg1, reg2)
				| Self::Div(reg1, reg2)
				| Self::Mod(reg1, reg2)
				| Self::And(reg1, reg2)
				| Self::Or(reg1, reg2)
				| Self::Xor(reg1, reg2)
				| Self::Lsh(reg1, reg2)
				| Self::Rsh(reg1, reg2) => write!(buf, " {:02X} {:02X}", reg1, reg2)?,

			Self::PushBSx(sbyte) => write!(buf, " {:02X}", sbyte)?,
			Self::Jeq(offset)
				| Self::Jne(offset)
				| Self::Jlt(offset)
				| Self::Jle(offset)
				| Self::Jgt(offset)
				| Self::Jge(offset)
				| Self::Jmp(offset)
				| Self::Call(offset) => write!(buf, " {:02X} {:02X}", (*offset as u16) >> 8, (*offset as u16) & 0xff)?,
			Self::PushI(word)
				| Self::CallF(word) => write_word!(buf, " {:02X}", *word)?,

			Self::MovW(reg, word) => { write!(buf, " {:02X}", reg)?; write_word!(buf, " {:02X}", *word)? },
			Self::MovBSx(reg, sbyte) => write!(buf, " {:02X} {:02X}", reg, sbyte)?,
			Self::Lfs(reg, byte) => write!(buf, " {:02X} {:02X}", reg, byte)?,
			Self::Int(int) => write!(buf, " {:X}", int)?,
			Self::Ext(_) => panic!("'ext' is not currently supported"),
		}

		f.pad(&buf)
	}
}

impl Binary for Instruction {
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		use std::fmt::Write;
		let mut buf = String::new();

		write!(buf, "{:08b}", OpCode::from(self).0)?;

		match self {
			Self::Nop() 
				| Self::Dbg()
				| Self::Ret() => {},
			Self::Push(reg)
				| Self::Pop(reg)
				| Self::Inc(reg)
				| Self::Dec(reg)
				| Self::Neg(reg)
				| Self::Inv(reg)
				| Self::Not(reg)
				| Self::Cmp(reg) => write!(buf, " {:08b}", reg)?,
			Self::Load(reg1, reg2)
				| Self::Store(reg1, reg2)
				| Self::Mov(reg1, reg2)
				| Self::Add(reg1, reg2)
				| Self::Sub(reg1, reg2)
				| Self::Mul(reg1, reg2)
				| Self::Div(reg1, reg2)
				| Self::Mod(reg1, reg2)
				| Self::And(reg1, reg2)
				| Self::Or(reg1, reg2)
				| Self::Xor(reg1, reg2)
				| Self::Lsh(reg1, reg2)
				| Self::Rsh(reg1, reg2) => write!(buf, " {:08b} {:08b}", reg1, reg2)?,

			Self::PushBSx(sbyte) => write!(buf, " {:08b}", sbyte)?,
			Self::Jeq(offset)
				| Self::Jne(offset)
				| Self::Jlt(offset)
				| Self::Jle(offset)
				| Self::Jgt(offset)
				| Self::Jge(offset)
				| Self::Jmp(offset)
				| Self::Call(offset) => write!(buf, " {:08b} {:08b}", (*offset as u16) >> 8, (*offset as u16) & 0xff)?,
			Self::PushI(word)
				| Self::CallF(word) => write_word!(buf, " {:08b}", *word)?,

			Self::MovW(reg, word) => { write!(buf, " {:08b}", reg)?; write_word!(buf, " {:08b}", *word)? },
			Self::MovBSx(reg, sbyte) => write!(buf, " {:08b} {:08b}", reg, sbyte)?,
			Self::Lfs(reg, byte) => write!(buf, " {:08b} {:08b}", reg, byte)?,
			Self::Int(int) => write!(buf, " {:b}", int)?,
			Self::Ext(_) => panic!("'ext' is not currently supported"),
		}

		f.pad(&buf)
	}
}
