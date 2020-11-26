use sjef::{File, Data, Instruction};
use sjef::instruction::{Reg, Interrupt};
use crate::{Word, uw, Text, Stack, Registers};
use crate::{RegisterTrait, HeapTrait, PointerTrait};
use read_from::{ReadFrom, WriteTo};

#[derive(Debug)]
#[non_exhaustive]
pub struct SojournVm<
	R: RegisterTrait=crate::Register,
	H: HeapTrait=crate::Heap
> {
	pub text: Text,
	pub stack: Stack,
	pub data: Data,
	pub registers: Registers<R>,
	pub heap: H,
	flags: u8 // TODO: Flags
}

const FLAG_EQL: u8 = 0b001;
const FLAG_LTH: u8 = 0b010;
const FLAG_GTH: u8 = 0b100;

impl<R: RegisterTrait, H: HeapTrait> SojournVm<R, H> {
	pub fn new(file: File) -> Self {
		let mut text = Vec::new();
		file.text.write_to(&mut text).expect("writes to vecs cannot file");

		Self {
			stack: Stack::new(),
			registers: Registers::new(),
			data: file.data,
			text: Text::new(text),
			heap: H::new(),
			flags: 0
		}
	}

	pub fn step(&mut self) -> Result<(), <Instruction as ReadFrom>::Error> {
		let instr = Instruction::read_from(&mut self.text)?;
		self.execute(instr);
		Ok(())
	}

	pub fn run(&mut self) {
		while !self.text.is_eof() {
			self.step().expect("couldn't read instruction!");
		}
	}
}

/// Bytecode commands.
impl<R: RegisterTrait, H: HeapTrait> SojournVm<R, H> {
	fn op_nop(&mut self) {
		debug!("Performed 'nop'. Whee!");
	}

	fn op_push(&mut self, reg: Reg) {
		let value = self.registers[reg].load();
		self.stack.push(value);

		debug!("Performed 'push'. {}={:016x}", reg, value);
	}

	fn op_pushw(&mut self, word: Word) {
		self.stack.push(word);

		debug!("Performed 'pushw'. word={:016x} ({0})", word);
	}

	fn op_pushbsx(&mut self, sbyte: i8) {
		self.stack.push(sbyte as i64);

		debug!("Performed 'pushbsx'. sbyte={:02x} ({0})", sbyte);
	}

	fn op_pop(&mut self, dst: Reg) {
		self.registers[dst].store(self.stack.pop().expect("popped from an empty stack!"));

		debug!("Performed 'pop'. {}={:016x}", dst, self.registers[dst]);
	}

	fn op_load(&mut self, dst: Reg, src: Reg) {
		let ptr = H::Pointer::from_uword(uw(self.registers[dst].load()));

		// SAFETY: Like every other opcode, we have no way to guarantee this is safe. The compiler must do that.
		self.registers[src].store(unsafe { self.heap.get_word(ptr) });

		debug!("Performed 'load'. {}={:016p} -> {}={:016x}", dst, self.registers[dst], src, self.registers[src]);
	}

	fn op_store(&mut self, dst: Reg, src: Reg) {
		let ptr = H::Pointer::from_uword(uw(self.registers[dst].load()));
		let value = self.registers[src].load();

		// SAFETY: Like every other opcode, we have no way to guarantee this is safe. The compiler must do that.
		unsafe {
			self.heap.set_word(ptr, value);
		}

		debug!("Performed 'store'. {}={:016p} {}={:016x}", dst, self.registers[dst], src, self.registers[src]);
	}

	fn op_storew(&mut self, dst: Reg, word: Word) {
		let ptr = H::Pointer::from_uword(uw(self.registers[dst].load()));

		// SAFETY: Like every other opcode, we have no way to guarantee this is safe. The compiler must do that.
		unsafe {
			self.heap.set_word(ptr, word);
		}

		debug!("Performed 'store'. {}={:016p} -> {1:p}={:016x}", dst, self.registers[dst], word);
	}

	fn op_storeb(&mut self, dst: Reg, byte: u8) {
		let ptr = H::Pointer::from_uword(uw(self.registers[dst].load()));

		// SAFETY: Like every other opcode, we have no way to guarantee this is safe. The compiler must do that.
		unsafe {
			self.heap.set_byte(ptr, byte);
		}

		debug!("Performed 'store'. {}={:016p} -> {1:p}={:02x}", dst, self.registers[dst], byte);
	}


	fn op_mov(&mut self, dst: Reg, src: Reg) {
		if dst != src {
			let value = self.registers[src].load();
			self.registers[dst].store(value);
		}

		debug!("Performed 'mov'. {}={:016x} -> {}={:016x}", src, self.registers[src], dst, self.registers[dst]);
	}

	fn op_movw(&mut self, dst: Reg, word: Word) {
		self.registers[dst].store(word);

		debug!("Performed 'movw'. {}={:016x}", dst, self.registers[dst]);
	}

	fn op_movbsx(&mut self, dst: Reg, sbyte: i8) {
		self.registers[dst].store(sbyte as i64);

		debug!("Performed 'movwbsx'. {}={:016x}", dst, self.registers[dst]);
	}

	fn op_dbg(&mut self) {
		// println!("{:#?}", self);

		unsafe {
			let ptr = H::Pointer::from_uword(uw(self.registers[Reg::RETURN_REG].load()));

			let len = self.heap.get_word(ptr) as usize;
			let ptr = ptr.offset(std::mem::size_of::<Word>() as isize);
			let mut string = String::with_capacity(len as usize);
			for i in 0..len {
				string.push(self.heap.get_byte(ptr.offset(i as isize)) as char);
			}

			print!("{}", string);
		}

		debug!("Performed 'dbg'.");
	}

	fn op_int(&mut self, int: Interrupt) {

		match int {
			Interrupt::Exit(code) => {
				// note we write the debug message _before_ exit here, as we need it to be printed out before we exit.
				debug!("Performed 'int' ('exit'). code={}", code);
				std::process::exit(code);
			},
			Interrupt::Malloc(reg, size) => {
				let ptr = self.heap.malloc(size as usize);

				self.registers[reg].store(ptr.into_uword() as Word);

				debug!("Performed 'int' ('malloc'). size={} -> {}={:016p}", size, reg, self.registers[reg]);
			},
			Interrupt::Free(reg) => {
				let ptr = H::Pointer::from_uword(uw(self.registers[reg].load()));

				// SAFETY: Lol, we have to hope that whoever called `Free` knows that
				// they're doing. We have no safety guarantees in release mode. 
				unsafe {
					self.heap.free(ptr);
				}

				debug!("Performed 'int' ('free'). {}={:016p}", reg, self.registers[reg]);
			},
			Interrupt::Realloc(reg, size) => {
				let old_reg = self.registers[reg];
				let ptr = H::Pointer::from_uword(uw(old_reg.load()));

				// SAFETY: Lol, we have to hope that whoever called `Realloc` knows that
				// they're doing. We have no safety guarantees in release mode. 
				self.registers[reg].store(unsafe { self.heap.realloc(ptr, size as usize) }.into_uword() as Word);

				debug!("Performed 'int' ('realloc'). {}={:016p}, new_size={} -> {0}={:016p}", reg, old_reg, size,
					self.registers[reg]);
			},
			other => unimplemented!("interrupt: {:?}", other)
		};
	}

	fn op_inc(&mut self, reg: Reg) {
		let value = self.registers[reg].load();
		self.registers[reg].store(value + 1);

		debug!("Performed 'inc'. {}={:016x} -> {0}={:016x}", reg, value, self.registers[reg]);
	}

	fn op_dec(&mut self, reg: Reg) {
		let value = self.registers[reg].load() ;
		self.registers[reg].store(value - 1);

		debug!("Performed 'dec'. {}={:016x} -> {0}={:016x}", reg, value, self.registers[reg]);
	}

	fn op_add(&mut self, reg1: Reg, reg2: Reg) {
		let lhs = self.registers[reg1];
		let rhs = self.registers[reg2];
		self.registers[reg1] += rhs;

		debug!("Performed 'add'. {}={:016x} {}={:016x} -> {0}={:016x}", reg1, lhs, reg2, rhs, self.registers[reg1]);
	}

	fn op_sub(&mut self, reg1: Reg, reg2: Reg) {
		let lhs = self.registers[reg1];
		let rhs = self.registers[reg2];
		self.registers[reg1] -= rhs;

		debug!("Performed 'sub'. {}={:016x} {}={:016x} -> {0}={:016x}", reg1, lhs, reg2, rhs, self.registers[reg1]);
	}

	fn op_mul(&mut self, reg1: Reg, reg2: Reg) {
		let lhs = self.registers[reg1];
		let rhs = self.registers[reg2];
		self.registers[reg1] *= rhs;

		debug!("Performed 'mul'. {}={:016x} {}={:016x} -> {0}={:016x}", reg1, lhs, reg2, rhs, self.registers[reg1]);
	}

	fn op_div(&mut self, reg1: Reg, reg2: Reg) {
		let lhs = self.registers[reg1];
		let rhs = self.registers[reg2];
		self.registers[reg1] /= rhs;

		debug!("Performed 'div'. {}={:016x} {}={:016x} -> {0}={:016x}", reg1, lhs, reg2, rhs, self.registers[reg1]);
	}

	fn op_mod(&mut self, reg1: Reg, reg2: Reg) {
		let lhs = self.registers[reg1];
		let rhs = self.registers[reg2];
		self.registers[reg1] %= rhs;

		debug!("Performed 'mod'. {}={:016x} {}={:016x} -> {0}={:016x}", reg1, lhs, reg2, rhs, self.registers[reg1]);
	}

	fn op_and(&mut self, reg1: Reg, reg2: Reg) {
		let lhs = self.registers[reg1];
		let rhs = self.registers[reg2];
		self.registers[reg1] &= rhs;

		debug!("Performed 'and'. {}={:016x} {}={:016x} -> {0}={:016x}", reg1, lhs, reg2, rhs, self.registers[reg1]);
	}

	fn op_or(&mut self, reg1: Reg, reg2: Reg) {
		let lhs = self.registers[reg1];
		let rhs = self.registers[reg2];
		self.registers[reg1] |= rhs;

		debug!("Performed 'or'. {}={:016x} {}={:016x} -> {0}={:016x}", reg1, lhs, reg2, rhs, self.registers[reg1]);
	}

	fn op_xor(&mut self, reg1: Reg, reg2: Reg) {
		let lhs = self.registers[reg1];
		let rhs = self.registers[reg2];
		self.registers[reg1] ^= rhs;

		debug!("Performed 'xor'. {}={:016x} {}={:016x} -> {0}={:016x}", reg1, lhs, reg2, rhs, self.registers[reg1]);
	}

	fn op_lsh(&mut self, reg1: Reg, reg2: Reg) {
		let lhs = self.registers[reg1];
		let rhs = self.registers[reg2];
		self.registers[reg1] <<= rhs;

		debug!("Performed 'lsh'. {}={:016x} {}={:016x} -> {0}={:016x}", reg1, lhs, reg2, rhs, self.registers[reg1]);
	}

	fn op_rsh(&mut self, reg1: Reg, reg2: Reg) {
		let lhs = self.registers[reg1];
		let rhs = self.registers[reg2];
		self.registers[reg1] >>= rhs;

		debug!("Performed 'rsh'. {}={:016x} {}={:016x} -> {0}={:016x}", reg1, lhs, reg2, rhs, self.registers[reg1]);
	}

	fn op_neg(&mut self, reg: Reg) {
		let value = self.registers[reg];
		self.registers[reg].neg_assign();

		debug!("Performed 'neg'. {}={:016x} -> {0}={:016x}", reg, value, self.registers[reg]);
	}

	fn op_inv(&mut self, reg: Reg) {
		let value = self.registers[reg];
		self.registers[reg].inv_assign();

		debug!("Performed 'inv'. {}={:016x} -> {0}={:016x}", reg, value, self.registers[reg]);
	}

	fn op_not(&mut self, reg: Reg) {
		let value = self.registers[reg];
		self.registers[reg].not_assign();

		debug!("Performed 'not'. {}={:016x} -> {0}={:016x}", reg, value, self.registers[reg]);
	}

	fn op_cmp(&mut self, reg1: Reg, reg2: Reg) {
		let lhs = self.registers[reg1];
		let rhs = self.registers[reg2];
		let cmp = lhs.cmp(&rhs);

		self.flags &= !(FLAG_EQL | FLAG_GTH | FLAG_LTH);

		match cmp {
			std::cmp::Ordering::Less => self.flags |= FLAG_LTH,
			std::cmp::Ordering::Equal => self.flags |= FLAG_EQL,
			std::cmp::Ordering::Greater => self.flags |= FLAG_GTH,
		}

		debug!("Performed 'cmp'. {}={:016x} {}={:016x} -> FLAGS={:03b}", reg1, lhs, reg2, rhs, self.flags);
	}

	fn jump_by(&mut self, offset: isize) {
		let ip = self.text.ip();

		self.text.set_ip((ip as isize + offset) as u64);
	}

	fn op_jeq(&mut self, offset: i16) {
		if (self.flags & FLAG_EQL) != 0 {
			self.jump_by(offset as isize);
			debug!("Performed 'jeq', and jumped. FLAGS={:03b} offset={:04x} -> ip=0x{:016x}", self.flags,
				offset, self.text.ip());
		} else {
			debug!("Performed 'jeq', and didn't jump. FLAGS={:03b} offset={:04x}", self.flags, offset);
		}
	}

	fn op_jne(&mut self, offset: i16) {
		if (self.flags & FLAG_EQL) == 0 {
			self.jump_by(offset as isize);
			debug!("Performed 'jne', and jumped. FLAGS={:03b} offset={:04x} -> ip=0x{:016x}", self.flags,
				offset, self.text.ip());
		} else {
			debug!("Performed 'jne', and didn't jump. FLAGS={:03b} offset={:04x}", self.flags, offset);
		}
	}

	fn op_jlt(&mut self, offset: i16) {
		if (self.flags & FLAG_LTH) != 0 {
			self.jump_by(offset as isize);
			debug!("Performed 'jlt', and jumped. FLAGS={:03b} offset={:04x} -> ip=0x{:016x}", self.flags,
				offset, self.text.ip());
		} else {
			debug!("Performed 'jlt', and didn't jump. FLAGS={:03b} offset={:04x}", self.flags, offset);
		}
	}

	fn op_jle(&mut self, offset: i16) {
		if (self.flags & FLAG_GTH) == 0 {
			self.jump_by(offset as isize);
			debug!("Performed 'jle', and jumped. FLAGS={:03b} offset={:04x} -> ip=0x{:016x}", self.flags,
				offset, self.text.ip());
		} else {
			debug!("Performed 'jle', and didn't jump. FLAGS={:03b} offset={:04x}", self.flags, offset);
		}
	}

	fn op_jgt(&mut self, offset: i16) {
		if (self.flags & FLAG_GTH) != 0 {
			self.jump_by(offset as isize);
			debug!("Performed 'jgt', and jumped. FLAGS={:03b} offset={:04x} -> ip=0x{:016x}", self.flags,
				offset, self.text.ip());
		} else {
			debug!("Performed 'jgt', and didn't jump. FLAGS={:03b} offset={:04x}", self.flags, offset);
		}
	}

	fn op_jge(&mut self, offset: i16) {
		if (self.flags & FLAG_LTH) == 0 {
			self.jump_by(offset as isize);
			debug!("Performed 'jge', and jumped. FLAGS={:03b} offset={:04x} -> ip=0x{:016x}", self.flags,
				offset, self.text.ip());
		} else {
			debug!("Performed 'jge', and didn't jump. FLAGS={:03b} offset={:04x}", self.flags, offset);
		}
	}

	fn op_jmp(&mut self, offset: i16) {
		self.jump_by(offset as isize);

		debug!("Performed 'jmp', and jumped. offset={:04x} -> ip=0x{:016x}", offset, self.text.ip());
	}

	fn op_call(&mut self, offset: i16) {
		let old_ip = self.text.ip();

		self.stack.push(old_ip as Word);
		self.jump_by(offset as isize);

		debug!("Performed 'call'. old_ip=0x{:016x} offset={:04x} -> ip=0x{:016x}", old_ip, offset, self.text.ip());
	}

	fn op_callf(&mut self, offset: Word) {
		let old_ip = self.text.ip();

		self.stack.push(old_ip as Word);
		self.jump_by(offset as isize);

		debug!("Performed 'callf'. old_ip=0x{:016x} offset={:016x} -> ip=0x{:016x}", old_ip, offset, self.text.ip());
	}

	fn op_ret(&mut self) {
		let dst = self.stack.pop().expect("returned from an empty stack?");

		self.text.set_ip(uw(dst));

		debug!("Performed 'ret'. ip={:016x}", self.text.ip());
	}

	fn op_lfs(&mut self, reg: Reg, which: u8) {
		self.registers[reg].store(self.stack.as_ref()[self.stack.as_ref().len() - 2 + which as usize]);

		debug!("Performed 'lfs'. which={} -> {}={:016x}", which, reg, self.registers[reg]);
	}
}

impl<R: RegisterTrait, H: HeapTrait> SojournVm<R, H> {
	#[instrument(level = "debug", skip(self, instr), fields(start_ip=%format!("{:08x}", self.text.ip()), %instr))]
	pub fn execute(&mut self, instr: Instruction) {
		trace!(?instr, ip=%format!("{:08x}", self.text.ip()), "starting execution");

		match instr {
			Instruction::Nop() => self.op_nop(),
			Instruction::Push(reg) => self.op_push(reg),
			Instruction::PushW(word) => self.op_pushw(word),
			Instruction::PushBSx(sbyte) => self.op_pushbsx(sbyte),
			Instruction::Pop(reg) => self.op_pop(reg),
			Instruction::Load(reg1, reg2) => self.op_load(reg1, reg2),
			Instruction::Store(reg1, reg2) => self.op_store(reg1, reg2),
			Instruction::StoreW(reg, word) => self.op_storew(reg, word),
			Instruction::StoreB(reg, byte) => self.op_storeb(reg, byte),
			Instruction::Mov(dst, src) => self.op_mov(dst, src),
			Instruction::MovW(reg, word) => self.op_movw(reg, word),
			Instruction::MovBSx(reg, sbyte) => self.op_movbsx(reg, sbyte),
			Instruction::Dbg() => self.op_dbg(),
			Instruction::Int(int) => self.op_int(int),

			Instruction::Inc(reg) => self.op_inc(reg),
			Instruction::Dec(reg) => self.op_dec(reg),
			Instruction::Add(reg1, reg2) => self.op_add(reg1, reg2),
			Instruction::Sub(reg1, reg2) => self.op_sub(reg1, reg2),
			Instruction::Mul(reg1, reg2) => self.op_mul(reg1, reg2),
			Instruction::Div(reg1, reg2) => self.op_div(reg1, reg2),
			Instruction::Mod(reg1, reg2) => self.op_mod(reg1, reg2),
			Instruction::And(reg1, reg2) => self.op_and(reg1, reg2),
			Instruction::Or(reg1, reg2) => self.op_or(reg1, reg2),
			Instruction::Xor(reg1, reg2) => self.op_xor(reg1, reg2),
			Instruction::Lsh(reg1, reg2) => self.op_lsh(reg1, reg2),
			Instruction::Rsh(reg1, reg2) => self.op_rsh(reg1, reg2),
			Instruction::Neg(reg) => self.op_neg(reg),
			Instruction::Inv(reg) => self.op_inv(reg),
			Instruction::Not(reg) => self.op_not(reg),

			Instruction::Cmp(lhs, rhs) => self.op_cmp(lhs, rhs),
			Instruction::Jeq(offset) => self.op_jeq(offset - (instr.byte_len() as i16)),
			Instruction::Jne(offset) => self.op_jne(offset - (instr.byte_len() as i16)),
			Instruction::Jlt(offset) => self.op_jlt(offset - (instr.byte_len() as i16)),
			Instruction::Jle(offset) => self.op_jle(offset - (instr.byte_len() as i16)),
			Instruction::Jgt(offset) => self.op_jgt(offset - (instr.byte_len() as i16)),
			Instruction::Jge(offset) => self.op_jge(offset - (instr.byte_len() as i16)),
			Instruction::Jmp(offset) => self.op_jmp(offset - (instr.byte_len() as i16)),
			Instruction::Call(offset) => self.op_call(offset - (instr.byte_len() as i16)),
			Instruction::CallF(offset) => self.op_callf(offset - (instr.byte_len() as i64	)),
			Instruction::Ret() => self.op_ret(),
			Instruction::Lfs(reg, num) => self.op_lfs(reg, num),

			Instruction::Ext(_) => todo!(),
		};

		trace!(?instr, ip=%format!("{:08x}", self.text.ip()), "finished execution");
	}
}


