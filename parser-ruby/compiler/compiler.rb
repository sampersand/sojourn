require 'stringio'
require_relative 'bytecode'

class Scope
	attr_reader :registers, :rreg

	def initialize
		@registers = [] 
		@rreg = "a"
	end
end

class Label
	def initialize(compiler)
		@compiler = compiler
		@emitted = false
		@binding = nil
	end

	def inspect; "Label(emitted=#@emitted, binding=#{@binding.inspect})" end
	def to_s; '<label>' end

	def emit
		fail 'already emitted!' if @emitted
		@emitted = true
		@compiler.instance_variable_get(:@bytecode).push self
		self
	end

	def bound?; @bound end
	def bind!(location) raise 'already bound' if @binding; @binding = location end

	def to_i; @binding end
end

class Set
	def pop_any
		mem = each.first or return
		delete mem or fail '<this should exist, compiler error>'
		mem
	end
end

class Integer
	def byte; (0..0xff).include?(self) ? self : raise("not a byte: #{self}") end
	def word; [self].pack('q<').bytes end
	def short; [self].pack('s<').bytes end
end

class Compiler
	POSSIBLE_REGISTERS = 255.downto(1).to_a.freeze
	FN_RET_REG = 0

	def initialize(&block)
		@available_registers = POSSIBLE_REGISTERS.dup
		@scopes = [Scope.new]
		@bytecode = []
		@data = []
		@function_scopes = [[@scopes.last, {}]]
		instance_exec(&block) if block
	end

	def compile(output=StringIO.new(''))
		text = compile_text
		data = ''

		@data.each do |datum|
			case datum
			when String 
				if datum.length < 255
					data.concat [datum.length, datum].pack('CA*')
				else
					data.concat [255, datum.length, datum].pack('CL<A*')
				end
			else raise "unknown datum type: #{datum.class}"
			end
		end

		output.write ['sojourn1', text.length, data.length + 8, text, @data.length, data].pack 'A*Q<Q<A*L<A*'

		output
	end

	def compile_text
		output = @bytecode.dup
		nonbytes = output.reject { _1.is_a?(Integer) || _1.is_a?(Label) }
		nonbytes.each do
			output.insert(output.index(_1)+1, *[nil]*2)
		end

		while (lbl_idx = output.index { _1.is_a? Label })
			output[lbl_idx].bind! lbl_idx
			output.delete(output[lbl_idx])
		end


		while (jmp_or_call_idx = output.index { _1 && !_1.is_a?(Integer) })
			output[jmp_or_call_idx..jmp_or_call_idx+2] = *output[jmp_or_call_idx].emit(position: jmp_or_call_idx).bytes
		end

		output.pack 'C*'
	end

	NearOrFarJump = Struct.new :near, :far, :label do
		alias == equal?
		alias eql? ==
		def inspect; "NearOrFarJump(#{label})" end
		def emit_near(offset) [near, offset].pack 'Cs<' end
		def emit_far(offset) [near, offset].pack 'Cs<' end
		def emit(position:)
			offset = label.to_i - position
			if (-127..128).include?(offset)
				emit_near offset
			else
				emit_far offset
			end
		end
	end

	NearOrFarCall = Struct.new :near, :far, :label do
		alias == equal?
		alias eql? ==

		def inspect; "NearOrFarCall(#{label})" end
		def emit(position:)
			offset = label.to_i - position
			if (-32768..32767).include?(offset)
				[near, offset].pack 'Cs<'
			else
				[near, offset].pack 'Cs<'
			end
		end
	end

	def new_reg(scope=@scopes.last)
		reg = @available_registers.pop or raise 'all registers in use'
		scope.registers << reg
		reg
	end

	def free_reg(reg)
		@available_registers.push reg or raise 'register not in use!'
	end

	def new_label
		Label.new self
	end

	def new_data(type, value)
		raise "unknown data type #{type}" unless type == :string
		@data.index(value) || @data.push(value).length - 1
	end

	def bytepos
		@bytecode.length
	end

	def rreg
		@scopes.last.rreg
	end

	def emit(*bytes)
		bytes.find { !(0..0xff).include?(_1) &&
				!_1.is_a?(NearOrFarJump) && !_1.is_a?(NearOrFarCall) \
				&& !_1.is_a?(Bytecode) }
			&.tap { raise "nonbyte value given: #{_1}" }
		@bytecode.concat bytes
		self
	end

	def switch_scope(new_scope, &block)
		if new_scope
			with_scope(&block)
		else
			block.call
		end
	end

	def with_function_scope(&block)
		with_scope do
			@function_scopes.push [@scopes.last, {}]
			yield
		end
	ensure
		@function_scopes.pop
	end

	def variable(variable)
		@function_scopes.last.last.fetch variable
	rescue
		pp @function_scopes
		raise
	end

	def set_variable(variable)
		scope, vars = @function_scopes.last
		vars[variable] ||= new_reg(scope)
	end

	def fetch_function(name)
		@function_scopes.reverse.each do |_, vars|
			vars.include? name and return vars[name]
		end
		@function_scopes
		raise "couldnt find function: #{name}"
	end

	def with_scope(rreg=nil)
		@scopes.push Scope.new
		@scopes.last.instance_variable_set :@rreg, (rreg || new_reg)
		yield
		@scopes.last.rreg
	ensure
		(@scopes.pop || raise('popped a scope when none are left!'))
			.registers
			.each { free_reg _1 }
	end

	def nop; emit Bytecode::NOP.to_i end
	def push(reg) emit Bytecode::PUSH.to_i, reg.byte end
	def pushi(reg) emit Bytecode::PUSHI.to_i, *reg.word end
	def pop(reg) emit Bytecode::POP.to_i, reg.byte end
	def load(dst, reg) emit Bytecode::LOAD.to_i, dst.byte, reg.byte end
	def store(dst, reg) emit Bytecode::STORE.to_i, dst.byte, reg.byte end
	def mov(dst, src) emit Bytecode::MOV.to_i, dst.byte, src.byte end
	def movw(dst, int) emit Bytecode::MOVW.to_i, dst.byte, *int.word end
	def movbsx(dst, byte) emit Bytecode::MOVBSX.to_i, dst.byte, byte.byte end
	def debug; emit Bytecode::DEBUG.to_i end
	def trap(what, *data) emit Bytecode::TRAP.to_i, what.byte, *data end

	def add(dst, src) emit Bytecode::ADD.to_i, dst.byte, src.byte end
	def sub(dst, src) emit Bytecode::SUB.to_i, dst.byte, src.byte end
	def mul(dst, src) emit Bytecode::MUL.to_i, dst.byte, src.byte end
	def div(dst, src) emit Bytecode::DIV.to_i, dst.byte, src.byte end
	def mod(dst, src) emit Bytecode::MOD.to_i, dst.byte, src.byte end
	def and_(dst, src) emit Bytecode::AND.to_i, dst.byte, src.byte end
	def or_(dst, src) emit Bytecode::OR.to_i, dst.byte, src.byte end
	def xor(dst, src) emit Bytecode::XOR.to_i, dst.byte, src.byte end
	def lsh(dst, src) emit Bytecode::LSH.to_i, dst.byte, src.byte end
	def rsh(dst, src) emit Bytecode::RSH.to_i, dst.byte, src.byte end
	def neg(dst) emit Bytecode::NEG.to_i, dst.byte end
	def not_(dst) emit Bytecode::NOT.to_i, dst.byte end
	def inv(dst) emit Bytecode::INV.to_i, dst.byte end

	def cmp(lhs, rhs) emit Bytecode::CMP.to_i, lhs.byte, rhs.byte end
	def jeq(lbl) emit NearOrFarJump.new(Bytecode::JEQ.to_i, nil, lbl) end
	def jne(lbl) emit NearOrFarJump.new(Bytecode::JNE.to_i, nil, lbl) end
	def jlt(lbl) emit NearOrFarJump.new(Bytecode::JLT.to_i, nil, lbl) end
	def jle(lbl) emit NearOrFarJump.new(Bytecode::JLE.to_i, nil, lbl) end
	def jgt(lbl) emit NearOrFarJump.new(Bytecode::JGT.to_i, nil, lbl) end
	def jge(lbl) emit NearOrFarJump.new(Bytecode::JGE.to_i, nil, lbl) end
	def jmp(lbl) emit NearOrFarJump.new(Bytecode::JMP.to_i, nil, lbl) end

	def inc(reg) emit Bytecode::INC.to_i, reg.byte end
	def dec(reg) emit Bytecode::DEC.to_i, reg.byte end
	def call(lbl) emit NearOrFarCall.new(Bytecode::CALLS.to_i, Bytecode::CALLW.to_i, lbl) end
	def ret; emit Bytecode::RET.to_i end
	def lfs(reg, which); emit Bytecode::LFS.to_i, reg.byte, which.byte end
end

class Compiler
	class Block
		def initialize(&proc) @proc = proc end
		def emit(compiler)
			compiler.with_scope do
				compiler.mov compiler.rreg, compiler.instance_eval(&@proc)
			end
		end
	end

	def block(...)
		Block.new(...)
	end

	def emit_return(value)
		mov FN_RET_REG, value.emit(self)
		ret
	end

	def emit_string(string)
		emit_integer new_data(:string, string)
	end

	def emit_boolean(bool)
		emit_integer(bool ? 1 : 0)
	end

	def emit_variable(name)
		with_scope do
			mov rreg, variable(name)
		end
	end

	def emit_assignment(name, value)
		with_scope do

			mov set_variable(name), value.emit(self)
		end
	end

	def emit_integer(int, new_scope=true)
		switch_scope new_scope do
			if (0..255).include? int
				movbsx rreg, int
			else
				movw rreg, int
			end
		end
	end

	def emit_null
		emit_integer 0 # TODO: null?
	end

	def emit_binary_op(op, lhs, rhs)
		with_scope do
			temp = new_reg
			temp2 = new_reg
			mov temp, lhs.emit(self)
			mov temp2, rhs.emit(self)
			mov rreg, temp
			case op
			when '+'  then add rreg, temp2
			when '-'  then sub rreg, temp2
			when '*'  then mul rreg, temp2
			when '/'  then div rreg, temp2
			when '%'  then mod rreg, temp2
			when '&'  then and_ rreg, temp2
			when '|'  then or_ rreg, temp2
			when '^'  then xor rreg, temp2
			when '<<' then lsh rreg, temp2
			when '>>' then rsh rreg, temp2
			else raise "unknown binary op '#{op}"
			end
		end
	end

	def emit_if(cond, if_true, if_false)
		if_false_lbl = new_label
		end_if_lbl = new_label

		with_scope do
			tmp = new_reg
			mov tmp, 0
			cmp cond.emit(self), tmp
			jeq if_false_lbl

			mov rreg, if_true.emit(self)
			jmp end_if_lbl

			if_false_lbl.emit
			mov rreg, if_false.emit(self)

			end_if_lbl.emit
		end
	end

	def emit_while(cond, body)
		start_lbl = new_label
		end_lbl = new_label
		tmp = new_reg
		mov tmp, 0

		with_scope do
			mov rreg, emit_null

			start_lbl.emit
			cmp cond.emit(self), tmp
			jeq end_lbl
			mov rreg, body.emit(self)
			jmp start_lbl
			end_lbl.emit
		end
	end

	def emit_function(name, *args, body, label: new_label)
		endfunc = new_label
		@function_scopes.last[1][name] = label
		jmp endfunc
		with_function_scope do
			label.emit

			args.each_with_index do |name, idx|
				lfs set_variable(name), idx
			end

			emit_return body
		end
		endfunc.emit

		label
	end

	def emit_call_function(fnlabel, *argregs)
		with_scope do
			argregs.each { push _1.emit(self) }
			call(fnlabel)
			mov rreg, FN_RET_REG
			argregs.each { with_scope { pop rreg } }
		end
	end
end

__END__

# compiler = Compiler.new do
# 	addlbl = new_label

# 	lhs = new_reg
# 	rhs = new_reg
# 	movbsx lhs, 2
# 	movbsx rhs, 3
# 	emit_call_function addlbl, lhs, rhs

# 	push Compiler::FN_RET_REG
# 	trap 0, 0x00, 0x00

# 	emit_function(2, label: addlbl) do |lhs, rhs|
# 		add lhs, rhs
# 		mov rreg, lhs
# 	end
# end
compiler = Compiler.new do
	lbl = new_label
	jmp lbl
	nop
	lbl.emit
	debug

	# movbsx variable('foo'), 12
	# movbsx rreg, new_data(:string, 'hello')
	# trap 1, rreg

	# movbsx rreg, new_data(:string, ' ')
	# trap 1, rreg

	# movbsx rreg, new_data(:string, 'world!')
	# trap 1, rreg

	# movbsx rreg, new_data(:string, ' ')
	# trap 1, rreg

	# movbsx rreg, new_data(:string, 'yay!')
	# trap 1, rreg

	# movbsx rreg, new_data(:string, "\n")
	# trap 1, rreg
end

open('test.sjc', 'wb'){ compiler.compile _1 }


require_relative 'disassembler'
disassemble(open('test.sjc', 'rb', &:read), $>)

__END__

void Compiler::emit(VarStatement* v, std::vector<u8>& bytecode) {
    const uint8_t result = emitExpression(v->value, bytecode);
    endReg(result);
    const uint8_t loc = newReg();
    v->loc = loc;
    scopes.back().add(v);
    bytecode.push_back(MOVREGREG_OP);
    bytecode.push_back(loc);
    bytecode.push_back(result);
}

what is emitExpression you might ask?
uint8_t Compiler::emitExpression(Expr* e, std::vector<u8>& bytecode) {
    if (auto lit = dynamic_cast<NumberLiteral*>(e)) {
        const uint8_t loc = newReg();
        bytecode.push_back(MOVREGINT_OP);
        bytecode.push_back(loc);
        bytecode += lit.toBytes();
        return loc;
    }
    // oops not implemented
}


