require_relative '../utils'

class Bytecode
	BYTECODES = []

	class << self
		# Don't let others declare new bytecodes.
		private :new

		# Only this class can declare new bytecodes.
		private def declare_bytecode(opcode, name, *args, jump: false)
			raise "duplicate opcode: #{opcode}" if from_opcode opcode

			bytecode = new(name.to_s.downcase, opcode, args, jump)
			const_set name, bytecode
			BYTECODES.push bytecode
		end
	end

	def self.from_opcode(opcode)
		BYTECODES.find { _1.opcode == opcode }
	end

	# Readers for all our attributes.
	attr_reader :name, :opcode, :args

	# Iniitalize the bytecode and freeze all values.
	def initialize(name, opcode, args, is_jump)
		# Sanity check.
		raise TypeError unless name.is_a?(String) && opcode.u8? && args.is_a?(Array)

		@name, @opcode, @args, @is_jump = name.freeze, opcode, args.freeze, is_jump
		freeze
	end

	# Whether this is a jump instruction or not
	def jump?; @is_jump end

	# We should be able to use this as if it were an integer.
	alias to_i opcode
	alias to_int to_i
	def u8?; true end
	alias u8! itself

	# Our string representation's simply our name.
	alias to_s name

	# Provide a debug inspect method.
	def inspect; "#{to_s}(#@opcode, #{@args.inspect})" end

	# General bytecodes
	declare_bytecode  0, :NOP
	declare_bytecode  1, :PUSH,   :reg
	declare_bytecode  2, :PUSHI,  :i64
	declare_bytecode  3, :PUSHBSX,  :i64
	declare_bytecode  4, :POP,    :reg
	declare_bytecode  5, :LOAD,   :reg, :reg
	declare_bytecode  6, :STORE,  :reg, :reg
	declare_bytecode  7, :MOV,    :reg, :reg
	declare_bytecode  8, :MOVW,   :reg, :i64
	declare_bytecode  9, :MOVBSX, :reg, :i8
	declare_bytecode 10, :DEBUG
	declare_bytecode 11, :TRAP,   :i8,  :u81
	# Math bytecodes
	declare_bytecode 12, :INC, :reg
	declare_bytecode 13, :DEC, :reg
	declare_bytecode 14, :ADD, :reg, :reg
	declare_bytecode 15, :SUB, :reg, :reg
	declare_bytecode 16, :MUL, :reg, :reg
	declare_bytecode 17, :DIV, :reg, :reg
	declare_bytecode 18, :MOD, :reg, :reg
	declare_bytecode 19, :AND, :reg, :reg
	declare_bytecode 20, :OR,  :reg, :reg
	declare_bytecode 21, :XOR, :reg, :reg
	declare_bytecode 22, :LSH, :reg, :reg
	declare_bytecode 23, :RSH, :reg, :reg
	declare_bytecode 24, :NEG, :reg
	declare_bytecode 25, :NOT, :reg
	declare_bytecode 26, :INV, :reg

	# Jump bytecodes
	declare_bytecode 27, :CMP, :reg
	declare_bytecode 28, :JEQ, :i16, jump: true
	declare_bytecode 29, :JNE, :i16, jump: true
	declare_bytecode 30, :JLT, :i16, jump: true
	declare_bytecode 31, :JLE, :i16, jump: true
	declare_bytecode 32, :JGT, :i16, jump: true
	declare_bytecode 33, :JGE, :i16, jump: true
	declare_bytecode 34, :JMP, :i16, jump: true

	# Function bytecodes
	declare_bytecode 35, :CALLS, :i16
	declare_bytecode 36, :CALLW, :i64
	declare_bytecode 37, :RET
	declare_bytecode 38, :LFS, :reg, :u8

	declare_bytecode 255, :EXT
end
