require 'stringio'
require_relative '../utils'
require_relative 'bytecode'

class Emitter
	attr_reader :out

	def initialize(out=StringIO.new)
		@out = out
	end

	def emit(*bytes)
		@out.write bytes.pack('C*')
	end

	Bytecode::BYTECODES.each do |bytecode|
		expected = bytecode.args.length
		define_method bytecode.to_s do |*args|
			unless args.length == expected
				raise ArgumentError, "wrong number of arguments (given #{args.length}, expected #{expected})"
			end

			emit bytecode

			bytecode.args.zip(args).each do |type, arg|
				case type
				when :reg then emit arg.reg!
				when :u8, :i8, :u16, :i16, :u32, :i32, :u64, :i64
					emit(*arg.public_send(:"#{type}_bytes"))
				else raise "internal error: unknown type: #{type}; bytecode=#{bytecode.inspect}"
				end
			end
		end
	end
end
