require 'stringio'
require_relative '../utils'
require_relative 'bytecode'

module Disassembler
	module_function

	def disassemble_file(inp, out=$>)
		inp = fix_inp inp
		raise "unknown file type: #{type}" if "sojourn1" != type = inp.read(8)
		textlen = Integer.read_u64(inp) or raise "can't read textlen"
		datalen = Integer.read_u64(inp) or raise "can't read datalen"

		disassemble_text inp.read(textlen), out
		disassemble_data inp.read(datalen), out
	end

	def disassemble_text(inp, out=$>)
		inp = fix_inp inp

		out.puts "===[text]==="

		until inp.eof?
			opcode = Integer.read_u8(inp) or raise "can't read opcode"
			bytecode = Bytecode::from_opcode(opcode) or raise "unknown opcode: 0x#{opcode.to_hex}"
			out.write "#{inp.tell.-(1).to_hex(4, '') }: "
			hexform = [opcode.to_i.to_hex]
			humanform = +bytecode.to_s

			jump_amnt = nil
			bytecode.args.each do |type|
				case type
				when :reg
					reg = Integer.read_u8(inp) or raise 'cannot read register'
					hexform += reg.to_hex.split
					humanform.concat " r#{reg}"
				when :u8, :i8, :u16, :i16, :u32, :i32, :u64, :i64
					num = Integer.public_send(:"read_#{type}", inp) or raise "can't read #{type}"
					jump_amnt = num
					hexform += num.to_hex(type[1..].to_i / 8).split
					humanform.concat " $0x#{num.to_hex nil, ''}"
				else
					raise "internal error: unknown type: #{type}; bytecode=#{bytecode.inspect}"
				end
			end

			out.printf "%-30s %-10s", hexform.join(' '), humanform
			if bytecode.jump?
				out.printf "% 30s", "(=#{(inp.tell - 3 + jump_amnt).to_hex 4, ''})"
			end
			out.puts
		end
	end

	def disassemble_data(inp, out=$>)
		inp = fix_inp inp
		amount = Integer.read_u32(inp) or fail "couldn't read amount of data"
		out.puts "===[data]==="
		(out.puts "<empty>"; return) if amount.zero?

		amount.times do |idx|
			len = Integer.read_u8(inp) or raise "can't read length of #{idx}"
			if len == 255
				len = Integer.read_u32(inp) or raise "can't read length of #{idx}"
			end

			out.printf "%4d: %p\n", idx, inp.read(len)
		end
	end

	class << self
		private def fix_inp(inp)
			inp.is_a?(String) ? StringIO.new(inp) : inp
		end
	end
end
