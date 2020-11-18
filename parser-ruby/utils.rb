class Integer
	U8_MIN =  0x00 ; U16_MIN =  0x00_00 ; U32_MIN =  0x0000_0000 ; U64_MIN =  0x0000_0000_0000_0000 ; 
	U8_MAX =  0xFF ; U16_MAX =  0xFF_FF ; U32_MAX =  0xFFFF_FFFF ; U64_MAX =  0xFFFF_FFFF_FFFF_FFFF ; 
	I8_MIN = -0x80 ; I16_MIN = -0x80_00 ; I32_MIN = -0x8000_0000 ; I64_MIN = -0x8000_0000_0000_0000 ; 
	I8_MAX =  0x7F ; I16_MAX =  0x7F_FF ; I32_MAX =  0x7FFF_FFFF ; I64_MAX =  0x7FFF_FFFF_FFFF_FFFF ; 

	def to_hex(amnt = nil, sep=' ')
		pack = 
			case
			when amnt == 1 || amnt.nil? && (u8? || i8?) then return ("%02X" % self)[-2..]
			when amnt == 2 || amnt.nil? && (u16? || i16?) then (i16? ? 's' : 'S') + '>'
			when amnt == 4 || amnt.nil? && (u32? || i32?) then (i32? ? 'l' : 'L') + '>'
			when amnt == 8 || amnt.nil? && (u64? || i64?) then (i64? ? 'q' : 'Q') + '>'
			else raise "unknown to_hex amount #{amount.inspect}"
			end
		[self].pack(pack).unpack("C*").map(&:to_hex).join sep
	end

	# Generate instance methods for all int variants
	[%w(8 C), %w(16 S<), %w(32 L<), %w(64 Q<)].each do |size, pack|
		eval <<~CODE, binding, __FILE__, __LINE__
			def u#{size}?; (U#{size}_MIN..U#{size}_MAX).include? self end
			def i#{size}?; (I#{size}_MIN..I#{size}_MAX).include? self end
			def u#{size}!; u#{size}? ? self : raise(ArgumentError, "not a u#{size}: \#{self}") end
			def i#{size}!; i#{size}? ? self : raise(ArgumentError, "not a i#{size}: \#{self}") end

			def pack_u#{size}; [u#{size}!].pack #{pack.upcase.inspect} end
			def pack_i#{size}; [i#{size}!].pack #{pack.downcase.inspect} end
			def u#{size}_bytes; pack_u#{size}.bytes end
			def i#{size}_bytes; pack_i#{size}.bytes end

			def self.unpack_u#{size}(bytes) bytes.unpack(#{pack.upcase.inspect}).first end
			def self.unpack_i#{size}(bytes) bytes.unpack(#{pack.downcase.inspect}).first end
			def self.read_u#{size}(io) unpack_u#{size} io.read(#{size.to_i / 8}) || return end
			def self.read_i#{size}(io) unpack_i#{size} io.read(#{size.to_i / 8}) || return end
		CODE
	end

	alias reg! u8!
	alias reg? u8?
end
