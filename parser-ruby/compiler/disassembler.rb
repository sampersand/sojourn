require 'stringio'


def disassemble(inp, output=StringIO.new)
	inp = StringIO.new inp if inp.is_a? String
	raise "bad file type: #{f}" unless 'sojourn1' == f = inp.read(8)

	amount_read = 0
	line = []
	inp.define_singleton_method :read do |amnt|
		amount_read += amnt
		line.concat((x=super(amnt)).bytes)
		x
	rescue
		amount_read -= amnt
		raise
	end
	byte = proc { "$#{inp.read(1).unpack('C').first}" }
	sbyte   = proc { "$#{inp.read(1).unpack('c').first }" }
	sshort  = proc { "$#{inp.read(2).unpack('s<').first }" }
	sword = proc { "$#{inp.read(8).unpack('q<').first}" }
	reg = proc { "r#{inp.read(1).ord.to_hex}" }

	output.puts "offset: hex repr   human repr      (=offfset)"
	output.puts "------:--------------------------------------"

	textlen = inp.read(8).unpack('Q<').first
	datalen = inp.read(8).unpack('Q<').first
	text_start_amount_read = amount_read
	line.clear
	while amount_read < textlen + text_start_amount_read # - 1
		offset = nil
		output.puts format("%-1$3s : %-4$10s %-2$10s % 3$13s",
			(amount_read-text_start_amount_read).>>(8).to_hex + ' ' + (amount_read-text_start_amount_read).&(0xff).to_hex,
			case (bte = inp.read(1).unpack('C').first)
			when  0 then 'nop'
			when  1 then "push #{reg.()}"
			when  2 then "pushi #{sword.()}"
			when  3 then "pop #{reg.()}"
			when  4 then "load #{reg.()} #{reg.()}"
			when  5 then "store #{reg.()} #{reg.()}"
			when  6 then "mov #{reg.()} #{reg.()}"
			when  7 then "movw #{reg.()} #{sword.()}"
			when  8 then "movbsx #{reg.()} #{sbyte.()}"
			when  9 then 'debug'
			when 10 then 
				"trap #{bte = byte.()} #{case bte[1..].to_i
					when 0 then sshort.()
					when 1 then byte.()
					else raise "unknown trap #{bte}"
					end
				}"
			when 11 then "add #{reg.()} #{reg.()}"
			when 12 then "sub #{reg.()} #{reg.()}"
			when 13 then "mul #{reg.()} #{reg.()}"
			when 14 then "div #{reg.()} #{reg.()}"
			when 15 then "mod #{reg.()} #{reg.()}"
			when 16 then "and #{reg.()} #{reg.()}"
			when 17 then "or #{reg.()} #{reg.()}"
			when 18 then "xor #{reg.()} #{reg.()}"
			when 19 then "lsh #{reg.()} #{reg.()}"
			when 20 then "rsh #{reg.()} #{reg.()}"
			when 21 then "neg #{reg.()}"
			when 22 then "inv #{reg.()}"
			when 23 then "not #{reg.()}"
			when 24 then "cmp #{reg.()}"
			when 25 then "jeq #{"$%04x" % (offset = sshort.())[1..].to_i(16)}"
			when 26 then "jne #{"$%04x" % (offset = sshort.())[1..].to_i(16)}"
			when 27 then "jlt #{"$%04x" % (offset = sshort.())[1..].to_i(16)}"
			when 28 then "jle #{"$%04x" % (offset = sshort.())[1..].to_i(16)}"
			when 29 then "jgt #{"$%04x" % (offset = sshort.())[1..].to_i(16)}"
			when 30 then "jge #{"$%04x" % (offset = sshort.())[1..].to_i(16)}"
			when 31 then "jmp #{"$%04x" % (offset = sshort.())[1..].to_i(16)}"
			when 32 then "jeqf #{"$%08x" % (offset = sword.())[1..].to_i(16)}"
			when 33 then "jnef #{"$%08x" % (offset = sword.())[1..].to_i(16)}"
			when 34 then "jltf #{"$%08x" % (offset = sword.())[1..].to_i(16)}"
			when 35 then "jlef #{"$%08x" % (offset = sword.())[1..].to_i(16)}"
			when 36 then "jgtf #{"$%08x" % (offset = sword.())[1..].to_i(16)}"
			when 37 then "jgef #{"$%08x" % (offset = sword.())[1..].to_i(16)}"
			when 38 then "jmpf #{"$%08x" % (offset = sword.())[1..].to_i(16)}"
			when 39 then "inc #{reg.()}"
			when 40 then "dec #{reg.()}"
			when 41 then "calls #{offset = sshort.()}"
			when 42 then "callw #{offset = sword.()}"
			when 43 then "ret"
			when 44 then "lfs #{reg.()} #{byte.()}"
			when 255 then fail 'todo: extension'
			else raise "unknown bytecode #{bte.to_hex}\ninput: #{inp.each_byte.map(&:to_hex).join(' ')}\noutput=#{output}"
			end,
			offset && "(=%02x %02x)" % [(x = amount_read - text_start_amount_read - 3 + offset[1..].to_i) >> 8, x & 0xff],
			line.map(&:to_hex).join(' '),
		).rstrip
		line.clear
	end

	output.puts "\nidx: hex repr   type human repr"
	output.puts "---:-----------------------------------"
	data = []
	inp.read(8).unpack('Q<').first.times do
		data.push inp.read((255 == amnt = inp.read(1).unpack('C').first) ?
			inp.read(8).unpack('Q<').first : amnt)
	end

	data.each_with_index do |d, i|
		puts "#{i}   #{d.inspect}"
	end

	output
end

