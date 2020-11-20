require_relative 'syntax/ast'
require_relative 'compiler/compiler'

compiler = Compiler.new do
	# emit_function 'debug', 'wat', block { push variable('wat'); debug; pop rreg; rreg }
	# emit_function 'add', 'lhs', block {
	# 	mov 0, variable('wat')
	# 	debug
	# 	rreg
	# }
	ptr = new_reg
	start = 0

	# Interrupt 1: Malloc 5 bytes of memory.
	int 1, ptr, 0x0c, 0x00, 0x00, 0x00
	mov start, ptr

	storew ptr, 4

	add ptr, with_scope { movbsx rreg, 8 }
	storeb ptr, 'H'.ord; inc ptr
	storeb ptr, 'i'.ord; inc ptr
	storeb ptr, '!'.ord; inc ptr
	storeb ptr, "\n".ord; inc ptr

	# Prints out the string at pointer 
	# int 3, start
	debug

	# new_data :string, ("foobar"*4)
	# new_data :string, "hello, "
	# movw 1, -1
	# new_data :string, "world"
	# new_data :string, "1"
	# new_data :string, "2"
	# new_data :string, "3"
	# new_data :string, "4"
	# new_data :string, "9"
	# new_data :string, "100"
	# new_data :string, "1099"
	# new_data :string, "103"
	# new_data :string, "1039"
end
END { system "cd .. && cargo run --bin sjvm" }

AST.new(<<'EOS'
# fn fib(n: Int): Int {
# 	a: Int = 0;
# 	b: Int = 1;
# 	while n - 1 {
# 		tmp: Int = b;
# 		b: Int = b + a;
# 		a: Int = tmp;
# 		n: Int = n - 1;
# 	};
# 	a
# }
# fib(10)
EOS
=begin
(<<'EOS'
fn fact(n: Int): Int {
	if n {
		debug(n);
		x: Int = fact(n - 1) ;
		debug(n);
		# debug(123) ;
		# debug(x) ;
		# debug(123) ;
		x
	} else {
		1
	}
}

debug(fact(4))
EOS
=begin
fn fib(n: Int): Int {
	a: Int = 0;
	b: Int = 1;
	while n - 1 {
		tmp: Int = b;
		b: Int = b + a;
		a: Int = tmp;
		n: Int = n - 1;
	};
	a
}

debug(fib(10))
EOS
=begin
fn fib(n: Int): Int {
	a: Int = 0;
	b: Int = 1;
	while n {
		tmp: Int = b;
		b: Int = b + a;
		a: Int = tmp;
		n: Int = n - 1;
	};
	a
}
;
# fn fact(n: Int): Int {
# 	if n {
# 		x: Int = fact(n - 1 ) ;
# 		x * n
# 	} else {
# 		1
# 	}
# }
debug(fib(10))
# debug(fact(10))
EOS

=begin
x: String = "hey\n"
print(x)
EOS
=begin
# fn div_by_three(x: Int): Int {
#  	x / 3
# }

x: Int = 9 ;

debug(div_by_three(x));

# 3 < 4
# x: Int = 10;
# 
# if x < 10 {
# 	debug(4)
# } else {
# 	debug(5)
# } ;
# "
=begin
# debug()

debug() ;
debug()

# fn add_two(x: Int): Int { x + 2 }
# add_two(x)

# fn square(x: Int): Int { x * x } 
# x: Int = square(4)

#  i: Int = 0
#  n: Int = 0
#  while i < 10 {
#  	n += i
#  	i += 1
#  }
=end
).each do |x|
	x.emit compiler
	# x.generate_bytecode context
end

# require_relative 'compiler/dis'
open('test.sjc', 'wb') { compiler.compile _1 }
# Disassembler.disassemble_file(open('test.sjc', 'rb', &:read), $>)

# puts context.emit

# context = Context.new
# Construct::If.new(12, Construct::If.new(13, true, false), 4).generate_bytecode(context)
# puts context
