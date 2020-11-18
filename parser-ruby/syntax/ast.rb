require_relative 'lexer'
require_relative 'node'

class AST
	include Enumerable

	def initialize(lexer)
		@lexer = Lexer.new(lexer).each
	end

	def each
		return to_enum __method__ unless block_given?

		while (e = begin expression; rescue StopIteration; nil end)
			yield e
		end
	end

	private

	def next_if(type, value=nil, ignore_soft: true)
		while ignore_soft && @lexer.peek.type == :endline && @lexer.peek.value == :soft
			@lexer.next
		end

		return if @lexer.peek.type != type
		return if !value.nil? && @lexer.peek.value != value
		@lexer.next
	end

	# expression :=
	# 	<primary> | <ident> '=' <expr> | <ident> '(' (EMPTY | <expression> (',' <expresison>)*) ')'
	# 	| <expression> BINARY_OP <expression> 
	def expression
		return expression if next_if :endline
		prim = primary
		begin
			return prim if next_if :endline, ignore_soft: false
		rescue StopIteration
			return prim
		end


		if prim.is_a?(Node::Variable)
			if next_if(:colon)
				type = identifier&.value or raise 'missing type for assignment'
				next_if :operator, '=' or raise 'missing equal sign for assignment'
				rhs = expression or raise 'missing rhs for assignment'
				return Node::Assignment.new(prim.value, rhs, type)
			elsif next_if(:lparen, '(')
				args = []
				until next_if(:rparen, ')')
					arg = expression or raise 'missing argument for function call'
					args.push arg
					next_if :comma
				end
				return Node::FunctionCall.new(prim.value, args)
			end
		end

		if op = next_if(:operator)
			raise "unary op given in binary op position" if op.value == '~' || op.value == '!'
			rhs = expression
			Node::BinaryOperator.new(op.value, prim, rhs)
		else
			prim
		end
	end

	# primary := <keywordfn> | <literal> | <grouping> | <block> | <unary-op>
	def primary
		keywordfn || literal || grouping || block || unary_op
	end

	# keyword := <func> | <struct> | <if> | <while> | <return> | <continue> | <break>
	def keywordfn
		func || struct || if_ || while_ || return_ || continue || break_
	end

	# func := 'func' <identifier>? '(' (EMPTY | <identifier> (',' <identifier>)*) ')' <block>
	def func
		next_if :keyword, 'fn' or return
		name = next_if(:identifier)
		next_if :lparen, '(' or raise 'missing `(` after `func` declaration'

		args = []

		until next_if(:rparen, ')')
			arg = next_if(:identifier) or raise 'invalid identifier name in `func` declaration'
			next_if :colon or raise "missing colon after argument name"
			type = next_if(:identifier) or raise 'invalid type name in `func` declaration.'
			args.push [arg.value, type.value]
			next_if :comma
		end
		ret = next_if(:colon) or raise 'missing colon for return type'
		type = next_if(:identifier) or raise 'invalid type name in `func` declaration.'
		body = block or raise 'missing block for function declaration'

		Node::Function.new(name.value, type.value, args, body)
	end

	# struct := 'struct' <identifier> '{' <identifier> (',' <identifier>)* '}'
	def struct
		next_if :keyword, 'struct' or return
		name = next_if(:identifier) or raise 'missing identifier for `struct` declaration'
		next_if :lparen, '{' or raise 'missing `{` after `struct` declaration'

		fields = []
		until next_if(:rparen, '}')
			field = next_if(:identifier) or raise 'invalid field name in `struct` declaration'
			fields.push field.value
			next_if :comma
		end

		Node::Struct.new(name.value, fields)
	end

	# if := 'if' <expression> <block> ('else' <block>)?
	def if_
		next_if(:keyword, 'if') or return
		cond = expression or raise 'missing condition for `if` statement'
		if_true = block or raise 'missing true block for `if` statement'
		if next_if(:keyword, 'else')
			if_false = block or raise 'missing false block for `if` statement'
		end

		Node::If.new(cond, if_true, if_false)
	end

	# while := 'while' <expression> <block>
	def while_
		next_if :keyword, 'while' or return
		cond = expression or raise 'missing condition for `while` statement'
		body = block or raise 'missing true block for `while` statement'

		Node::While.new(cond, body)
	end

	# return := 'return' <expression>?
	def return_
		next_if :keyword, 'return' or return
		Node::Return.new expression
	end

	# continue := 'continue'
	def continue
		next_if :keyword, 'continue' and Node::Continue.new
	end

	# break := 'break'
	def break_
		next_if :keyword, 'break' and Node::Break.new
	end

	# grouping := '(' <expression> ')'
	def grouping
		next_if :lparen, '(' or return
		ret = expression or raise 'missing expression in grouping body'
		next_if :rparen, ')' or raise 'missing closing rparen in grouping body.'
		ret
	end

	# unary-op := [-+~!] <expression>
	def unary_op
		%w(+ - ! ~).each do |op|
			next_if(:operator, op) or next
			rhs = expression or raise 'missing rhs for unary op'
			return Node::UnaryOperator.new(op, rhs)
		end
		nil
	end

	# block := '{' <expression>* '}'
	def block
		next_if :lparen, '{' or return
		body = []

		until next_if(:rparen, '}')
			expr = expression or raise 'missing expression in block body'
			body.push expr
		end

		body
	end

	# literal := <boolean> | <null> | <string> | <integer> | <identifier>
	def literal
		boolean || null || string || integer || identifier
	end

	def null
		next_if(:null) and Node::Null.new
	end

	def boolean
		boolean = next_if(:boolean) and Node::Boolean.new(boolean.value)
	end

	def string
		string = next_if(:string) and Node::String.new(string.value)
	end

	def integer
		integer = next_if(:integer) and Node::Integer.new(integer.value)
	end

	def identifier
		identifier = next_if(:identifier) and Node::Variable.new(identifier.value)
	end
end
