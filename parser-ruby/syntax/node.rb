class Node
	def to_s
		"#{self.class.name}(#{instance_variables.map { instance_variable_get(_1).inspect }.join(', ') })"
	end
	alias inspect to_s
end

class Node
	Assignment = Class.new Node do
		attr_reader :ident, :value
		def initialize(ident, value, type) @ident, @value, @type = ident, value, type end

		def emit(compiler) compiler.emit_assignment(@ident, @value) end
	end

	Block = Class.new Node do
		attr_reader :exprs
		def initialize(exprs) @exprs = exprs end
	end

	BinaryOperator = Class.new Node do
		attr_reader :op, :lhs, :rhs
		def initialize(op, lhs, rhs) @op, @lhs, @rhs = op, lhs, rhs end

		def emit(compiler) compiler.emit_binary_op @op, @lhs, @rhs end
	end

	UnaryOperator = Class.new Node do
		attr_reader :op, :rhs
		def initialize(op, rhs) @op, @rhs = op, rhs end
	end
end

class Node::Function < Node
	attr_reader :name, :rettype, :args, :body
	def initialize(name, rettype, args, body) @name, @rettype, @args, @body = name, rettype, args, body end
	def emit(compiler)
		body = body()
		compiler.emit_function(name, *args.map(&:first), compiler.block {
			body.map { _1.emit compiler }.last
			# mov rreg, body.map {_1.emit compiler }.last
		})
	end
end

Node::Struct = ::Struct.new(:name, :args) { def inspect; "Struct(#{name.inspect}, #{args.inspect})" end }

class Node::Return < Node
	attr_reader :value

	def initialize(value) @value = value end
	def emit(compiler) compiler.emit_return(@value) end
end

Node::Continue = ::Object.new { def inspect; "Continue" end }
Node::Break = ::Object.new { def inspect; "Break" end }
class Node::FunctionCall < Node
	attr_reader :func, :args

	def initialize(func, args) @func, @args = func, args end
	def emit(compiler)
		compiler.emit_call_function compiler.fetch_function(@func), *@args
	end
end

class Node::While < Node
	attr_reader :cond, :body
	def initialize(cond, body)
		@cond, @body = cond, body
	end

	def emit(compiler)
		bod = @body
		compiler.emit_while(@cond, compiler.block { bod.map { _1.emit compiler }.last })
	end
end

class Node::If < Node
	attr_reader :cond, :if_true, :if_false
	def initialize(cond, if_true, if_false)
		@cond, @if_true, @if_false = cond, if_true, if_false
	end

	def emit(compiler)
		iftrue=@if_true
		iffalse=@if_false
		compiler.emit_if(@cond, compiler.block { iftrue.map { _1.emit compiler }.last },
			 compiler.block { iffalse.map { _1.emit compiler }.last }
			 )
	end
end

class Node::Variable < Node
	attr_reader :value

	def initialize(value) @value = value end

	def emit(compiler) compiler.emit_variable @value end
end

class Node::Boolean < Node
	attr_reader :value
	def initialize(value)
		@value = value
	end

	def inspect; "#{self.class}(#@value)" end

	def emit(compiler) compiler.emit_boolean @value end
end

class Node::Integer < Node
	attr_reader :value

	def initialize(value) @value = value end

	def emit(compiler)
		compiler.emit_integer(@value)
	end
end

class Node::String < Node
	attr_reader :value

	def initialize(value)
		@value = value
	end
	def inspect; "#{self.class}(#@value)" end

	def emit(compiler) compiler.emit_string(@value) end
end

class Node::Null < Node
	def inspect; "#{self.class}()" end

	def emit(compiler) compiler.emit_null end
end
