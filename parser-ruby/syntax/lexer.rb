Token = Struct.new(:type, :value) do
	def inspect
		"#{type.upcase}(#{value.inspect})"
	end
end

class Lexer
	include Enumerable

	def initialize(source)
		@source = source
	end

	def each
		return to_enum __method__ unless block_given?

		until @source.empty?
			case
			when @source.slice!(/\A(\n|;)/) then yield Token.new :endline, $& == ';' ? :hard : :soft
			when @source.slice!(/\A\s+/) then next
			when @source.slice!(/\A#.*$/) then next
			when @source.slice!(/\A\d+\b/) then yield Token.new :integer, $&.to_i
			when @source.slice!(/\Anull\b/) then yield Token.new :null, nil
			when @source.slice!(/\A(?:true|false)\b/) then yield Token.new :boolean, $& == 'true'
			when @source.slice!(/\A(?:if|else|return|while|continue|break|fn|struct)\b/) then yield Token.new :keyword, $&
			when @source.slice!(/\A[a-zA-Z_]\w*\b/) then yield Token.new :identifier, $&
			when @source.slice!(/\A(['"])((?:\\(?:[nrtf]|x\h\h)|(?!\1).)*)\1/)
				yield Token.new :string, $2.gsub('\n', "\n").gsub('\t', "\t").gsub('\r', "\r")
			when @source.slice!(/\A[({\[]/) then yield Token.new :lparen, $&
			when @source.slice!(/\A[)}\]]/) then yield Token.new :rparen, $&
			when @source.slice!(
					%r{\A(?:
						(?:\&\&\|\||\.)|
						(?:[-+*\/%]|\*\*)=?|  # mathematic operators
						(?:[&|^]|>>|<<)=?|    # bitwise operators
						(?:[=!<>]=?)|         # comparison operators and assignment
						[~!]                  # remaining unary operators
					)}x) then yield Token.new :operator, $&
			when @source.slice!(/\A,/) then yield Token.new :comma, nil
			when @source.slice!(/\A:/) then yield Token.new :colon, nil
			else raise "unknown token start #{@source[0].inspect}"
			end
		end
	end
end
