class RewriteRules
	class RewriteRulesException < Exception
		attr_reader :response
		def initialize(response)
			@response = response
		end
	end

	attr_reader :env
	attr_reader :path

	def self.rewrite(env, &block)
		self.new(env).run(&block)
	end

	def initialize(env, &block)
		@env = env
		@path_orig = "#{env['SCRIPT_NAME']}#{env['PATH_INFO']}"
		@path = @path_orig.dup
	end

	def run(&block)
		begin
			instance_eval(&block)
			if @path != @path_orig
				return [ 307, { 'x-reproxy-url' => path }, [ ] ]
			else
				return [ 399, {}, [ ] ]
			end
		rescue RewriteRulesException => e
			return e.response
		end
	end

	def rewrite(regexp, replace, flag=:continue)
		if @path.gsub!(regexp, replace)
			case flag
			when :redirect
				raise RewriteRulesException.new([ 302, { 'Location' => @path }, [ ] ])
			when :permanent
				raise RewriteRulesException.new([ 301, { 'Location' => @path }, [ ] ])
			when :break
				raise RewriteRulesException.new([ 307, { 'x-reproxy-url' => @path }, [ ] ])
			when :continue
				# nothing
			else
				raise "unsupported flag: #{flag}"
			end
		end
	end
end
