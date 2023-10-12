
module Gem::Buzz
  class Gem::LoadError < ::LoadError
    def message
      puts :foo
      
      super
    end
  end
end

Gem.singleton_class.prepend Gem::Buzz

raise Gem::LoadError

