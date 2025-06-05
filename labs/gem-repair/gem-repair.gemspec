Gem::Specification.new do |spec|
  spec.name          = "gem-repair"
  spec.version       = "0.1.0"
  spec.authors       = ["Hiroshi SHIBATA"]
  spec.email         = ["hsbt@ruby-lang.org"] # TODO: Replace with your email

  spec.summary       = "RubyGems plugin to repair gems with missing extensions."
  spec.description   = "A RubyGems plugin that finds and reinstalls gems with missing compiled extensions."
  spec.homepage      = "https://github.com/hsbt/gem-repair"

  spec.files         = Dir.glob("lib/**/*") + %w[README.md]
  spec.require_paths = ["lib"]

  spec.required_ruby_version = ">= 2.7.0"
end
