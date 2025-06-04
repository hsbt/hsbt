Gem::Specification.new do |spec|
  spec.name          = "gem-rebuild"
  spec.version       = "0.1.0"
  spec.authors       = ["Your Name"] # TODO: Replace with your name
  spec.email         = ["your.email@example.com"] # TODO: Replace with your email

  spec.summary       = "RubyGems plugin to rebuild gems with missing extensions."
  spec.description   = "A RubyGems plugin that finds and reinstalls gems with missing compiled extensions."
  spec.homepage      = "https://github.com/hsbt/hsbt/tree/main/labs/gem-rebuild" # TODO: Update if you have a different repoification.new do |spec|
  spec.name          = "gem-restore"
  spec.version       = "0.1.0"
  spec.authors       = ["Your Name"] # TODO: Replace with your name
  spec.email         = ["your.email@example.com"] # TODO: Replace with your email

  spec.summary       = "RubyGems plugin to restore gems with missing extensions."
  spec.description   = "A RubyGems plugin that finds and reinstalls gems with missing compiled extensions."
  spec.homepage      = "https://github.com/hsbt/hsbt/tree/main/labs/gem-restore" # TODO: Update if you have a different repo
  spec.license       = "MIT"

  # Prevent pushing this gem to RubyGems.org. To allow pushes either set the 'allowed_push_host'
  # to allow pushing to a single host or delete this section to allow pushing to any host.
  if spec.respond_to?(:metadata)
    spec.metadata["allowed_push_host"] = "TODO: Set to 'https://mygemserver.com'" # TODO: Or delete this line
  else
    raise "RubyGems 2.0 or newer is required to protect against public gem pushes."
  end

  spec.files         = Dir.glob("lib/**/*") + %w[README.md LICENSE.txt]
  spec.require_paths = ["lib"]

  spec.required_ruby_version = ">= 2.7.0" # Based on the original script

  # Add other dependencies here if any
  # spec.add_dependency "some_gem", ">= 1.0"
end
