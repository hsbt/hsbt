# RubyGems Clone Plugin

A RubyGems plugin that allows you to clone gem repositories using `ghq` based on gem metadata.

## Installation

```bash
gem install rubygems-clone
```

Or build and install locally:

```bash
gem build rubygems-clone.gemspec
gem install rubygems-clone-0.1.0.gem
```

## Prerequisites

This plugin requires `ghq` to be installed and available in your PATH.

```bash
# Install ghq (example for macOS)
brew install ghq

# Or install via Go
go install github.com/x-motemen/ghq@latest
```

## Usage

```bash
# Clone a gem repository
gem clone sinatra

# Clone with verbose output
gem clone rails --verbose
```

The command will:

1. Fetch gem metadata from RubyGems.org API
2. Extract repository URL from:
   - `source_code_uri` metadata
   - `homepage_uri` (if it looks like a repository URL)
   - `project_uri` (if it looks like a repository URL)
3. Use `ghq get` to clone the repository

## Examples

```bash
$ gem clone sinatra
Executing: ghq get https://github.com/sinatra/sinatra
Successfully cloned repository: https://github.com/sinatra/sinatra

$ gem clone rails --verbose
Fetching gem metadata for 'rails'...
Found repository URL: https://github.com/rails/rails
Executing: ghq get https://github.com/rails/rails
Successfully cloned repository: https://github.com/rails/rails
```

## Development

After checking out the repo, run tests and build the gem:

```bash
gem build rubygems-clone.gemspec
gem install rubygems-clone-0.1.0.gem
```

## License

The gem is available as open source under the terms of the MIT License.
