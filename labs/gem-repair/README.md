# gem-repair

A RubyGems plugin to repair gems with missing extensions.

## Installation

Build and install the gem locally:

```sh
gem build gem-repair.gemspec
gem install ./gem-repair-0.1.0.gem
```

Or, if you are managing it within a Bundler context, add it to your Gemfile:

```ruby
gem 'gem-repair', path: './labs/gem-repair' # Adjust path as necessary
```

## Usage

After installation, the `gem repair` command will be available:

```sh
gem repair
```

You can also specify the number of parallel threads to use:

```sh
gem repair -j 8  # Use 8 threads
gem repair --jobs 2  # Use 2 threads
```

This command will:
1. Find all installed gems that are missing their compiled C extensions.
2. Attempt to reinstall these gems using multiple threads.

## Development

After checking out the repo, run `bundle install` to install dependencies. Then, you can run `rake test` to run the tests (though no tests are set up in this initial version).

To install this gem onto your local machine, run `bundle exec rake install`. To release a new version, update the version number in `gem-repair.gemspec`, and then run `bundle exec rake release`, which will create a git tag for the version, push git commits and tags, and push the `.gem` file to [a configured gem server](https://rubygems.org).

## Contributing

Bug reports and pull requests are welcome on GitHub at https://github.com/hsbt/hsbt. # TODO: Update if you have a different repo

## License

The gem is available as open source under the terms of the [MIT License](https://opensource.org/licenses/MIT).
