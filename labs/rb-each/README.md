# rb-each

A command-line tool that executes a given command across all installed Ruby versions managed by mise.

## Usage

```bash
rb-each <command> [args...]
```

## Examples

Check Ruby version for all installed Ruby versions:
```bash
rb-each ruby --version
```

Check Gem version for all installed Ruby versions:
```bash
rb-each gem --version
```

Run a Ruby script with all Ruby versions:
```bash
rb-each ruby -e 'puts RUBY_VERSION'
```

Run tests with all Ruby versions:
```bash
rb-each ruby test.rb
```

## How it works

1. Runs `mise ls ruby -J` to get a list of all Ruby versions
2. Filters to only installed versions
3. For each installed version, runs `mise exec ruby@<version> -- <your_command>`
4. Displays the output for each version

## Building

```bash
cargo build --release
```

The binary will be available at `target/release/rb-each`.

## Requirements

- [mise](https://mise.jdx.dev/) - for managing Ruby versions
- Rust - for building the tool
