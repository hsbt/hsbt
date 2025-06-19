# GitHub Actions Updater

A GitHub CLI extension to update GitHub Actions in your workflow files from hash-based versions to the latest releases.

## Installation

Install this extension using the GitHub CLI:

```bash
gh extension install hsbt/gh-actions-updater
```

Or install locally from this directory:

```bash
gh extension install .
```

## Usage

Update all workflow files in `.github/workflows/`:

```bash
gh actions-updater
```

### Options

- `-f, --file FILE`: Specify workflow file(s) to update (can be used multiple times)
- `-a, --action ACTION`: Target specific action(s) to update (can be used multiple times)
- `--list-actions`: List all actions used in workflow files
- `-l, --list`: List available workflow files
- `-n, --dry-run`: Show what would be done without making changes
- `-v, --verbose`: Show more detailed output
- `-t, --token TOKEN`: GitHub API token to avoid rate limits
- `-h, --help`: Show help message

### Examples

List all workflow files:
```bash
gh actions-updater --list
```

List all actions in workflow files:
```bash
gh actions-updater --list-actions
```

Update specific workflow file:
```bash
gh actions-updater -f .github/workflows/ci.yml
```

Update specific action only:
```bash
gh actions-updater -a actions/checkout
```

Dry run to see what would be updated:
```bash
gh actions-updater --dry-run
```

Update with verbose output:
```bash
gh actions-updater --verbose
```

## Features

- Automatically detects GitHub Actions using hash-based versions (SHA commits)
- Fetches the latest release for each action
- Updates workflow files with the latest versions while preserving comments
- Supports dry-run mode to preview changes
- Can target specific workflow files or actions
- Provides detailed output and progress information

## Authentication

The extension uses GitHub CLI's authentication by default. You can also provide a GitHub token:

- Set `GITHUB_TOKEN` or `GH_TOKEN` environment variable
- Use the `-t, --token` option

## Requirements

- Ruby 2.7 or later
- GitHub CLI (`gh`)
- Internet connection to fetch latest action versions
