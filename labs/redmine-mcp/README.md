# Redmine MCP Server

This is a Model Context Protocol (MCP) server for Redmine integration. It provides tools to interact with Redmine issues through the MCP protocol.

## Features

- Search and list issues with filtering
- Get detailed issue information

## Installation

1. Install dependencies:
```bash
bundle install
```

2. Set up environment variables:
```bash
export REDMINE_URL=https://your-redmine-instance.com
export REDMINE_API_KEY=your_api_key_here
```

## Usage

```bash
ruby bin/redmine_mcp_server.rb
```

The server will start on port 9292 by default. You can specify a different port using the `PORT` environment variable.

## Configuration

The server requires the following environment variables:

- `REDMINE_URL`: Your Redmine instance URL
- `REDMINE_API_KEY`: Your Redmine API key
- `PORT`: Port number (optional, default: 9292)

## Tools

### `list_issues`
List issues with optional filtering by project, status, assigned user, etc.

### `get_issue`
Get detailed information about a specific issue.

## Development

Debug mode:
```bash
DEBUG=1 ruby bin/redmine_mcp_server.rb
```
