# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is **netencode**, a data serialization format and library implementation. It's a binary format that's human-readable for debugging, type-safe, and designed for easy generation and parsing across languages.

### Key Format Features
- Scalars: unit, booleans, numbers (with explicit bit sizes), text (UTF-8), binary data
- Composite: tagged values (sum types), records (key-value maps), lists
- Length-prefixed variable data for efficient parsing
- ASCII prefixes make format partially human-readable

## Architecture

### Multi-language Implementation
- **Haskell** (`lib-haskell/Netencode.hs`, `lib-haskell/Netencode/Parse.hs`): Type-safe implementation with strong ADTs
- **Rust** (`lib-rust/netencode.rs`, `pretty.rs`): Performance-oriented with both owned (`T`) and borrowed (`U`) representations
- **Python** (`lib-python/netencode.py`): Generator library with standardized API for netencode construction
- **Nix** (`lib-nix/gen.nix`): Generator functions with unified API
- **Nix**: Primary build system orchestrating all language implementations

### Core Components
- `lib-haskell/Netencode.hs`: Main Haskell library with `TF` functor and `Fix` recursion
- `lib-rust/netencode.rs`: Rust library with nom-based streaming parser
- `lib-python/netencode.py`: Python generator library with unified API
- `lib-nix/gen.nix`: Nix generator functions with standardized interface
- `exec-helpers/`: Rust utilities for command-line tools
- `arglib/`: Argument parsing library
- `third-party/my-prelude/`: Vendored Haskell utility library

### CLI Tools (built by Nix)
- `netencode-pretty`: Format pretty-printer
- `netencode-plain`: Extract scalar values from netencode as plain text
- `netencode-filter`: Filter netencode records by field values
- `netencode-record-get`: Extract record fields
- `netencode-to-env`: Execute commands with record fields as environment
- `env-to-netencode`: Convert environment to netencode record
- `json-to-netencode`: Convert JSON to netencode format
- `netencode-mustache`: Template rendering

## Development Commands

### Testing (75 tests total: 73 offline + 2 network)

DON’T `cd` INTO INTO `tests/`!

#### Automated Testing (Nix Build - 73 offline tests)

```bash
# Run all offline tests automatically in nix-build (recommended)
nix-build -A netencode-tests

# Run specific test file
nix-build -A netencode-tests --arg testFiles '"test_integration.py"'

# Run tests matching a pattern
nix-build -A netencode-tests --arg pytestArgs '"-k json_to_netencode"'

# Run with verbose output
nix-build -A netencode-tests --arg pytestArgs '"-v"'

# Combine options: specific file with verbose output
nix-build -A netencode-tests --arg testFiles '"test_integration.py"' --arg pytestArgs '"-v"'
```

#### Ad-hoc Custom Testing

```bash
# Run custom commands with access to all netencode tools
nix-build -A netencode-tests --arg customTest ./path/to/test-script.sh

# Example custom test script (test-script.sh):
#!/bin/bash
echo "Testing specific functionality..."
echo '"hello"' | json-to-netencode | netencode-pretty
echo "Custom test completed"

# Multi-line testing with immediate feedback
nix-build -A netencode-tests --arg customTest "$(cat <<'EOF'
#!/bin/bash
echo "Testing multiple formats:"
echo '"short"' | json-to-netencode | netencode-pretty
echo '"This is a very long text that definitely exceeds the forty character limit"' | json-to-netencode | netencode-pretty
EOF
)"

# Combine custom tests with regular pytest
nix-build -A netencode-tests --arg customTest ./my-test.sh --arg testFiles '"test_integration.py"'
```

#### Manual Testing (Network Tests Only)

```bash
# Enter test environment for network tests
nix-shell tests/shell.nix --run "<test-command>"

# Run network tests only (requires internet)
pytest -q --tb=short test_network.py          # 2 network tests

# Run all tests including network tests (requires internet)
pytest -q --tb=short

# Run with verbose output for debugging
pytest -v test_network.py
```

#### Test Structure

- **test_integration.py**: 36 CLI tool integration tests (offline)
- **test_readme_examples.py**: 15 documentation example tests (offline) 
- **test_netencode_py.py**: 22 Python module unit tests (offline)
- **test_network.py**: 2 tests requiring network connectivity (GitHub API, nix flake)

The offline tests (73 total) run automatically in nix-build without network access. Network tests are available for manual verification when internet connectivity is available.

### Build Systems

#### Nix (Primary)
```bash
# Build all components
nix-build

# Build specific components
nix-build -A netencode-hs      # Haskell library
nix-build -A netencode-rs      # Rust library
nix-build -A pretty            # Pretty-printer
nix-build -A netencode-mustache # Template tool
```

#### Nix Flake
```bash
# Build using flake
nix build

# Run tools via flake
nix run .#netencode-pretty
nix run .  # Default app (netencode-pretty)

# Enter development shell
nix develop
```

#### Cabal (Alternative)
```bash
# Build all packages
cabal build all

# Build specific components
cabal build netencode          # Main library
cabal build arglib-netencode   # Argument parsing
cabal build exec-helpers       # Utilities
```

#### Rust Components
```bash
# In exec-helpers/ directory
cargo build
```

## Project Structure

- **Language-organized directories**: `lib-haskell/`, `lib-rust/`, `lib-python/`, `lib-nix/`
- **Unified API**: Standardized function names across all generator libraries
- Multi-package Cabal project (`cabal.project`)
- Haskell IDE configuration in `hie.yaml`
- Nix helpers in `nix-lib/` for custom build utilities
- Automated test suite with nix-build integration
- Cross-language compatibility with consistent APIs
- Nix flake for reproducible builds and development environments

## Key Implementation Details

### Haskell
- Uses Fix-based recursion with TF functor
- Attoparsec for parsing with proper error handling
- Hedgehog property-based testing for roundtrip verification
- Stable encoding with sorted record keys

### Rust
- Nom-based incremental parsing
- Zero-copy parsing support with borrowed types
- Composable decoder framework for type-safe extraction
- Integration with Unix tooling philosophy

### Python
- Generator library with standardized API (`lib-python/netencode.py`)
- Unified function names: `unit()`, `natural()`, `integer()`, `boolean()`, `text()`, `binary()`, `tag()`, `record()`, `list()`
- Direct binary format generation without intermediate representations
- Convenience functions for common patterns (`simple_record()`, `record_ordered()`)

### API Standardization
- **Unified naming**: All generator libraries use identical function names
- **Cross-language compatibility**: Same logical operations work identically everywhere
- **Consistent output**: All languages produce identical netencode for same data
- **Documentation**: Single API reference covers all language implementations

## Commit Message Format

Use conventional commit format with these conventions:
- Basic verbs: `feat`, `chore`, `doc`, `refact`
- Always include scope in parentheses: `(scope)`
- Add ✨ emoji after scope for AI-generated commits: `doc(meta): ✨ description`
- Include detailed explanation in commit body
- End with Claude Code attribution

Example:
```
doc(meta): ✨ add CLAUDE.md for AI assistant guidance

Analyzed codebase structure, build systems (Nix/Cabal/Cargo), and architecture
to create comprehensive guidance file for Claude Code.

🤖 Generated with [Claude Code](https://claude.ai/code)

Co-Authored-By: Claude <noreply@anthropic.com>
```

### Commit Guidelines
- Wrap commit line lengths at around 70 characters
- For commit messages, make the first line less than 70 characters long
- Use little bullet points in the extended message and no flowery language
- When compacting your history, write a file to docs/ that is called `<date>_<timestamp>_claude-compact.md` and contains the compacted history text


## Known Issues / Workarounds

### Bash Tool File Redirection Bug
**Issue**: The Bash tool has a bug with the `<` character where it incorrectly adds `/dev/null` redirection. Commands like `./tool < file.txt` will fail with "stdin was empty" even when the file exists and has content.

**Bug Report**: https://github.com/anthropics/claude-code/issues/2851

**Workaround**: Use pipe redirection instead:
```bash
# Don't use: ./tool < file.txt
# Use instead: cat file.txt | ./tool
```

**For Claude Code**: AVOID suggesting any bash commands that contain the `<` character for file redirection. Always use `cat file |` or `printf ... |` patterns instead.
