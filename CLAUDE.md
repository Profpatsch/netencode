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
- **Haskell** (`Netencode.hs`, `Netencode/Parse.hs`): Type-safe implementation with strong ADTs
- **Rust** (`netencode.rs`, `pretty.rs`): Performance-oriented with both owned (`T`) and borrowed (`U`) representations
- **Python** (`tests/netencode_py.py`): Testing module with bytes-based netencode construction
- **Nix**: Primary build system orchestrating all language implementations

### Core Components
- `Netencode.hs`: Main Haskell library with `TF` functor and `Fix` recursion
- `netencode.rs`: Rust library with nom-based streaming parser
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

### Testing (58 tests total)

DON’T `cd` INTO INTO `tests/`!

```bash
# Enter test environment
nix-shell tests/shell.nix --run "<test-command>"

# Run all tests (default: quiet mode with short tracebacks)
pytest -q --tb=short

# Run specific test files
pytest -q --tb=short test_integration.py      # 36 integration tests
pytest -q --tb=short test_readme_examples.py  # 17 README example tests
pytest -q --tb=short test_netencode_py.py     # 22 Python module tests

# Run with full verbose output (only when errors need investigation)
pytest -v

# Run specific test
pytest -q --tb=short test_readme_examples.py::TestReadmeExamples::test_basic_record_field_extraction
```

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

## Development Environment

### Main Development Shell (`nix-shell` or `nix develop`)
- GHC with Hoogle documentation
- Cabal and Haskell Language Server
- All required dependencies for Haskell, Rust, and Python

### Test Environment (`nix-shell tests/shell.nix`)
- Python 3.13+ with pytest
- All netencode tools available via environment variables
- Proper test isolation and tool path setup

## Project Structure

- Multi-package Cabal project (`cabal.project`)
- Haskell IDE configuration in `hie.yaml`
- Nix helpers in `nix-lib/` for custom build utilities
- Comprehensive Python test suite in `tests/` (58 tests total)
- Cross-language compatibility across Haskell, Rust, and Python
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
- Bytes-based netencode construction for testing
- Direct binary format generation without intermediate representations
- Comprehensive test suite covering all CLI tools and edge cases
- README example verification ensuring documentation accuracy

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

## Testing Guidelines

### Test Organization
- `test_integration.py`: Tool pipelines and cross-tool compatibility (36 tests)
- `test_readme_examples.py`: Documentation example verification (17 tests)
- `test_netencode_py.py`: Python module unit tests (22 tests)
- `netencode_py.py`: Python netencode construction module
- `conftest.py`: Shared utilities with bytes-based `run_tool()`

### Test Approach
- Bytes-based approach consistent with netencode being a binary format
- Use `b'expected'` not `'expected'` in assertions
- The `run_tool()` function accepts both string and bytes stdin, returns bytes stdout/stderr

### Adding New Tests
- Integration tests → `test_integration.py`
- README examples → `test_readme_examples.py`
- Use existing `netencode_py` module for test data construction

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
