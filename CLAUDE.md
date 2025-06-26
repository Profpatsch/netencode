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
- **Nix**: Primary build system orchestrating both language implementations

### Core Components
- `Netencode.hs`: Main Haskell library with `TF` functor and `Fix` recursion
- `netencode.rs`: Rust library with nom-based streaming parser
- `exec-helpers/`: Rust utilities for command-line tools
- `arglib/`: Argument parsing library
- `third-party/my-prelude/`: Vendored Haskell utility library

### CLI Tools (built by Nix)
- `netencode-pretty`: Format pretty-printer
- `record-get`: Extract record fields
- `record-splice-env`: Execute commands with record fields as environment
- `env-splice-record`: Convert environment to netencode record
- `netencode-mustache`: Template rendering

## Development Commands

### Primary Build System (Nix)
```bash
# Enter development environment
nix-shell

# Build all components
nix-build

# Build specific components
nix-build -A netencode-hs      # Haskell library
nix-build -A netencode-rs      # Rust library  
nix-build -A pretty            # Pretty-printer
nix-build -A netencode-mustache # Template tool
```

### Alternative Build (Cabal)
```bash
# Build all packages
cabal build all

# Build specific components
cabal build netencode          # Main library
cabal build arglib-netencode   # Argument parsing
cabal build exec-helpers       # Utilities
```

### Rust Components
```bash
# In exec-helpers/ directory
cargo build
```

## Development Environment

The `nix-shell` provides:
- GHC with Hoogle documentation
- Cabal and Haskell Language Server
- All required dependencies for both Haskell and Rust

## Project Structure

- Multi-package Cabal project (`cabal.project`)
- Haskell IDE configuration in `hie.yaml`
- Nix helpers in `nix-lib/` for custom build utilities
- Cross-language compatibility maintained between Haskell and Rust implementations

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
