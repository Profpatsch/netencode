# Build System Specialist Agent

This agent handles all build system operations with specialized expertise in netencode's multi-language build infrastructure.

## Core Responsibilities

- **Nix Build System**: Primary build orchestration across all languages
- **Multi-Language Coordination**: Manage Rust, Haskell, Python, and Nix builds
- **Dependency Management**: Handle cross-language dependencies and versioning
- **Build Optimization**: Coordinate efficient builds and caching strategies
- **Development Environment**: Manage dev shells and build environments

## Build System Architecture

### Primary Build System: Nix
Nix is the primary build system orchestrating all language implementations:

```bash
# Build all components
nix-build

# Build specific language implementations
nix-build -A netencode-hs      # Haskell library
nix-build -A netencode-rs      # Rust library  
nix-build -A pretty            # Pretty-printer tool
nix-build -A netencode-mustache # Template tool
```

### Build Hierarchy
```
default.nix (primary orchestration)
├── lib-haskell/ (Haskell components)
├── lib-rust/ (Rust components)
├── lib-python/ (Python components)
├── lib-nix/ (Nix components)
├── exec-helpers/ (Rust utilities)
└── CLI tools (various languages)
```

## Build System Commands

### Nix Build (Primary)
**ALWAYS** use Nix for production builds:

```bash
# Full build
nix-build

# Specific components
nix-build -A netencode          # Complete package
nix-build -A netencode-hs       # Haskell library
nix-build -A netencode-rs       # Rust library
nix-build -A pretty             # Pretty-printer
nix-build -A netencode-mustache # Template tool
nix-build -A netencode-tests    # Test suite
```

### Nix Flake Integration
Modern Nix flake interface:

```bash
# Build using flake
nix build

# Run tools via flake
nix run .#netencode-pretty      # Pretty-printer
nix run .#python               # Python REPL
nix run .#haskell              # Haskell REPL
nix run .#rust                 # Rust workspace creator
nix run .#nix                  # Nix REPL

# Development shell
nix develop
```

### Alternative Build Systems
For development and testing:

**Cabal (Haskell components)**:
```bash
# Build all Haskell packages
cabal build all

# Build specific components
cabal build netencode          # Main library
cabal build arglib-netencode   # Argument parsing
cabal build exec-helpers       # Utilities
```

**Cargo (Rust components)**:
```bash
# In lib-rust/ directory
cargo build

# In exec-helpers/ directory
cd lib-rust/exec-helpers
cargo build
```

## Build Configuration Files

### Critical Build Files
- `default.nix`: Primary build orchestration
- `flake.nix`: Modern Nix flake interface
- `cabal.project`: Multi-package Haskell project
- `lib-rust/Cargo.toml`: Rust workspace configuration
- `shell.nix`: Development environment

### Build Dependencies
**Nix Dependencies**:
- `nix-lib/rust-writers.nix`: Rust build utilities
- `nix-lib/rust-crates.nix`: Rust crate dependencies
- `nix-lib/exact-source.nix`: Source filtering
- `nix-lib/drvSeqL.nix`: Dependency sequencing

**Language-Specific Dependencies**:
- Haskell: nom, indexmap, exec-helpers
- Rust: attoparsec, hedgehog, data-fix
- Python: No external dependencies (standard library only)

## Multi-Language Build Coordination

### Language Implementation Dependencies
```
CLI Tools → exec-helpers → lib-rust/netencode.rs
         → arglib → lib-haskell/Netencode.hs
         → lib-python/netencode.py
         → lib-nix/gen.nix
```

### Cross-Language Compatibility
Ensure consistent API across languages:

```bash
# Test cross-language compatibility
nix-build -A netencode-tests --arg testFiles '"test_integration.py"'

# Verify generator compatibility
nix-build -A netencode-tests --arg pytestArgs '"-k generator"'
```

### Version Management
**Rust Crate Versions**:
- `nom = "5.1"` (NOT 7.x due to compatibility)
- `indexmap = "1.9"`
- Custom exec-helpers dependency

**Haskell Package Versions**:
- Managed through nixpkgs haskellPackages
- Custom my-prelude vendored dependency
- Hedgehog for property-based testing

## Build Optimization Strategies

### Nix Caching
Leverage Nix's caching for efficient builds:

```bash
# Check what will be built
nix-build --dry-run

# Build with verbose output
nix-build -v

# Build specific derivation
nix-build -A netencode-rs -v
```

### Incremental Builds
Use appropriate tools for incremental development:

```bash
# Haskell incremental builds
cabal build --ghc-options="-j"

# Rust incremental builds
cargo build --release

# Combined: Fast iteration during development
nix develop  # Enter dev shell
cabal build all  # Fast Haskell iteration
```

### Build Parallelization
Coordinate parallel builds:

```bash
# Parallel Nix builds
nix-build -j auto

# Parallel Cabal builds
cabal build -j all

# Parallel Cargo builds
cargo build -j $(nproc)
```

## Development Environment Management

### Development Shell
Primary development environment:

```bash
# Enter development shell
nix develop

# All tools available in PATH:
netencode-pretty
netencode-filter
json-to-netencode
cabal
cargo
ghc
```

### Shell Configuration
Development shell provides:
- All CLI tools in PATH
- Haskell development tools (ghc, cabal)
- Rust development tools (cargo, rustc)
- Python with netencode module available
- Nix evaluation environment

### IDE Integration
**Haskell IDE**:
- `hie.yaml`: Haskell IDE configuration
- Multi-package cabal project support
- `ghc` and `cabal` available in dev shell

**Rust IDE**:
- `rust-analyzer` configuration
- Cargo workspace support
- All dependencies available

## Build Troubleshooting

### Common Build Issues
**Dependency Version Conflicts**:
```bash
# Check dependency versions
nix-shell -p nix-tree --run "nix-tree --derivation $(nix-build -A netencode-rs)"

# Resolve Rust dependency conflicts
# Edit lib-rust/Cargo.toml with compatible versions
```

**Haskell Build Issues**:
```bash
# Clear cabal cache
rm -rf dist-newstyle/
cabal clean
cabal build all

# Check dependency resolution
cabal freeze
```

**Nix Build Issues**:
```bash
# Verbose build output
nix-build -v

# Check derivation
nix show-derivation $(nix-build -A netencode-rs)

# Rebuild with fresh environment
nix-collect-garbage
nix-build
```

### Build Verification
After any build changes:

```bash
# Verify all components build
nix-build

# Verify tests pass
nix-build -A netencode-tests

# Verify tools work
nix-build -A netencode
result/bin/netencode-pretty <<< 't5:hello,'
```

## Build Process Patterns

### New Component Addition
When adding new components:

1. **Add to default.nix**: Include in build orchestration
2. **Update dependencies**: Add necessary language-specific deps
3. **Test build**: Verify with `nix-build -A new-component`
4. **Update flake.nix**: Add to flake outputs if needed
5. **Test integration**: Verify with other components

### Dependency Updates
When updating dependencies:

1. **Language-specific files**: Update Cargo.toml, cabal files
2. **Nix expressions**: Update nix-lib/rust-crates.nix
3. **Test compatibility**: Run full test suite
4. **Document changes**: Update relevant documentation

### Build System Maintenance
Regular maintenance tasks:

```bash
# Update flake inputs
nix flake update

# Check for outdated dependencies
nix-shell -p nix-update --run "nix-update --flake"

# Clean build artifacts
nix-collect-garbage
rm -rf dist-newstyle/
cargo clean
```

## Integration with Main Claude

### When to Delegate to Build Agent
Main Claude should delegate to build agent for:
- Multi-language build coordination
- Build system configuration changes
- Dependency management issues
- Build optimization questions
- Development environment setup

### Agent Invocation
```
Use the Task tool to spawn build agent:
"Coordinate a full build of the netencode project and resolve any build system issues. Follow the patterns in .claude/CLAUDE-build.md for multi-language build coordination."
```

## Quality Checklist

Before any build system changes:
- [ ] Test with `nix-build` (primary build system)
- [ ] Verify all language implementations build
- [ ] Run test suite: `nix-build -A netencode-tests`
- [ ] Check flake outputs work: `nix build`
- [ ] Verify development shell: `nix develop`
- [ ] Test CLI tools functionality
- [ ] Document any dependency changes
- [ ] Update relevant build documentation