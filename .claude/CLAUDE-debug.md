# Debugging Specialist Agent

This agent handles all debugging and troubleshooting operations with specialized expertise in netencode's multi-language ecosystem.

## Core Responsibilities

- **Issue Diagnosis**: Identify and analyze problems across all language implementations
- **Error Pattern Recognition**: Recognize common failure modes and solutions
- **Cross-Language Debugging**: Debug issues spanning multiple language implementations
- **Performance Analysis**: Identify and resolve performance bottlenecks
- **Workaround Documentation**: Maintain known issues and workarounds

## Known Issues and Workarounds

### Claude Code Tool Issues

#### Bash Tool File Redirection Bug
**Issue**: The Bash tool has a bug with the `<` character where it incorrectly adds `/dev/null` redirection.

**Symptoms**:
- Commands like `./tool < file.txt` fail with "stdin was empty"
- File exists and has content, but tool receives no input

**Bug Report**: https://github.com/anthropics/claude-code/issues/2851

**Workaround**:
```bash
# Don't use: ./tool < file.txt
# Use instead: cat file.txt | ./tool
# Or: printf "content" | ./tool
```

**For Development**: 
- AVOID suggesting bash commands with `<` character
- Use `cat file |` or `printf ... |` patterns instead
- Use Write tool to create `./.claude-test` scripts instead of bash heredocs

## Common Debug Patterns

### Build System Issues

#### Nix Build Failures
**Symptoms**:
- Build fails with cryptic error messages
- Missing dependencies
- Version conflicts

**Debug Commands**:
```bash
# Verbose build output
nix-build -v

# Check derivation
nix show-derivation $(nix-build -A component-name)

# Dry run to see what would build
nix-build --dry-run

# Check dependency tree
nix-shell -p nix-tree --run "nix-tree --derivation $(nix-build -A component-name)"
```

**Common Solutions**:
- Clear nix cache: `nix-collect-garbage`
- Update flake inputs: `nix flake update`
- Check for missing dependencies in default.nix

#### Rust Dependency Conflicts
**Symptoms**:
- Cargo build fails with version conflicts
- Incompatible dependency versions
- Missing crate features

**Debug Commands**:
```bash
# Check dependency resolution
cargo tree

# Update dependencies
cargo update

# Check for conflicts
cargo check --verbose
```

**Common Solutions**:
- Use nom 5.1 (NOT 7.x) for netencode compatibility
- Check exec-helpers path dependencies
- Verify Cargo.toml version specifications

#### Haskell Build Issues
**Symptoms**:
- Cabal build fails with dependency issues
- Missing packages
- Version bound conflicts

**Debug Commands**:
```bash
# Clean build environment
cabal clean
rm -rf dist-newstyle/

# Check dependency resolution
cabal build --dry-run

# Freeze dependencies
cabal freeze
```

**Common Solutions**:
- Use `nix develop` for consistent environment
- Check haskellPackages versions in default.nix
- Verify my-prelude vendored dependency

### Runtime Issues

#### CLI Tool Failures
**Symptoms**:
- Tools fail to parse input
- Unexpected output format
- Permission errors

**Debug Commands**:
```bash
# Test with simple input
echo 't5:hello,' | netencode-pretty

# Check tool availability
which netencode-pretty
netencode-pretty --version

# Test with verbose output
echo 't5:hello,' | strace netencode-pretty
```

**Common Solutions**:
- Verify input format with json-to-netencode
- Check file permissions
- Ensure tool is built with correct dependencies

#### Generator Library Issues
**Symptoms**:
- Generated netencode doesn't match expected format
- Length prefixes incorrect
- Cross-language compatibility issues

**Debug Commands**:
```bash
# Test generator output
echo '{"test": "data"}' | json-to-netencode | hexdump -C

# Compare across languages
# Python: ne.text("hello")
# Rust: T::text("hello")
# Haskell: text "hello"
```

**Common Solutions**:
- Always use json-to-netencode for test data generation
- Check UTF-8 encoding for text values
- Verify length calculations

### Testing Issues

#### Test Failures
**Symptoms**:
- Tests fail unexpectedly
- Inconsistent test results
- Network test failures

**Debug Commands**:
```bash
# Run tests with verbose output
nix-build -A netencode-tests --arg pytestArgs '"-v --tb=short"'

# Run specific failing test
nix-build -A netencode-tests --arg pytestArgs '"-k test_name -v"'

# Create custom debug test
cat > .claude-test << 'EOF'
#!/bin/bash
set -x
echo "Debug test..."
# Add debugging commands
EOF
nix-build -A netencode-tests --arg customTest ./.claude-test
```

**Common Solutions**:
- Use proper test data generation
- Check for network connectivity for network tests
- Verify test environment consistency

## Performance Debugging

### Build Performance
**Symptoms**:
- Slow build times
- Excessive memory usage
- Build cache misses

**Debug Commands**:
```bash
# Profile build time
time nix-build

# Check build cache usage
nix-build --option substituters "" --option builders ""

# Monitor build resources
nix-build --option cores 1 --option max-jobs 1
```

**Optimization Strategies**:
- Use `nix-build -j auto` for parallel builds
- Leverage Nix binary cache
- Optimize dependency graph

### Runtime Performance
**Symptoms**:
- Slow netencode parsing/generation
- High memory usage with large netencode data
- Inefficient netencode format processing

**Debug Commands**:
```bash
# Profile netencode tool performance
time echo 'large-data' | netencode-pretty

# Test with progressively larger netencode data
echo '{"large": "data"}' | json-to-netencode | time netencode-pretty
```

**Common Netencode Bottlenecks**:
- Inefficient length prefix calculations
- Excessive memory allocation during format generation
- Suboptimal netencode parsing algorithms

## Cross-Language Debugging

### API Consistency Issues
**Symptoms**:
- Different output across language implementations
- Inconsistent behavior
- Missing functionality

**Debug Process**:
1. **Isolate behavior**: Test same input across all languages
2. **Compare outputs**: Use hex dump to compare byte-level output
3. **Check implementations**: Review generator library code
4. **Verify tests**: Ensure cross-language tests exist

**Debug Commands**:
```bash
# Test consistency across languages
echo '{"test": "data"}' | json-to-netencode | hexdump -C

# Compare language implementations
python3 -c "import netencode as ne; print(ne.text('hello'))"
# Compare with Rust, Haskell implementations
```

### Format Compatibility
**Symptoms**:
- Tools can't parse generated data
- Length prefix mismatches
- Encoding issues

**Debug Process**:
1. **Verify format**: Check against specification
2. **Test with reference**: Use json-to-netencode as reference
3. **Check encoding**: Verify UTF-8 handling
4. **Validate lengths**: Check length prefix calculations

## Error Pattern Recognition

### Common Error Patterns

#### "stdin was empty"
**Cause**: Bash tool `<` redirection bug
**Solution**: Use `cat file |` instead of `< file`

#### "No such file or directory"
**Cause**: Tool not in PATH or not built
**Solution**: Use `nix develop` or `nix-build -A tool-name`

#### "Permission denied"
**Cause**: Nix store read-only files
**Solution**: Use `cp --no-preserve=mode` when copying

#### "Version conflict"
**Cause**: Dependency version mismatch
**Solution**: Check dependency specifications in build files

#### "Parse error"
**Cause**: Invalid netencode format
**Solution**: Use json-to-netencode for proper generation

### Netencode-Specific Debug Commands
```bash
# Format validation
echo 'data' | json-to-netencode | netencode-pretty

# Length verification
echo 'data' | json-to-netencode | wc -c

# Cross-tool testing
echo 'data' | json-to-netencode | netencode-record-get field | netencode-plain

# Format inspection
echo 'data' | json-to-netencode | hexdump -C
```

## Integration with Main Claude

### When to Delegate to Debug Agent
Main Claude should delegate to debug agent for:
- Troubleshooting build failures
- Analyzing runtime errors
- Performance investigation
- Cross-language compatibility issues
- Complex multi-component debugging

### Agent Invocation
```
Use the Task tool to spawn debug agent:
"Analyze the build failure and provide debugging steps. Follow the systematic debugging process outlined in .claude/CLAUDE-debug.md and check for known issues."
```

## Netencode Debug Checklist

Before concluding netencode debugging:
- [ ] Problem reproduced with json-to-netencode test data
- [ ] Cross-language behavior verified (Rust, Haskell, Python, Nix)
- [ ] Netencode format validity confirmed with hexdump
- [ ] Solution tested across all netencode CLI tools
- [ ] Known netencode issues documented in .NOTES
- [ ] Workarounds tested with actual netencode workflows
- [ ] Related netencode components tested (generators, parsers, tools)