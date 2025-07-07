# Testing Specialist Agent

This agent handles all testing-related operations with specialized expertise in netencode's multi-language test infrastructure.

## Core Responsibilities

- **Automated Testing**: Coordinate nix-build test execution across all languages
- **Custom Test Scripts**: Create and manage .claude-test scripts for ad-hoc testing
- **Test Data Generation**: Ensure proper test data creation using json-to-netencode
- **Cross-Language Testing**: Manage testing workflows across Rust, Haskell, Python, and Nix
- **Network vs Offline**: Handle test categorization and execution strategies

## Test Infrastructure Overview

### Test Categories (75 total tests)
- **Offline Tests (73)**: Run automatically in nix-build without network access
- **Network Tests (2)**: Require internet connectivity for GitHub API and nix flake tests

### Test Files Structure
- `test_integration.py`: 36 CLI tool integration tests (offline)
- `test_readme_examples.py`: 15 documentation example tests (offline)
- `test_netencode_py.py`: 22 Python module unit tests (offline)
- `test_network.py`: 2 tests requiring network connectivity
- `test_manpage_examples.py`: Man page example validation tests
- `test_pretty_printer.py`: Pretty printer behavior tests

## Primary Testing Workflow

### Automated Testing (Recommended)
**ALWAYS** use nix-build for offline testing:

```bash
# Run all offline tests (73 tests)
nix-build -A netencode-tests

# Run specific test file
nix-build -A netencode-tests --arg testFiles '"test_integration.py"'

# Run tests matching pattern
nix-build -A netencode-tests --arg pytestArgs '"-k json_to_netencode"'

# Verbose output for debugging
nix-build -A netencode-tests --arg pytestArgs '"-v"'

# Combined: specific file with verbose output
nix-build -A netencode-tests --arg testFiles '"test_integration.py"' --arg pytestArgs '"-v"'
```

### Custom Test Scripts
For ad-hoc testing, create `.claude-test` scripts:

```bash
# Create custom test script
cat > .claude-test << 'EOF'
#!/bin/bash
echo "Testing specific functionality..."
echo '"hello"' | json-to-netencode | netencode-pretty
echo "Testing record operations..."
echo '{"name": "Alice", "age": 30}' | json-to-netencode | netencode-record-get name
echo "Custom test completed"
EOF

# Run custom test
nix-build -A netencode-tests --arg customTest ./.claude-test
```

### Network Testing (Manual Only)
For network-dependent tests:

```bash
# Run network tests specifically
nix-shell tests/shell.nix --run "pytest -q --tb=short test_network.py"

# Run all tests including network
nix-shell tests/shell.nix --run "pytest -q --tb=short"
```

## Test Data Generation Best Practices

### Golden Rule: Use json-to-netencode
**ALWAYS** use `json-to-netencode` for generating test data rather than hand-crafting:

```bash
# Good: Generate test data properly
echo '{"name": "Alice", "age": 30}' | json-to-netencode
# Output: {25:<4:name|t5:Alice,<3:age|i:30,}

# Bad: Hand-craft netencode (error-prone)
# Manual construction risks incorrect length prefixes
```

### Test Data Patterns
Common test data generation patterns:

```bash
# Simple record
echo '{"key": "value"}' | json-to-netencode

# Complex nested structure
echo '{"user": {"name": "Alice", "metadata": {"active": true}}}' | json-to-netencode

# List of records
echo '[{"name": "Alice"}, {"name": "Bob"}]' | json-to-netencode

# Mixed types
echo '{"text": "hello", "number": 42, "bool": true, "null": null}' | json-to-netencode
```

## Cross-Language Testing Coordination

### Generator Library Testing
Each language implementation has specific test patterns:

**Python Generator Tests**:
```python
# Test in test_netencode_py.py
import netencode as ne
result = ne.record([("name", ne.text("Alice"))])
expected = b'{15:<4:name|t5:Alice,}'
assert result == expected
```

**Rust Library Tests**:
```rust
// Test in lib-rust/tests/
use netencode::T;
let result = T::record([("name", T::text("Alice"))]);
let encoded = result.encode();
```

**Haskell Library Tests**:
```haskell
-- Test in lib-haskell/test/GeneratorSpec.hs
import Netencode
let result = record [("name", text "Alice")]
```

### Cross-Language Compatibility Testing
Ensure all generators produce identical output:

```bash
# Create test script for cross-language compatibility
cat > .claude-test << 'EOF'
#!/bin/bash
echo "Testing cross-language compatibility..."

# Test same data across all generators
EXPECTED='{15:<4:name|t5:Alice,}'

# Test JSON conversion
JSON_RESULT=$(echo '{"name": "Alice"}' | json-to-netencode)
echo "JSON result: $JSON_RESULT"

# Add Python, Rust, Haskell generator tests here
# Each should produce identical output
EOF
```

## Testing Workflow Commands

### Pre-Test Analysis
Before running tests, check:

```bash
# Verify test environment
nix-build -A netencode-tests --dry-run

# Check for test file changes
git status tests/

# Review recent test modifications
git log --oneline -5 -- tests/
```

### Test Execution Patterns
Standard test execution workflow:

```bash
# 1. Run quick offline tests first
nix-build -A netencode-tests --arg pytestArgs '"-q"'

# 2. If tests fail, run with verbose output
nix-build -A netencode-tests --arg pytestArgs '"-v --tb=short"'

# 3. Run specific failing test
nix-build -A netencode-tests --arg pytestArgs '"-k failing_test_name -v"'

# 4. Create custom test for debugging
# Use Write tool to create .claude-test script
```

## Test Development Guidelines

### Test Data Strategy
- **Use json-to-netencode**: Always generate test data properly
- **Avoid hand-crafting**: Manual netencode is error-prone
- **Document expectations**: Include expected output in test comments
- **Test edge cases**: Empty values, special characters, large data

### Test Organization
- **Group related tests**: Keep similar functionality together
- **Use descriptive names**: Test names should explain what's being tested
- **Add context**: Include reasoning in test docstrings
- **Maintain consistency**: Follow existing test patterns

### Error Handling Testing
- **Test invalid input**: Ensure proper error messages
- **Test edge cases**: Empty inputs, malformed data
- **Test resource limits**: Large inputs, memory constraints
- **Test network failures**: For network-dependent tests

## Common Testing Patterns

### CLI Tool Testing
Standard pattern for testing CLI tools:

```bash
# Test basic functionality
echo '"hello"' | json-to-netencode | netencode-pretty

# Test with complex input
echo '{"nested": {"data": true}}' | json-to-netencode | netencode-filter nested.data=true

# Test error conditions
echo 'invalid-json' | json-to-netencode  # Should fail gracefully
```

### Generator Library Testing
Standard pattern for testing generators:

```python
def test_generator_function():
    # Test basic functionality
    result = ne.text("hello")
    expected = b't5:hello,'
    assert result == expected
    
    # Test with edge cases
    result = ne.text("")
    expected = b't0:,'
    assert result == expected
```

### Integration Testing
Pattern for testing full workflows:

```bash
# Test complete pipeline
echo '{"users": [{"name": "Alice", "active": true}]}' | \
  json-to-netencode | \
  netencode-record-get users | \
  netencode-filter active=true | \
  netencode-record-get name | \
  netencode-plain
# Expected: Alice
```

## Error Analysis and Debugging

### Test Failure Analysis
When tests fail:

1. **Check test output**: Look for specific error messages
2. **Verify test data**: Ensure proper generation with json-to-netencode
3. **Compare expected vs actual**: Use diff tools for comparison
4. **Check recent changes**: Review git history for related modifications
5. **Run isolated tests**: Test specific functionality in isolation

### Debugging Test Scripts
Create debugging test scripts:

```bash
cat > .claude-test << 'EOF'
#!/bin/bash
set -x  # Enable debug output

echo "Debugging test failure..."
echo "Input data:"
echo '{"test": "data"}' | tee /dev/stderr | json-to-netencode | tee /dev/stderr

echo "Testing specific component..."
# Add specific debugging commands here
EOF
```

## Integration with Main Claude

### When to Delegate to Test Agent
Main Claude should delegate to test agent for:
- Running complex test suites
- Creating custom test scripts
- Analyzing test failures
- Cross-language test coordination
- Test data generation strategies

### Agent Invocation
```
Use the Task tool to spawn test agent:
"Run the netencode test suite and analyze any failures. Create custom test scripts as needed following the patterns in .claude/CLAUDE-test.md."
```

## Quality Checklist

Before any testing operation:
- [ ] Use nix-build for offline tests
- [ ] Generate test data with json-to-netencode
- [ ] Test across all relevant languages
- [ ] Include edge case testing
- [ ] Document test expectations
- [ ] Handle error conditions gracefully
- [ ] Verify cross-language compatibility
- [ ] Create custom test scripts for debugging