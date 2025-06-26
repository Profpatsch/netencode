# Claude Integration Testing Instructions

This document provides instructions for creating and running integration tests for the netencode tools using Python pytest.

## Setup and Prerequisites

### Python Testing Environment

The tests use Python pytest with the development environment configured through Nix:

```bash
# Enter development shell with Python and pytest
nix-shell tests/shell.nix

# Or manually install pytest in your environment
pip install pytest
```

### Build Tools for Testing

The integration tests require the netencode tools to be built via nix-build:

```bash
# Build the tools (pytest will find them automatically)
nix-build -A json-to-netencode
nix-build -A netencode-filter
nix-build -A netencode-plain
nix-build -A record-get
nix-build -A env-splice-record
nix-build -A record-splice-env
nix-build -A pretty
```

## Test Structure

Tests are organized in the `tests/` directory:

- `conftest.py` - Shared fixtures and utilities
- `test_integration.py` - Core functionality tests (22 tests)
- `test_readme_examples.py` - Documentation verification tests (13 tests)
- `shell.nix` - Development environment with Python dependencies

### Test Organization

Tests are grouped by functionality:

1. **JSON conversion tests** - JSON to netencode transformation
2. **Filtering tests** - Record field filtering
3. **Pipeline integration tests** - End-to-end workflows
4. **README example tests** - Ensuring documentation examples work
5. **Error handling tests** - Invalid input handling

### Test Utilities

The `conftest.py` provides helper functions:

```python
def run_tool(tool_name: str, *args: str, stdin: Optional[str] = None, 
             expect_success: bool = True) -> subprocess.CompletedProcess:
    """Run a netencode tool with input and return the result."""
    
def assert_netencode_equal(actual: str, expected: str):
    """Compare netencode outputs, ignoring whitespace differences."""
```

## Running Tests

### Run All Tests

```bash
# From the project root in nix-shell
pytest tests/

# Or with verbose output
pytest -v tests/
```

### Run Specific Test Files

```bash
# Integration tests only
pytest tests/test_integration.py

# README example verification only
pytest tests/test_readme_examples.py

# Run with output on failure
pytest -s tests/
```

### Run Specific Tests

```bash
# Run tests matching a pattern
pytest -k "json_conversion" tests/

# Run with detailed output
pytest --tb=long tests/
```

## Test Coverage Areas

### JSON to Netencode Conversion
- Basic object conversion with various data types
- Array and nested object handling
- Error handling for malformed JSON
- Unicode and special character handling
- Binary data embedding examples

### Netencode Filtering and Processing
- Field-based filtering with different value types
- Record extraction and manipulation
- Environment variable integration
- Pipeline composition

### Documentation Verification
- All README examples are tested automatically
- GitHub API processing examples
- Binary data handling with printf/wc
- Configuration management workflows

### Error Handling
- Invalid input handling with readable error messages
- Tool chain error propagation
- UTF-8 string conversion for debugging

## Adding New Tests

When adding new tests:

1. Use descriptive test names that explain the behavior being tested
2. Include both positive (success) and negative (error) test cases
3. Test edge cases and boundary conditions
4. Use the `run_tool()` helper for consistent tool execution
5. Add new README examples to `test_readme_examples.py`

### Example Test Addition

```python
def test_json_to_netencode_handles_empty_objects():
    """Test that empty JSON objects are handled correctly."""
    result = run_tool("json-to-netencode", stdin='{}', expect_success=False)
    assert result.returncode != 0
    assert "empty record" in result.stderr.lower()
```

## Troubleshooting

### Tool Path Issues
Tests automatically find tools built by nix-build. If tools aren't found:
```python
# Check tool discovery in conftest.py
python -c "from tests.conftest import get_tool_path; print(get_tool_path('json-to-netencode'))"
```

### Output Format Issues
Use `netencode-pretty` to examine output:
```bash
echo '{"test": true}' | json-to-netencode | netencode-pretty
```

### Python Test Debugging
Add print statements or use pytest's `-s` flag to see output:
```bash
pytest -s -v tests/test_integration.py::test_specific_function
```

## Best Practices

1. Keep tests focused and atomic
2. Use the shared fixtures in `conftest.py`
3. Test realistic data scenarios from the README
4. Include performance considerations for large inputs
5. Verify both success and error conditions
6. Document any test-specific requirements

## Migration from BATS

This Python test suite replaced the original BATS-based tests to avoid shell escaping issues with netencode's `<` characters. Python handles the binary data and special characters directly without shell interpretation problems.