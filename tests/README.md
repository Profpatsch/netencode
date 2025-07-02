# Netencode Integration Tests

This directory contains comprehensive integration tests for the netencode toolkit.

## Test Structure

The test suite is written in Python using pytest and split into offline and network tests:

- **`test_integration.py`** - Core integration tests for all netencode tools (36 tests)
- **`test_readme_examples.py`** - Tests that verify README examples work correctly (15 tests, offline)
- **`test_netencode_py.py`** - Python module unit tests (22 tests)
- **`test_network.py`** - Network-requiring tests (2 tests, GitHub API and nix flake)
- **`conftest.py`** - Shared utilities and fixtures
- **`netencode_py.py`** - Python module for constructing netencode data
- **`pytest.ini`** - Pytest configuration with network markers

## Running Tests

### Automated Testing (Recommended)

```bash
# Run all offline tests automatically (no network required)
nix-build -A netencode-tests

# Run specific test file
nix-build -A netencode-tests --arg testFiles '"test_integration.py"'

# Run tests matching a pattern
nix-build -A netencode-tests --arg pytestArgs '"-k json"'

# Run with verbose output
nix-build -A netencode-tests --arg pytestArgs '"-v"'
```

### Manual Testing (Development)

Enter the test environment:
```bash
nix-shell tests/shell.nix
```

Then run tests manually:
```bash
# Run all offline tests
pytest -m "not network"

# Run all tests (including network tests)
pytest

# Run with verbose output
pytest -v

# Run specific test file
pytest test_integration.py
pytest test_readme_examples.py
pytest test_netencode_py.py

# Run network tests only
pytest test_network.py

# Run specific test
pytest test_readme_examples.py::TestReadmeExamples::test_basic_record_field_extraction

# Run tests matching a pattern
pytest -k "json"
```

## Test Categories

### Integration Tests (`test_integration.py`) - 36 tests

1. **JSON to Netencode Conversion**
   - Simple objects, arrays, booleans, numbers, null values
   - Error handling for malformed JSON

2. **Netencode Filtering**
   - Filter by text, number, and boolean fields
   - Handle missing fields gracefully
   - Pass through non-record values

3. **Pipeline Integration**
   - Complete JSON → netencode → filter → extract workflows
   - Multi-record processing
   - Field ordering preservation

4. **Tool Compatibility**
   - Cross-tool data format compatibility
   - Python-generated records work with CLI tools

5. **Netencode Plain Tool**
   - Extract scalar values as plain text
   - Handle different data types correctly

6. **Edge Cases**
   - Empty input handling
   - Unicode support
   - Special characters in field names and values
   - Environment integration

### README Examples (`test_readme_examples.py`) - 15 tests (offline)

Tests for every example mentioned in the README to ensure documentation accuracy:

- Binary data construction with `printf`
- Record field extraction and plain text conversion
- Environment variable integration
- JSON escaping vs netencode simplicity
- Shell primitive construction
- Pipeline data transformation
- Type safety demonstrations
- Length-prefixed streaming advantages
- Boolean representation as tagged units
- GitHub API processing (simplified, no network)

### Python Module Tests (`test_netencode_py.py`) - 22 tests

Unit tests for the Python netencode construction module:

- Basic type constructors (unit, natural, integer, boolean, text, binary)
- Composite types (tags, records, lists)
- Field ordering and sorting
- Complex nested structures
- Unicode handling
- Error handling and validation

### Network Tests (`test_network.py`) - 2 tests

Tests requiring internet connectivity (separated for nix-build):

- GitHub API data processing with real network calls
- Nix flake app examples that may need to build from scratch

## Test Infrastructure

### Nix-Build Testing

The preferred testing approach uses `nix-build -A netencode-tests`:

- **Isolated Environment**: Tests run in Nix sandbox without network access
- **Reproducible**: Same environment every time, no dependency conflicts
- **Fast**: Pre-built tools, no setup time
- **Configurable**: Can specify test files and pytest arguments
- **Automated**: Fails build if tests fail, perfect for CI

### Manual Testing

The `tests/shell.nix` provides a lightweight development environment:

- **Network Tests**: Includes tests that require internet connectivity
- **Development**: Easy to add debug prints and iterate on tests
- **All Tools Available**: Uses the combined netencode package

## Test Features

### Python Advantages

1. **No Shell Escaping Issues**: Direct string handling without shell interpretation
2. **Clear Error Messages**: Detailed assertion failures with context
3. **Easy Debugging**: Simple to add print statements and inspect values
4. **Reliable Subprocess Handling**: Python's subprocess module handles stdin/stdout correctly
5. **Organized Structure**: Logical grouping into test classes with shared utilities

### Python Netencode Module

The `netencode_py.py` module provides utilities for constructing test data:

- `ne.unit()` → `b"u,"`
- `ne.text("hello")` → `b"t5:hello,"`
- `ne.natural(42)` → `b"n:42,"`
- `ne.simple_record(name=ne.text("Alice"))` → complete record with sorted fields
- `ne.record_ordered()` → record with explicit field ordering

### Tool Discovery

Tests work in both environments:

- **Nix-build**: Uses environment variables (e.g., `$JSON_TO_NETENCODE`)
- **Shell.nix**: Falls back to PATH lookup via `which` command

## Contributing

When adding new functionality:

1. **Integration tests** → `test_integration.py` (for tool behavior)
2. **README examples** → `test_readme_examples.py` (for documentation accuracy)
3. **Python utilities** → `test_netencode_py.py` (for netencode construction)
4. **Network tests** → `test_network.py` (if requires internet)
5. Use the `run_tool()` helper function for consistent tool execution
6. Follow existing test structure and naming conventions
7. Test with `nix-build -A netencode-tests` before submitting
