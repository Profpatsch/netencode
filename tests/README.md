# Netencode Integration Tests

This directory contains comprehensive integration tests for the netencode toolkit.

## Test Structure

The test suite is written in Python using pytest:

- **`test_integration.py`** - Core integration tests for all netencode tools (22 tests)
- **`test_readme_examples.py`** - Tests that verify all README examples work correctly (13 tests)
- **`conftest.py`** - Shared utilities and fixtures
- **`requirements.txt`** - Python dependencies

## Running Tests

### Setup

Enter the test environment:
```bash
nix-shell ./tests/shell.nix
```

### Running Tests

```bash
# Run all tests
pytest

# Run with verbose output
pytest -v

# Run specific test file
pytest test_integration.py
pytest test_readme_examples.py

# Run specific test
pytest test_readme_examples.py::TestReadmeExamples::test_basic_record_field_extraction

# Run tests matching a pattern
pytest -k "json_to_netencode"
```

## Test Categories

### Integration Tests (`test_integration.py`)

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
   - Error propagation

4. **Tool Compatibility**
   - Cross-tool data format compatibility
   - Proper netencode generation and consumption

5. **Edge Cases**
   - Empty input handling
   - Unicode support
   - Special characters in field names and values

### README Examples (`test_readme_examples.py`)

Tests for every example mentioned in the README to ensure documentation accuracy:

- Binary data construction with `printf`
- Record field extraction
- Environment variable integration
- JSON escaping vs netencode simplicity
- Shell primitive construction
- Pipeline data transformation
- Type safety demonstrations
- Length-prefixed streaming advantages
- Boolean representation as tagged units
- GitHub API processing (with network fallback)

## Advantages of Python Tests

### Key Features

1. **No Shell Escaping Issues**: Direct string handling without shell interpretation of `<` characters
2. **Clear Error Messages**: Detailed assertion failures with context
3. **Easy Debugging**: Simple to add print statements and inspect intermediate values
4. **Reliable Subprocess Handling**: Python's subprocess module handles stdin/stdout correctly

### Enhanced Features

1. **Organized Test Structure**: Logical grouping into test classes
2. **Shared Utilities**: Common functions for tool execution and assertions
3. **Fixtures**: Reusable test data and setup
4. **Network Testing**: Proper handling of GitHub API tests with fallbacks
5. **Flexible Test Selection**: Run specific tests or patterns easily

## Test Data

The tests use realistic netencode examples that match the actual tool output format:

- `{29:<3:age|i:30,<4:name|t5:Alice,}` - Record with age and name
- `{27:<3:age|i:25,<4:name|t3:Bob,}` - Alternative record
- `t5:Alice,` - Simple text value
- `<4:true|u,` - Boolean as tagged unit

## Environment Variables

The test environment sets up these variables for tool access:

- `JSON_TO_NETENCODE` - Path to json-to-netencode tool
- `NETENCODE_FILTER` - Path to netencode-filter tool
- `NETENCODE_RECORD_GET` - Path to netencode-record-get tool
- `ENV_TO_NETENCODE` - Path to env-to-netencode tool
- `NETENCODE_TO_ENV` - Path to netencode-to-env tool
- `NETENCODE_PRETTY` - Path to netencode-pretty tool

## Contributing

When adding new functionality:

1. Add integration tests to `test_integration.py`
2. If the feature is documented in README, add tests to `test_readme_examples.py`
3. Use the `run_tool()` helper function for consistent tool execution
4. Follow the existing test structure and naming conventions
