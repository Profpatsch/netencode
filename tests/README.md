# Netencode Integration Tests

This directory contains comprehensive integration tests for the netencode toolkit.

## Test Structure

The test suite is written in Python using pytest and split into offline and network tests:

- **`test_integration.py`** - Core integration tests for all netencode tools (36 tests)
- **`test_readme_examples.py`** - Tests that verify README examples work correctly (15 tests, offline)
- **`test_manpage_examples.py`** - Tests that verify man page examples work correctly (37 tests, offline)
- **`test_netencode_py.py`** - Python module unit tests (22 tests)
- **`test_pretty_printer.py`** - Comprehensive pretty-printer formatting tests (not integrated into nix build)
- **`test_network.py`** - Network-requiring tests (2 tests, GitHub API and nix flake)
- **`conftest.py`** - Shared utilities and fixtures
- **`pytest.ini`** - Pytest configuration with network markers
- **`GENERATOR_TEST_SPEC.md`** - Cross-language implementation test specification

Note: The Python netencode module is now located at `lib-python/netencode.py` and uses standardized API function names.

## Running Tests

### Automated Testing (Recommended)

```bash
# Run all offline tests automatically (no network required)
# Runs 110 tests: 36 integration + 15 readme + 37 manpage + 22 python (offline)
nix-build -A netencode-tests

# Run specific test file
nix-build -A netencode-tests --arg testFiles '"test_integration.py"'
nix-build -A netencode-tests --arg testFiles '"test_manpage_examples.py"'

# Run tests matching a pattern
nix-build -A netencode-tests --arg pytestArgs '"-k json"'
nix-build -A netencode-tests --arg pytestArgs '"-k manpage"'

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
pytest test_manpage_examples.py
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

### Man Page Examples (`test_manpage_examples.py`) - 37 tests (offline)

Tests for every example mentioned in the man pages to ensure documentation accuracy:

**Format Specification (`netencode.5`)**:
- Complete record examples with proper length prefixes
- Boolean representation as tagged units
- Number format validation (integers vs naturals)

**Tool-Specific Examples**:
- **`netencode-pretty.1`**: Pretty-printer output formatting
- **`netencode-plain.1`**: Scalar value extraction as plain text
- **`netencode-record-get.1`**: Record field extraction with complex data
- **`netencode-to-env.1`**: Environment variable conversion and command execution
- **`netencode-filter.1`**: Data filtering by field values (has known bug - outputs nothing)
- **`netencode-mustache.1`**: Template rendering with records and lists
- **`json-to-netencode.1`**: JSON conversion with complex nested structures
- **`env-to-netencode.1`**: Environment capture and record creation

**Key Features Tested**:
- Exact netencode format compliance with actual tool output
- Cross-tool data compatibility and pipeline integration
- Documentation examples match real tool behavior
- Complex nested structures (records in lists, lists in records)
- Unicode handling in field names and values
- Template processing with mustache integration

### Python Module Tests (`test_netencode_py.py`) - 22 tests

Unit tests for the Python netencode construction module using the standardized API:

- **Basic type constructors**: `unit()`, `natural()`, `integer()`, `boolean()`, `text()`, `binary()`
- **Composite types**: `tag()`, `record()`, `list()`
- **Field ordering and sorting**: Records maintain insertion order or sort alphabetically
- **Complex nested structures**: Records containing lists, lists containing records
- **Unicode handling**: Proper UTF-8 encoding for text values
- **Error handling and validation**: Type checking and boundary conditions

### Network Tests (`test_network.py`) - 2 tests

Tests requiring internet connectivity (separated for nix-build):

- GitHub API data processing with real network calls
- Nix flake app examples that may need to build from scratch

### Pretty Printer Tests (`test_pretty_printer.py`) - Comprehensive formatting validation

Standalone comprehensive tests for the netencode pretty-printer (not integrated into nix build):

**Scalar Type Formatting**:
- Unit, natural numbers, signed integers, booleans
- Large number handling and edge cases
- Text formatting with length indicators and truncation
- Binary data detection and hexdump formatting

**Text Processing**:
- Short text (no truncation)
- Medium text at 40-character threshold
- Long text truncation with length indicators
- Unicode support (multi-byte characters, emojis)
- Special characters (quotes, newlines, control chars)

**Binary Data Handling**:
- Small binary (single-line hex)
- Large binary (multi-line hexdump with ASCII column)
- UTF-8 vs non-UTF-8 detection
- Hexdump formatting with offset columns

**Complex Structure Formatting**:
- Tagged values and nested tags
- Record formatting with proper indentation
- List formatting and structure
- Mixed content types
- Deep nesting scenarios
- Realistic data structures

**Output Format Verification**:
- Exact formatting compliance
- Indentation consistency (2-4 spaces per level)
- Line breaking behavior
- Complete format validation with precise assertions

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

### Tool Discovery

Tests work in both environments:

- **Nix-build**: Uses environment variables (e.g., `$JSON_TO_NETENCODE`)
- **Shell.nix**: Falls back to PATH lookup via `which` command

## Contributing

When adding new functionality:

1. **Integration tests** → `test_integration.py` (for tool behavior)
2. **README examples** → `test_readme_examples.py` (for documentation accuracy)
3. **Man page examples** → `test_manpage_examples.py` (for documentation validation)
4. **Python utilities** → `test_netencode_py.py` (for netencode construction)
5. **Network tests** → `test_network.py` (if requires internet)
6. **Cross-language tests** → Reference `GENERATOR_TEST_SPEC.md` for consistent implementation across languages
7. Use the `run_tool()` helper function for consistent tool execution
8. Follow existing test structure and naming conventions
9. Test with `nix-build -A netencode-tests` before submitting

### Generator Test Specification

The `GENERATOR_TEST_SPEC.md` file defines a comprehensive test suite that all netencode language implementations (Haskell, Rust, Python, Nix) must pass. It includes:

- **309 test cases** covering basic types, composite types, complex scenarios, and error cases
- **Unified API validation** ensuring consistent function names across languages
- **Binary format compliance** with exact expected outputs for each test case
- **Edge case coverage** including Unicode, large numbers, and deeply nested structures
- **Error handling specifications** for invalid inputs and boundary conditions

This specification ensures that all language implementations produce identical netencode output for the same logical data, maintaining cross-language compatibility.
