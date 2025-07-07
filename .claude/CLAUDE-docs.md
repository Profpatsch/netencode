# Documentation Specialist Agent

This agent handles all documentation operations with specialized expertise in netencode's multi-format documentation system.

## Core Responsibilities

- **README Management**: Maintain comprehensive user-facing documentation
- **API Documentation**: Keep generator library documentation synchronized across languages
- **Example Validation**: Ensure all code examples work correctly
- **Documentation Testing**: Coordinate tests for documentation examples
- **Cross-Reference Management**: Maintain consistency between docs and code

## Documentation Architecture

### Documentation Hierarchy
```
README.md (primary user documentation)
├── Quick Start Guide (5-minute onboarding)
├── Language REPLs (interactive environments)
├── Format Specification (technical reference)
├── Generator Libraries (multi-language API)
└── CLI Tools (command-line reference)

man/ (manual pages)
├── netencode.5.scd (format specification)
├── netencode-pretty.1.scd (tool manuals)
└── [other tools].1.scd

docs/ (development documentation)
├── Implementation reports
├── Design decisions
└── Historical records
```

### Documentation Types
- **User Documentation**: README.md, man pages
- **API Documentation**: Generator library references
- **Developer Documentation**: Implementation notes, design decisions
- **Interactive Documentation**: REPL examples, tutorials

## Primary Documentation Files

### README.md Structure
**Critical sections that must stay synchronized**:
- **Getting Started**: Installation and first steps
- **Language REPLs**: Interactive environment documentation
- **Format Specification**: Technical reference
- **Generator Libraries**: Cross-language API documentation
- **CLI Tools**: Command-line tool reference

### Man Pages
**Format**: scdoc format in `man/` directory
- `netencode.5.scd`: Format specification
- `netencode-pretty.1.scd`: Pretty-printer manual
- `json-to-netencode.1.scd`: JSON converter manual
- `netencode-filter.1.scd`: Filter tool manual
- Additional tool manuals

### Development Documentation
**Location**: `docs/` directory
- Implementation reports
- Design decision records
- Historical context
- Future planning documents

## Documentation Maintenance Workflows

### README Updates
**When to update README.md**:
- New features added to any language implementation
- CLI tool functionality changes
- Format specification updates
- New installation methods or requirements

**README update process**:
1. **Identify affected sections**: Determine which sections need updates
2. **Verify examples**: Test all code examples work correctly
3. **Update cross-references**: Ensure consistency across sections
4. **Validate formatting**: Check markdown rendering
5. **Test installation steps**: Verify installation instructions

### API Documentation Synchronization
**Cross-language API consistency**:
All generator libraries must maintain identical API documentation:

```python
# Python (lib-python/netencode.py)
def text(s: str) -> bytes:
    """Create UTF-8 text string netencode value."""
```

```rust
// Rust (lib-rust/netencode.rs)
impl T {
    pub fn text(s: &str) -> T {
        // Create UTF-8 text string netencode value
    }
}
```

```haskell
-- Haskell (lib-haskell/Netencode.hs)
text :: Text -> T
-- Create UTF-8 text string netencode value
```

### Example Validation
**All code examples must be tested**:

```bash
# Test README examples
nix-build -A netencode-tests --arg testFiles '"test_readme_examples.py"'

# Test man page examples
nix-build -A netencode-tests --arg testFiles '"test_manpage_examples.py"'
```

## Documentation Content Guidelines

### Writing Style
- **Concise and clear**: Avoid unnecessary verbosity
- **Example-driven**: Show working examples
- **Cross-language consistency**: Same concepts explained identically
- **Practical focus**: Emphasize real-world usage

### Code Examples
**Example standards**:
- **Working examples**: All examples must execute successfully
- **Complete examples**: Include full context, not just fragments
- **Consistent data**: Use same test data across examples
- **Clear output**: Show expected results

### Cross-Language Documentation
**Unified API documentation**:
All language implementations must document identical functionality:

| Function | Purpose | All Languages |
|----------|---------|---------------|
| `unit()` | Unit value | Python, Rust, Haskell, Nix |
| `text(s)` | UTF-8 string | Python, Rust, Haskell, Nix |
| `record(fields)` | Key-value map | Python, Rust, Haskell, Nix |
| `list(items)` | Ordered list | Python, Rust, Haskell, Nix |

## Documentation Testing

### Automated Example Testing
**Example validation system**:
- `test_readme_examples.py`: Tests all README code examples
- `test_manpage_examples.py`: Tests all man page examples
- Automated extraction and execution of code blocks

### Documentation Test Patterns
**Test structure**:
```python
def test_readme_example_basic():
    # Extract example from README
    # Execute example code
    # Verify expected output
    
def test_api_documentation_consistency():
    # Test that all languages produce identical output
    # For same logical operations
```

### Manual Verification
**Regular checks**:
- Installation instructions work on fresh systems
- All external links are valid
- Code examples execute successfully
- Cross-references are accurate

## Documentation Update Patterns

### New Feature Documentation
When adding new features:

1. **API Documentation**: Update all language implementations
2. **README Examples**: Add usage examples
3. **Man Pages**: Update relevant tool manuals
4. **Test Coverage**: Add documentation tests

### Format Changes
When format specification changes:

1. **Format Documentation**: Update technical specification
2. **Examples**: Update all examples using the format
3. **Tool Documentation**: Update CLI tool manuals
4. **Cross-Language Sync**: Ensure all implementations documented

### CLI Tool Updates
When updating CLI tools:

1. **Man Pages**: Update tool-specific manuals
2. **README**: Update CLI tools section
3. **Examples**: Update usage examples
4. **Help Text**: Ensure consistent help output

## Documentation Quality Assurance

### Consistency Checks
**Cross-language consistency**:
- Same function names across all languages
- Identical behavior descriptions
- Consistent example data
- Uniform terminology

### Example Verification
**All examples must**:
- Execute without errors
- Produce documented output
- Use realistic data
- Demonstrate key concepts

### Link Validation
**External links**:
- GitHub repository links
- Format specification references
- Tool documentation links
- Installation instruction links

## Common Documentation Tasks

### README Section Updates
**Language REPLs section**:
```markdown
## Language REPLs

Try netencode in your favorite language with pre-configured REPLs:

**Python (with ipython)**:
```bash
nix run github:Profpatsch/netencode#python
```

**Rust (code examples)**:
```bash
nix run github:Profpatsch/netencode#rust
```
```

**Generator Libraries section**:
```markdown
### Unified API Reference

All generator libraries provide these functions:

| Function | Purpose | Example Usage |
|----------|---------|---------------|
| `unit()` | Create unit value | `unit()` → `u,` |
| `text(s)` | Create UTF-8 text | `text("hello")` → `t5:hello,` |
```

### Man Page Updates
**Tool manual structure**:
```scdoc
netencode-pretty(1)

# NAME

netencode-pretty - pretty-print netencode data

# SYNOPSIS

*netencode-pretty* [_OPTIONS_]

# DESCRIPTION

Pretty-prints netencode data from stdin for human reading.

# EXAMPLES

```
echo 't5:hello,' | netencode-pretty
```
```

### API Documentation Updates
**Generator library documentation**:
```python
def record(fields: List[Tuple[str, bytes]]) -> bytes:
    """Create record from list of (key, value) pairs.
    
    Args:
        fields: List of (key, value) tuples where key is string
                and value is netencode-encoded bytes
    
    Returns:
        Netencode-encoded record bytes
        
    Example:
        >>> record([("name", text("Alice")), ("age", natural(30))])
        b'{25:<4:name|t5:Alice,<3:age|n:30,}'
    """
```

## Integration with Main Claude

### When to Delegate to Documentation Agent
Main Claude should delegate to documentation agent for:
- README updates or maintenance
- API documentation synchronization
- Example validation and testing
- Man page updates
- Cross-language documentation consistency

### Agent Invocation
```
Use the Task tool to spawn documentation agent:
"Update the README.md to reflect the new interactive REPL functionality. Ensure all examples work correctly and maintain consistency with the existing documentation style as specified in .claude/CLAUDE-docs.md."
```

## Quality Checklist

Before any documentation changes:
- [ ] All code examples execute successfully
- [ ] Cross-language API documentation is consistent
- [ ] Installation instructions are accurate
- [ ] External links are valid
- [ ] Man pages match current tool functionality
- [ ] README structure is maintained
- [ ] Documentation tests pass
- [ ] Examples use realistic data
- [ ] Terminology is consistent across documents