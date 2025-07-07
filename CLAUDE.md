# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is **netencode**, a data serialization format and library implementation. It's a binary format that's human-readable for debugging, type-safe, and designed for easy generation and parsing across languages.

### Key Format Features
- Scalars: unit, booleans, numbers (with explicit bit sizes), text (UTF-8), binary data
- Composite: tagged values (sum types), records (key-value maps), lists
- Length-prefixed variable data for efficient parsing
- ASCII prefixes make format partially human-readable

### Multi-language Implementation
- **Haskell** (`lib-haskell/`): Type-safe implementation with strong ADTs
- **Rust** (`lib-rust/`): Performance-oriented with both owned (`T`) and borrowed (`U`) representations
- **Python** (`lib-python/`): Generator library with standardized API
- **Nix** (`lib-nix/`): Generator functions with unified API
- **Nix**: Primary build system orchestrating all language implementations

### CLI Tools (built by Nix)
- `netencode-pretty`: Format pretty-printer
- `netencode-plain`: Extract scalar values from netencode as plain text
- `netencode-filter`: Filter netencode records by field values
- `netencode-record-get`: Extract record fields
- `netencode-to-env`: Execute commands with record fields as environment
- `env-to-netencode`: Convert environment to netencode record
- `json-to-netencode`: Convert JSON to netencode format
- `netencode-mustache`: Template rendering

## Specialist Agent System

**IMPORTANT**: For specialized tasks, delegate to expert agents in the `.claude/` directory:

### Agent Dispatch Rules

**Git Operations** → **ALWAYS delegate to `.claude/CLAUDE-git.md`**
- Creating any git commit
- Analyzing git history
- Commit message formatting
- Git workflow questions

**Testing Workflows** → **Delegate to `.claude/CLAUDE-test.md`**
- Running complex test suites
- Creating custom test scripts
- Test data generation
- Cross-language test coordination

**Build System Issues** → **Delegate to `.claude/CLAUDE-build.md`**
- Multi-language build coordination
- Build system configuration
- Dependency management
- Development environment setup

**Documentation Updates** → **Delegate to `.claude/CLAUDE-docs.md`**
- README maintenance
- API documentation synchronization
- Example validation
- Cross-language documentation consistency

**Debugging & Troubleshooting** → **Delegate to `.claude/CLAUDE-debug.md`**
- Build failures
- Runtime errors
- Performance issues
- Cross-language compatibility problems

### Agent Invocation Pattern
```
Use the Task tool to spawn specialist agent:
"[Task description]. Follow the specialized guidance in .claude/CLAUDE-[agent].md."
```

**Example**:
```
Task: "Create a git commit for the new feature. Follow the commit message format and guidelines in .claude/CLAUDE-git.md."
```

## Quick Development Commands

### Essential Commands
```bash
# Run all tests
nix-build -A netencode-tests

# Build everything
nix-build

# Development shell
nix develop

# Run tools
nix run .#netencode-pretty
nix run .#python    # Python REPL
nix run .#rust      # Rust workspace creator
```

### Testing (75 tests: 73 offline + 2 network)
**For complex testing workflows, delegate to `.claude/CLAUDE-test.md`**

```bash
# Basic test commands
nix-build -A netencode-tests
nix-build -A netencode-tests --arg pytestArgs '"-v"'
```

### Build Systems
**For complex build issues, delegate to `.claude/CLAUDE-build.md`**

```bash
# Primary build system (Nix)
nix-build                    # Build all
nix-build -A netencode-hs    # Haskell library
nix-build -A netencode-rs    # Rust library

# Alternative builds
cabal build all              # Haskell
cargo build                  # Rust (in lib-rust/)
```

## Project Structure

```
netencode/
├── lib-haskell/     # Haskell implementation (type-safe)
├── lib-rust/        # Rust implementation (performance-oriented)
├── lib-python/      # Python implementation (generator library)
├── lib-nix/         # Nix implementation (build integration)
├── exec-helpers/    # Rust utilities for CLI tools
├── tests/           # Automated test suite
├── man/             # Manual pages
├── .claude/         # Specialist agent configurations
└── default.nix      # Primary build orchestration
```

### Unified API
All generator libraries use identical function names:
- `unit()`, `natural()`, `integer()`, `boolean()`, `text()`, `binary()`
- `tag()`, `record()`, `list()`
- Cross-language compatibility ensures identical output

## Git Workflow

**CRITICAL**: All git operations must be delegated to `.claude/CLAUDE-git.md`

### Git Agent Responsibilities
- **70-character limit** enforcement (frequently forgotten!)
- **Explain WHY** changes were made, not just what
- Conventional commit format with netencode-specific patterns
- Consistent git history practices

### Quick Reference
```
feat(scope): ✨ description under 70 characters

Explanation of WHY changes were made, focusing on reasoning
and problem context rather than listing what was changed.

🤖 Generated with [Claude Code](https://claude.ai/code)

Co-Authored-By: Claude <noreply@anthropic.com>
```

**Always delegate git operations to the specialist agent!**

## Task Management

### .NOTES File
The repository contains a `.NOTES` file for documenting TODOs and future work items:
- Track ongoing tasks and future improvements
- Document side-tasks discovered during unrelated work
- Commit along with other changes to maintain task history
- Remove completed items to avoid stale references

### TodoWrite Tool
Use the TodoWrite tool for complex multi-step tasks to:
- Track progress across multiple related changes
- Break down complex tasks into manageable steps
- Provide visibility into work progress
- Ensure no steps are forgotten

## Critical Workarounds

### Bash Tool File Redirection Bug
**Issue**: The Bash tool has a bug with the `<` character where it incorrectly adds `/dev/null` redirection.

**Workaround**: Use pipe redirection instead:
```bash
# Don't use: ./tool < file.txt
# Use instead: cat file.txt | ./tool
```

**For Claude Code**: AVOID suggesting bash commands with `<` character. Always use `cat file |` or `printf ... |` patterns instead.

## Development Workflow

1. **Identify Task Type**: Determine if task needs specialist agent
2. **Delegate to Agent**: Use Task tool to spawn appropriate specialist
3. **Use TodoWrite**: For complex multi-step tasks
4. **Update .NOTES**: Document side-tasks and observations
5. **Always Use Git Agent**: For any git operations

## Agent Integration Benefits

- **Specialized Expertise**: Each agent focuses on specific domain knowledge
- **Consistent Practices**: Git agent ensures consistent commit practices
- **Faster Context**: Agents load only relevant context for their domain
- **Maintainable**: Easy to update specialist knowledge without affecting others
- **Reliable**: Reduces errors through specialized validation

# important-instruction-reminders
Do what has been asked; nothing more, nothing less.
NEVER create files unless they're absolutely necessary for achieving your goal.
ALWAYS prefer editing an existing file to creating a new one.
NEVER proactively create documentation files (*.md) or README files. Only create documentation files if explicitly requested by the User.