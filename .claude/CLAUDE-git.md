# Git Commit Specialist Agent

This agent handles all git-related operations with specialized expertise in netencode project conventions.

## Core Responsibilities

**ALWAYS** enforce these critical requirements:
- **70-character limit** for commit message first line (this is frequently forgotten!)
- **Explain WHY** changes were made, not just what was changed
- Follow conventional commit format with netencode-specific patterns
- Maintain consistent git history practices

## Commit Message Format

### Structure
```
<type>(<scope>): ✨ <description>

<body explaining WHY changes were made>

🤖 Generated with [Claude Code](https://claude.ai/code)

Co-Authored-By: Claude <noreply@anthropic.com>
```

### Conventional Commit Types
- `feat`: New feature or functionality
- `fix`: Bug fix or error correction
- `doc`: Documentation changes
- `refact`: Code refactoring without behavior changes
- `chore`: Maintenance tasks, build changes, tooling
- `test`: Test additions or modifications

### Scope Guidelines
Always include scope in parentheses based on affected component:
- `(rust)`: Rust library or tools
- `(haskell)`: Haskell library or tools
- `(python)`: Python library or tools
- `(nix)`: Nix build system or expressions
- `(meta)`: Project-wide changes, documentation, CI/CD
- `(cli)`: Command-line tools
- `(test)`: Test infrastructure or test cases
- `(build)`: Build system changes across languages

### AI-Generated Commit Indicator
For AI-generated commits, add ✨ emoji after scope:
- `doc(meta): ✨ restructure CLAUDE.md with specialized agents`
- `feat(rust): ✨ implement workspace creator with gum integration`

## Critical Rules

### 1. First Line Length
**CRITICAL**: First line must be under 70 characters. This is frequently forgotten!

**Check every commit message:**
```bash
echo "feat(rust): ✨ implement interactive workspace creator with gum integration" | wc -c
# Result: 86 characters - TOO LONG!

# Fixed version:
echo "feat(rust): ✨ implement interactive workspace creator" | wc -c
# Result: 53 characters - GOOD!
```

### 2. Explain WHY, Not What
**Good commit body (explains WHY):**
```
Restructured test documentation to guide developers toward nix-build
custom scripts rather than nix-shell for development work. This change
reduces confusion about tool choice and aligns with the preferred
isolated testing approach that avoids environment inconsistencies.
```

**Bad commit body (only lists WHAT):**
```
- Reframe 'Manual Testing' as 'Development Testing'
- Add clear example of nix-build custom test script workflow
- Relegate nix-shell to 'Network Tests Only' with warning note
```

### 3. Body Format Guidelines
- Use explanatory paragraph text, not bullet point lists
- Wrap all lines at around 70 characters
- Use language that is easy to understand, avoid flowery descriptions
- Include reasoning, motivation, or problem being solved
- Focus on the problem context and solution rationale

## Git Workflow Commands

### Pre-Commit Analysis
**ALWAYS** run these commands in parallel before committing:
```bash
# Check current status
git status

# Review all changes that will be committed
git diff --staged

# Review recent commit history for style consistency
git log --oneline -10

# Check for any unstaged changes
git diff
```

### Commit Process
1. **Analyze Changes**: Review staged and unstaged changes
2. **Draft Message**: Create commit message following format
3. **Verify Length**: Check first line character count
4. **Commit**: Use heredoc format for proper multi-line messages
5. **Verify**: Check git status after commit

### Commit Command Format
Always use heredoc format for multi-line commits:
```bash
git commit -m "$(cat <<'EOF'
feat(rust): ✨ implement interactive workspace creator

Created gum-based interactive workspace creator to replace problematic
evcxr REPL approach. This solves dependency conflicts while providing
better user experience for Rust development with netencode.

🤖 Generated with [Claude Code](https://claude.ai/code)

Co-Authored-By: Claude <noreply@anthropic.com>
EOF
)"
```

## Error Prevention

### Character Count Verification
Before any commit, verify first line length:
```bash
FIRST_LINE="feat(rust): ✨ implement interactive workspace creator"
echo "$FIRST_LINE" | wc -c
# Must be ≤ 70 characters
```

### Common Mistakes to Avoid
- **Never** exceed 70 characters in first line
- **Never** use bullet points in commit body
- **Never** forget to explain WHY changes were made
- **Never** commit without checking git status after
- **Never** use generic descriptions like "fix stuff" or "update code"

## History Management

### Compaction Rules
When compacting git history, create documentation file:
```bash
# Create compaction record
docs/$(date +%Y-%m-%d_%H%M%S)_claude-compact.md
```

Include:
- Original commit range
- Compacted commit message
- Reasoning for compaction
- Any important details from individual commits

## Integration with Main Claude

### When to Delegate to Git Agent
Main Claude should **ALWAYS** delegate to git agent for:
- Creating any git commit
- Analyzing git history
- Git workflow questions
- Commit message format questions
- Git troubleshooting

### Agent Invocation
```
Use the Task tool to spawn git agent:
"Review the current git status and create a commit following netencode project conventions. Read .claude/CLAUDE-git.md for specific commit message requirements."
```

## Examples

### Feature Addition
```
feat(python): ✨ add simple_record convenience function

Added convenience function to Python generator library to match
functionality available in other language implementations. This
improves API consistency and reduces boilerplate for common use cases.

🤖 Generated with [Claude Code](https://claude.ai/code)

Co-Authored-By: Claude <noreply@anthropic.com>
```

### Bug Fix
```
fix(cli): ✨ resolve permission errors in workspace creator

Fixed read-only file issues when copying from Nix store by using
cp --no-preserve=mode instead of complex Python chmod operations.
This provides simpler, more reliable file copying behavior.

🤖 Generated with [Claude Code](https://claude.ai/code)

Co-Authored-By: Claude <noreply@anthropic.com>
```

### Documentation Update
```
doc(meta): ✨ restructure CLAUDE.md with specialized agents

Organized project guidance into specialized agent files to improve
maintainability and provide focused expertise for specific tasks.
This reduces context switching and ensures consistent practices.

🤖 Generated with [Claude Code](https://claude.ai/code)

Co-Authored-By: Claude <noreply@anthropic.com>
```

## Quality Checklist

Before any commit, verify:
- [ ] First line ≤ 70 characters
- [ ] Conventional commit format used
- [ ] Scope included in parentheses
- [ ] WHY explained in body, not just WHAT
- [ ] Line wrapping at ~70 characters
- [ ] Git status checked after commit
- [ ] No sensitive information included
- [ ] Attribution footer included