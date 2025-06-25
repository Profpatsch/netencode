---
title: Netencode Integer Simplification
date: 2025-06-25
type: Breaking Change
status: Implemented
---

# Netencode Integer Simplification

## Overview

This document describes the major simplification of the netencode format that reduces complexity while maintaining all core functionality and improving both human readability and implementation simplicity.

## Changes Made

### Unified Number Types

The format previously used multiple size-specific number types: `N1`, `N3`, `N6` for naturals and `I3`, `I6` for integers. These encoded as `n1:0,`, `n3:255,`, `n6:1000000,`, `i6:-42,` and required 18+ parser variants handling different bit sizes.

The format now uses two unified 64-bit types: `N(u64)` for naturals and `I(i64)` for integers. These encode as simple `n:42,` and `i:-42,` with just 2 parser variants.

### Semantic Boolean Representation

Booleans were represented as `N1(bool)` encoded as `n1:0,` (false) and `n1:1,` (true). This required knowledge that N1 represents booleans.

Booleans are now tagged units: `<5:false|u,` and `<4:true|u,`. This representation uses the existing tag infrastructure.

## Reasoning and Benefits

### Reduced Complexity

The original format had 7 different number variants requiring complex size-specific parsing logic. The new format has only 2 number types, simplifying parser implementation from 18+ type-specific parsers to 2 simple parsers. Encoder logic moves from multiple size branches to unified 64-bit handling.

### Universal 64-bit Support

Every modern programming language has native support for 64-bit integers. This eliminates the need for complex size-specific handling and allows implementations to use standard integer types directly. Languages like JavaScript that lack native 64-bit integers still have library support that covers this range effectively.

### Improved Human Readability

Size-specific confusion is eliminated. Where the old format used `n3:255,` vs `n6:255,` for identical values with different representations, the new format uses a single clear `n:255,` representation.

Booleans change from the old `n1:0,` and `n1:1,` which required knowing the N1 = boolean convention to `<5:false|u,` and `<4:true|u,`.

### Practical Advantages

The format becomes more readable for debugging. There is no need to remember size mapping (was `n3` 8-bit? 16-bit?). Boolean values use clear names.

Tooling becomes simpler with pretty printers needing fewer cases, simpler validation tools, and clearer documentation. Error messages become more descriptive (`expected number` vs `expected n6`) with no confusion about size mismatches.

## Backward Compatibility

This is a breaking change by design. The complexity reduction benefits outweigh compatibility concerns. The old format was complex for limited benefit. Size-specific types provided little value in practice. The migration path is straightforward since most numbers can be represented in 64 bits. Boolean migration represents a semantic improvement.

## Implementation Status

The Rust implementation is complete with all tests passing. The Haskell implementation is complete and building. The pretty printer is updated and working. Test coverage verifies all existing functionality.

## Future Considerations

This simplification makes netencode easier to implement in new languages, more approachable for new users, simpler to debug and inspect, more consistent with modern serialization formats, and better suited for human-readable use cases.
