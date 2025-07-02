# Netencode Generator Test Specification

This document defines test cases that all netencode language implementations must pass.
Each test case is defined using the unified API function names and expected binary output.

## Format

Each test case follows this structure:
```
### Test Name
- **Category**: Basic Types | Composite Types | Complex Scenarios | Error Cases
- **Input**: Function calls using unified API
- **Expected**: Binary output in hex or bytes
- **Description**: What this test validates
```

## Basic Types

### Unit Value
- **Category**: Basic Types
- **Input**: `unit()`
- **Expected**: `b"u,"`
- **Description**: Validates unit value construction

### Natural Zero
- **Category**: Basic Types
- **Input**: `natural(0)`
- **Expected**: `b"n:0,"`
- **Description**: Natural number zero

### Natural Number
- **Category**: Basic Types
- **Input**: `natural(42)`
- **Expected**: `b"n:42,"`
- **Description**: Typical natural number

### Natural Max U64
- **Category**: Basic Types
- **Input**: `natural(18446744073709551615)`
- **Expected**: `b"n:18446744073709551615,"`
- **Description**: Maximum 64-bit unsigned value

### Integer Zero
- **Category**: Basic Types
- **Input**: `integer(0)`
- **Expected**: `b"i:0,"`
- **Description**: Signed integer zero

### Integer Positive
- **Category**: Basic Types
- **Input**: `integer(42)`
- **Expected**: `b"i:42,"`
- **Description**: Positive signed integer

### Integer Negative
- **Category**: Basic Types
- **Input**: `integer(-42)`
- **Expected**: `b"i:-42,"`
- **Description**: Negative signed integer

### Integer Max I64
- **Category**: Basic Types
- **Input**: `integer(9223372036854775807)`
- **Expected**: `b"i:9223372036854775807,"`
- **Description**: Maximum 64-bit signed value

### Integer Min I64
- **Category**: Basic Types
- **Input**: `integer(-9223372036854775808)`
- **Expected**: `b"i:-9223372036854775808,"`
- **Description**: Minimum 64-bit signed value

### Boolean True
- **Category**: Basic Types
- **Input**: `boolean(true)`
- **Expected**: `b"<4:true|u,"`
- **Description**: Boolean true as tagged unit

### Boolean False
- **Category**: Basic Types
- **Input**: `boolean(false)`
- **Expected**: `b"<5:false|u,"`
- **Description**: Boolean false as tagged unit

### Text Empty
- **Category**: Basic Types
- **Input**: `text("")`
- **Expected**: `b"t0:,"`
- **Description**: Empty text string

### Text Simple
- **Category**: Basic Types
- **Input**: `text("hello")`
- **Expected**: `b"t5:hello,"`
- **Description**: Simple ASCII text

### Text With Space
- **Category**: Basic Types
- **Input**: `text("Hello, World!")`
- **Expected**: `b"t13:Hello, World!,"`
- **Description**: Text with punctuation and space

### Text UTF8 Accented
- **Category**: Basic Types
- **Input**: `text("café")`
- **Expected**: `b"t5:caf\xc3\xa9,"`
- **Description**: Text with UTF-8 accented character (é = 2 bytes)

### Text UTF8 Emoji
- **Category**: Basic Types
- **Input**: `text("🌍")`
- **Expected**: `b"t4:\xf0\x9f\x8c\x8d,"`
- **Description**: Text with 4-byte UTF-8 emoji

### Text With Quotes
- **Category**: Basic Types
- **Input**: `text("He said \"hi\"")`
- **Expected**: `b"t12:He said \"hi\","`
- **Description**: Text containing quotes

### Text With Newline
- **Category**: Basic Types
- **Input**: `text("line1\nline2")`
- **Expected**: `b"t11:line1\nline2,"`
- **Description**: Text with newline character

### Binary Empty
- **Category**: Basic Types
- **Input**: `binary(b"")`
- **Expected**: `b"b0:,"`
- **Description**: Empty binary data

### Binary Simple
- **Category**: Basic Types
- **Input**: `binary(b"hello")`
- **Expected**: `b"b5:hello,"`
- **Description**: Simple binary data

### Binary With Nulls
- **Category**: Basic Types
- **Input**: `binary(b"\x00\x01\x02")`
- **Expected**: `b"b3:\x00\x01\x02,"`
- **Description**: Binary data with null bytes

## Composite Types

### Tag Simple
- **Category**: Composite Types
- **Input**: `tag("foo", unit())`
- **Expected**: `b"<3:foo|u,"`
- **Description**: Simple tagged value

### Tag Empty Name
- **Category**: Composite Types
- **Input**: `tag("", integer(42))`
- **Expected**: `b"<0:|i:42,"`
- **Description**: Tag with empty name

### Tag With Value
- **Category**: Composite Types
- **Input**: `tag("Some", text("value"))`
- **Expected**: `b"<4:Some|t5:value,"`
- **Description**: Tag wrapping text value

### Tag UTF8
- **Category**: Composite Types
- **Input**: `tag("café", unit())`
- **Expected**: `b"<5:caf\xc3\xa9|u,"`
- **Description**: Tag name with UTF-8 characters

### Record Single Field
- **Category**: Composite Types
- **Input**: `record([("a", unit())])`
- **Expected**: `b"{7:<1:a|u,}"`
- **Description**: Record with single field (7 bytes content)

### Record Two Fields
- **Category**: Composite Types
- **Input**: `record([("foo", integer(42)), ("bar", text("baz"))])`
- **Expected**: `b"{26:<3:foo|i:42,<3:bar|t3:baz,}"`
- **Description**: Record with multiple fields (26 bytes content)

### Record Alphabetical Sort
- **Category**: Composite Types
- **Input**: `record({"b": text("2"), "a": text("1")})`
- **Expected**: `b"{20:<1:a|t1:1,<1:b|t1:2,}"`
- **Description**: Dict fields sorted alphabetically (a before b)

### Record Explicit Order
- **Category**: Composite Types
- **Input**: `record_ordered("zebra", text("1"), "alpha", text("2"))`
- **Expected**: `b"{28:<5:zebra|t1:1,<5:alpha|t1:2,}"`
- **Description**: Preserves explicit field order (zebra before alpha)

### List Empty
- **Category**: Composite Types
- **Input**: `list([])`
- **Expected**: `b"[0:]"`
- **Description**: Empty list

### List Single Item
- **Category**: Composite Types
- **Input**: `list([text("hello")])`
- **Expected**: `b"[9:t5:hello,]"`
- **Description**: List with one element

### List Multiple Items
- **Category**: Composite Types
- **Input**: `list([text("foo"), integer(42), unit()])`
- **Expected**: `b"[14:t3:foo,i:42,u,]"`
- **Description**: List with mixed types

## Complex Scenarios

### Nested List in Record
- **Category**: Complex Scenarios
- **Input**: `record([("items", list([text("foo"), text("bar")]))])`
- **Expected**: `b"{28:<5:items|[14:t3:foo,t3:bar,]}"`
- **Description**: Record containing a list

### All Types Record
- **Category**: Complex Scenarios
- **Input**: `record_ordered("unit_field", unit(), "nat_field", natural(42), "int_field", integer(-10), "bool_field", boolean(true), "text_field", text("hello"), "binary_field", binary(b"data"), "tag_field", tag("Some", text("value")), "list_field", list([text("a"), text("b")]))`
- **Expected**: `b"{188:<10:unit_field|u,<9:nat_field|n:42,<9:int_field|i:-10,<10:bool_field|<4:true|u,<10:text_field|t5:hello,<12:binary_field|b4:data,<9:tag_field|<4:Some|t5:value,<10:list_field|[10:t1:a,t1:b,]}"`
- **Description**: Record containing all netencode types

### Unicode Field Names
- **Category**: Complex Scenarios
- **Input**: `record([("café", text("value"))])`
- **Expected**: `b"{18:<5:caf\xc3\xa9|t5:value,}"`
- **Description**: Record with UTF-8 field name

### Unicode Complex
- **Category**: Complex Scenarios
- **Input**: `text("Hello 世界 🌍")`
- **Expected**: `b"t17:Hello \xe4\xb8\x96\xe7\x95\x8c \xf0\x9f\x8c\x8d,"`
- **Description**: Text with mixed ASCII, Chinese (3 bytes each), and emoji (4 bytes)

### Unicode Tag Name
- **Category**: Complex Scenarios
- **Input**: `tag("世界", unit())`
- **Expected**: `b"<6:\xe4\xb8\x96\xe7\x95\x8c|u,"`
- **Description**: Tag with Chinese characters (6 bytes UTF-8)

### Nested Record in List
- **Category**: Complex Scenarios
- **Input**: `list([record([("x", natural(1))]), record([("y", natural(2))])])`
- **Expected**: `b"[26:{9:<1:x|n:1,}{9:<1:y|n:2,}]"`
- **Description**: List containing records

### Field Name Prefixes
- **Category**: Complex Scenarios
- **Input**: `record_ordered("a", text("1"), "aa", text("2"), "aaa", text("3"))`
- **Expected**: `b"{33:<1:a|t1:1,<2:aa|t1:2,<3:aaa|t1:3,}"`
- **Description**: Fields with common prefixes maintain order

### Text With Null Byte
- **Category**: Complex Scenarios
- **Input**: `text("hello\x00world")`
- **Expected**: `b"t11:hello\x00world,"`
- **Description**: Text containing null byte

### Deeply Nested Structures
- **Category**: Complex Scenarios
- **Input**: `record([("nested", list([record([("value", text("deep"))])]))])`
- **Expected**: `b"{37:<6:nested|[22:{17:<5:value|t4:deep,}]}"`
- **Description**: Record containing list containing record (deeply nested)

### Large Binary
- **Category**: Complex Scenarios
- **Input**: `binary(b"x" * 1000)`
- **Expected**: Starts with `b"b1000:"`, total length 1007
- **Description**: Large binary data (1000 bytes + "b1000:" + ",")

## Error Cases

### Natural Negative Error
- **Category**: Error Cases
- **Input**: `natural(-1)`
- **Expected**: Error: "Natural numbers must be non-negative"
- **Description**: Natural numbers cannot be negative

### Natural Overflow Error
- **Category**: Error Cases
- **Input**: `natural(18446744073709551616)` (2^64)
- **Expected**: Error: "Natural number too large"
- **Description**: Natural exceeds 64-bit range

### Integer Overflow Positive
- **Category**: Error Cases
- **Input**: `integer(9223372036854775808)` (2^63)
- **Expected**: Error: "Integer out of 64-bit signed range"
- **Description**: Integer exceeds max signed 64-bit

### Integer Overflow Negative
- **Category**: Error Cases
- **Input**: `integer(-9223372036854775809)` (-(2^63) - 1)
- **Expected**: Error: "Integer out of 64-bit signed range"
- **Description**: Integer below min signed 64-bit

## Implementation Notes

1. All text values must be valid UTF-8
2. Binary data can contain any byte values
3. Record fields from dict are sorted alphabetically by key
4. Record fields from ordered structures preserve insertion order
5. Empty structures are valid (empty text, binary, list)
6. Tag names and record field names are UTF-8 encoded
7. Length prefixes count bytes, not characters