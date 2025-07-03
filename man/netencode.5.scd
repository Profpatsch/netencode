netencode(5) "netencode manual"

# NAME

netencode - Length-prefixed, type-safe data serialization format

# SYNOPSIS

A netencode record containing user information:

```
{49:<6:active|<4:true|u,<3:age|i:30,<4:name|t5:Alice,}
```

A netencode list of text values:

```
[23:t5:hello,t5:world,i:42,]
```

Binary data with length prefix:

```
b13:Hello, World!,
```

# DESCRIPTION

Netencode is a *bencode* and *netstring* inspired data serialization format
designed for Unix pipelines and command-line tools. It provides type-safe,
length-prefixed encoding that is both human-readable for debugging and
machine-efficient for parsing.

The format is designed to be trivial to generate (requiring only *byte_length()*
and *printf()*) and easy to parse (complete parsers can be written in under
100 lines of code).

Key features:
- Length-prefixed variable data for efficient streaming
- Explicit type information for all values
- ASCII prefixes make format partially human-readable
- No escaping required for embedded quotes or special characters
- Binary data support for arbitrary byte sequences

# SCALAR TYPES

All netencode values follow the pattern: *[type][size]:[data][terminator]*

Where *size* is a natural number without leading zeroes, and *terminator* is
always a comma (,).

## Unit Type

The unit type represents a value with no data content.

```
Format: u,
Example: u,
```

## Numbers

All numbers are 64-bit values. Floating-point numbers are not supported.

```
Natural numbers: n:[value],
Range: 0 to 18446744073709551615
Example: n:42,

Signed integers: i:[value],
Range: -9223372036854775808 to 9223372036854775807
Example: i:-42,
Note: JSON numbers are converted to signed integers
```

## Booleans

Booleans are represented as tagged unit values.

```
Boolean true: <4:true|u,
Boolean false: <5:false|u,
```

## Text Strings

UTF-8 encoded text with byte-length prefix. No escaping is required.

```
Format: t[size]:[text],
Example with regular text: t11:hello world,
Example with quotes: t12:He said "hi",
Empty string: t0:,
```

## Binary Data

Arbitrary byte sequences with length prefix. Can contain null bytes and
any binary data.

```
Format: b[size]:[data],
Example: b5:hello,
Empty binary: b0:,
```

# COMPOSITE TYPES

## Tagged Values (Sum Types)

A tag (<) gives a value a name, creating sum types or tagged unions.

```
Format: <[tag_size]:[tag]|[value]
Example: <3:foo|t5:hello,
Empty tag: <0:|i:42,
Nested tag: <4:Some|<4:data|t6:secret,
```

## Records (Maps)

A record ({) is a concatenation of tagged values, closed with }. Records
start with the byte length of their entire encoded content.

```
Format: {[size]:[tag1][tag2]...}
Multiple fields: {21:<3:foo|u,<1:x|t3:baz,}
Single field: {9:<3:foo|u,}
```

*Note*: If tag names repeat, earlier occurrences are ignored. Tag ordering
within records does not matter.

## Lists

A list ([) imposes ordering on a sequence of values, closed with ]. Lists
start with the byte length of their entire encoded content.

```
Format: [[size]:[value1][value2]...]
Example: [13:t3:foo,i:-42,]
Empty list: [0:]
Nested list: [23:[7:t3:foo,]t5:hello,]
```

# PARSING SECURITY CONSIDERATIONS

The length field in netencode is a decimal number that is not length-restricted.
Attackers could provide extremely long length values to overflow parsers or
cause excessive memory allocation.

*Recommended protection*:

1. Define a maximum value length in bytes for your use case
2. Count the digits in that maximum (e.g., 1024 bytes = 4 digits)  
3. Restrict length field parsing to that many decimal digits
4. This implicitly limits the maximum value size

*Example*: If your maximum value is 1024 bytes, limit length fields to 4
digits. This prevents parsing anything longer than 9999 bytes.

*Implementation*: Check length field size before parsing the actual value.
Reject any length field exceeding your digit limit.

# EXAMPLES

## Simple Data Types

```
# Natural number
n:42,

# Negative integer  
i:-123,

# Text with embedded quotes
t26:She said "Hello, world!",

# Binary data (3 bytes)
b3:abc,
```

## Structured Data

```
# User record
{49:<6:active|<4:true|u,<3:age|i:30,<4:name|t5:Alice,}

# List of names
[23:t5:Alice,t3:Bob,t7:Charlie,]

# Optional value (Some)
<4:Some|t5:value,

# Optional value (None)  
<4:None|u,
```

## Nested Structures

```
# Record containing a list
{25:<5:items|[13:t3:foo,t3:bar,],}

# List of records
[42:{12:<4:name|t3:foo,}{12:<4:name|t3:bar,}]
```

# SEE ALSO

*netencode-pretty*(1), *netencode-record-get*(1), *netencode-to-env*(1), 
*env-to-netencode*(1), *netencode-mustache*(1), *json-to-netencode*(1)

Project homepage: https://github.com/Profpatsch/netencode