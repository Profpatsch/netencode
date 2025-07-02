# netencode 0.1-unreleased

[bencode][] and [netstring][]-inspired pipe format that is trivial to generate correctly in every context (only requires a `byte_length()` and a `printf()`), easy to parse (100 lines of code or less), mostly human-decipherable for easy debugging, and supports nested record and sum types.

## Quick Start: Pipeline-Friendly Data Processing

Netencode excels at **human-readable data pipelines**. Unlike binary formats, you can debug your data streams by simply looking at them.

### The Debug-by-Looking Advantage

```bash
# With netencode, you can see what's flowing through your pipes:
some-data-source | tee /dev/stderr | next-processing-step
# Output shows readable data:
# {25:<4:name|t5:Alice,<3:age|n:30,}
# {23:<4:name|t3:Bob,<3:age|n:25,}

# Compare with binary formats where you see:
# ���A9�f��binary-gibberish��

# With text formats, you need extra work to handle structure:
echo "Alice,30" | cut -d',' -f1  # Fragile CSV parsing
echo "Alice 30" | awk '{print $1}'  # Whitespace assumptions

# Netencode handles structure naturally:
echo '{25:<4:name|t5:Alice,<3:age|n:30,}' | netencode-record-get name
# Outputs: t5:Alice,

# Convert netencode values to plain text for shell use:
echo '{25:<4:name|t5:Alice,<3:age|n:30,}' | netencode-record-get name | netencode-plain
# Outputs: Alice
```

## Netencode vs JSON

While JSON and netencode both represent structured data, they solve different problems:

**JSON Example:**
```json
{"name": "Alice", "age": 30, "active": true}
```

**Netencode Example:**
```
{33:<4:name|t5:Alice,<3:age|n:30,<6:active|<4:true|u,}
```

### Length-Prefixed vs Delimiter-Based Parsing

JSON requires scanning the entire document to find object boundaries. With `{"key": "some value with \"}\" inside"}`, you must parse the whole thing to know where it ends. Netencode's `{33:...}` tells you exactly 33 bytes of content follow, enabling efficient streaming and skipping over uninteresting values.

### Explicit Type Information

JSON numbers are ambiguous—`42` could be an integer or float. Netencode distinguishes `n:42` (natural number) from `i:-42` (signed integer). Booleans in JSON are primitives; in netencode they're tagged units: `<4:true|u,` making the type system more uniform.

### Binary Data Support

JSON can only represent UTF-8 clean text. Unix filenames can contain arbitrary bytes that aren't valid UTF-8. In JSON, you'd need base64 encoding or escaping. Netencode handles this directly with binary strings:

```bash
# Create a filename with non-UTF-8 bytes (common in legacy systems)
FILENAME=$(printf 'file\xff\x00name')
LENGTH=$(printf "%s" "$FILENAME" | wc -c)

# JSON: Must escape or encode - cannot represent raw bytes
# Escape sequences:
echo '{"filename": "file\\u00ff\\u0000name"}'
# Base64 encoding:
echo '{"filename": "ZmlsZfAAbmFtZQ=="}'

# Netencode: Embed raw bytes directly with length prefix
printf "b${LENGTH}:${FILENAME},"
# Creates: b9:file<0xff><0x00>name, (raw bytes, no escaping needed)
```

### No Escaping Required

JSON strings need escaping: `"He said \"Hello\""` becomes complex with nested quotes and backslashes. Netencode's length prefixes eliminate this: `t12:He said "Hello",` contains the quotes literally.

### Construct with only a byte_length() and a printf()

```bash
# Construct a netencode record anywhere if you have these two primitives:
printf "b%d:%s," $(echo "Hello, World!" | wc -c) "Hello, World!"
# Output: b13:Hello, World!,
```

### When to Use JSON

- **Web APIs**: Universal browser support and widespread tooling
- **Configuration files**: When human editing is primary concern
- **Interoperability**: Communicating with systems that expect JSON

### When to Use Netencode

- **Unix pipelines**: Processing structured data with shell tools
- **Binary data**: Embedding filenames, images, or arbitrary byte sequences
- **Type-safe systems**: When explicit types prevent bugs
- **Performance-critical parsing**: Minimal parser complexity
- **Debugging data flows**: When you need to see what's flowing through pipes

### JSON to Netencode Pipeline

```bash
# Convert JSON API response to netencode for pipeline processing
curl -s api/users.json | json-to-netencode
# Output: {25:<4:name|t5:Alice,<3:age|n:30,}

# Filter active users (you can see exactly what's being filtered)
curl -s api/users.json | json-to-netencode | netencode-filter active=true
# Shows: {33:<4:name|t5:Alice,<3:age|n:30,<6:active|<4:true|u,}

# Extract specific field
curl -s api/users.json | json-to-netencode | netencode-filter active=true | netencode-record-get name
# Shows: t5:Alice,
```

### Environment Integration

```bash
# Convert your environment to structured data
env-to-netencode
# Output: {200:<4:HOME|t10:/home/user,<4:PATH|t50:/usr/bin:/bin,<4:USER|t4:user,...}

# Use structured data as environment for commands
echo '{25:<4:name|t5:Alice,<3:age|n:30,}' | netencode-to-env sh -c 'echo "Hello $name, you are $age years old"'
# Output: Hello Alice, you are 30 years old
```

### Log Processing Pipeline

```bash
# Process application logs (assuming they're in JSON format)
tail -f app.log | json-to-netencode | netencode-filter level=error | netencode-record-get message
# Shows error messages as they happen, and you can see the full structured data mid-pipeline
```

## Real-World Examples

### API Data Processing

```bash
# Fetch user data from GitHub API and extract active repositories
curl -s https://api.github.com/users/octocat/repos | json-to-netencode | netencode-filter archived=false | netencode-record-get name
# Output: t9:Hello-World,t13:octocat.github.io,

# Compare with raw JSON (binary soup when debugging):
curl -s https://api.github.com/users/octocat/repos | jq '.[] | select(.archived == false) | .name'
```

### Log Processing Pipeline

```bash
# Monitor application logs and extract errors in real-time:
tail -f /var/log/app.log |
  json-to-netencode |
  netencode-filter level=error |
  tee error.log |                    # Save errors while processing
  netencode-record-get message                 # Show just the error messages

# You can see the full structured data at any stage by adding `tee /dev/stderr`
```

### Data Transformation

```bash
# Transform API response data for another service:
curl -s api/source.json |
  json-to-netencode |
  netencode-filter status=active |    # Filter active records
  while read -r record; do
    # Extract fields and transform
    NAME=$(echo "$record" | netencode-record-get name | netencode-plain)
    EMAIL=$(echo "$record" | netencode-record-get email | netencode-plain)
    # Output in netencode format for next stage
    echo "{$(( ${#NAME} + ${#EMAIL} + 25 )):<4:user|t${#NAME}:${NAME},<5:email|t${#EMAIL}:${EMAIL},}"
  done
```

### Format Comparison: JSON vs Netencode

**JSON:**
```json
{"name": "Alice", "age": 30, "active": true}
```

**Netencode:**
```
{33:<4:name|t5:Alice,<3:age|n:30,<6:active|<4:true|u,}
```

**Advantages of netencode version:**
- Length-prefixed: `{33:` tells you exactly 33 bytes of content
- Type-explicit: `t5:` = 5 bytes of text, `n:30` = natural number
- No escaping: Text like `"quotes"` or `\backslashes` require no special handling
- Streaming-friendly: You can parse incrementally without scanning for delimiters
- Visually debuggable: Each component is clearly separated and typed

## Using with Nix Flakes

### Quick Try (No Installation Required)

```bash
# Pretty-print netencode data
echo 't5:hello,' | nix run github:Profpatsch/netencode

# The default app is netencode-pretty, but you can access others:
echo 't5:hello,' | nix run github:Profpatsch/netencode#netencode-pretty
```

### Development Environment

```bash
# Enter development shell with all tools available
nix develop

# Now all CLI tools are in your PATH:
echo 't5:hello,' | netencode-pretty
echo 't5:hello,' | netencode-plain
echo '{25:<4:name|t5:Alice,<3:age|n:30,}' | netencode-record-get name

# Process data pipelines with full toolset
curl -s api/users.json | json-to-netencode | netencode-filter active=true | netencode-record-get name | netencode-plain
```

### Available Flake Outputs

- `packages.default` / `packages.netencode`: Complete netencode package with all tools
- `packages.netencode-tests`: Test suite (configurable with testFiles and pytestArgs)
- `apps.default` / `apps.netencode-pretty`: Pretty-printer app for quick inspection
- `devShells.default`: Development environment with all tools in PATH

### Local Development

```bash
# Clone and enter development environment
git clone https://github.com/Profpatsch/netencode
cd netencode
nix develop

# All tools are now available for testing and development
```

## Format Specification

## Scalar Types

All netencode values follow a consistent pattern: `[type][size]:[data][terminator]`

Where **size** is a natural number without leading zeroes.

### Unit Type
The unit type represents a value with no data.

| Format | Description |
|--------|-------------|
| `u,` | The only unit value |

### Numbers
All numbers are 64-bit values. Floats are not supported—use integers for fixed-point decimals.

| Type | Format | Examples |
|------|--------|----------|
| **Natural** | `n:[value],` | `n:0,` `n:1234,` `n:18446744073709551615,` |
| **Integer** | `i:[value],` | `i:0,` `i:-42,` `i:23,` `i:-9223372036854775808,` |

### Booleans
Represented as tagged unit values (see Tagged Values section below).

| Value | Format | Description |
|-------|--------|-------------|
| **true** | `<4:true\|u,` | Boolean true as tagged unit |
| **false** | `<5:false\|u,` | Boolean false as tagged unit |

### Text Strings
UTF-8 encoded text with byte-length prefix.


| Format | Examples |
|--------|----------|
| `t[size]:[text],` | `t11:hello world,` |
| | `t9:今日は,` (9 UTF-8 bytes) |
| | `t2::,,` (contains `:,` literally) |
| | `t0:,` (empty string) |

### Binary Data

Arbitrary byte sequences with length prefix. No escaping required.

| Format | Examples |
|--------|----------|
| `b[size]:[data],` | `b11:hello world,` |
| | `b0:,` (empty binary string) |
| | `b1:,` (contains `^D` byte) |

Since the binary strings are length-prefixd, they can contain `\0` and no escaping is required. Care has to be taken in languages with `\0`-terminated bytestrings.

Use text (`t`) if you have utf-8 encoded data.

## tagged values

### tags

A tag (`<`) gives a value a name. The tag is UTF-8 encoded, starting with its length in bytes and proceeding with the value.

* The tag `foo` (3 bytes) tagging the text `hello` (5 bytes): `<3:foo|t5:hello,`
* The tag `` (0 bytes) tagging the integer 0: `<0:|i:0,`

### records (products/records), also maps

A record (`{`) is a concatenation of tags (`<`). It needs to be closed with `}`.

If tag names repeat the *earlier* ones should be ignored.
Using the last tag corresponds with the way most languages handle converting a list of tuples to Maps, by using a for-loop and Map.insert without checking the contents first. Otherwise you’d have to revert the list first or remember which keys you already inserted.

Ordering of tags in a record does not matter.

Similar to text, records start with the length of their *whole encoded content*, in bytes. This makes it possible to treat their contents as opaque bytestrings.

* There is no empty record.
* A record with one empty field, `foo`: `{9:<3:foo|u,}`
* A record with two fields, `foo` and `x`: `{21:<3:foo|u,<1:x|t3:baz,}`
* The same record: `{21:<1:x|t3:baz,<3:foo|u,}`
* The same record (earlier occurences of fields are ignored): `{<1:x|u,28:<1:x|t3:baz,<3:foo|u,}`

### sums (tagged unions)

Simply a tagged value. The tag marker `<` indicates it is a sum if it appears outside of a record.

## lists

A list (`[`) imposes an ordering on a sequence of values. It needs to be closed with `]`. Values in it are simply concatenated.

Similar to records, lists start with the length of their whole encoded content.

* The empty list: `[0:]`.
* The list with one element, the string `foo`: `[7:t3:foo,]`
* The list with text `foo` followed by integer `-42`: `[13:t3:foo,i:-42,]`
* The list with `Some` and `None` tags: `[33:<4:Some|t3:foo,<4None|u,<4None|u,]`

**Note on Empty Lists**: The empty list `[0:]` is essentially equivalent to the unit type in terms of information content, but we maintain it as a distinct construct because it's valuable for filtering operations and interoperability with systems that distinguish between "no value" and "empty collection".

## parser security considerations

The length field is a decimal number that is not length-restricted,
meaning an attacker could give an infinitely long length (or extremely long)
thus overflowing your parser if you are not careful.

You should thus put a practical length limit to the length of length fields,
which implicitely enforces a length limit on how long the value itself can be.

Start by defining a max value length in bytes.
Then count the number of decimals in that number.

So if your max length is 1024 bytes, your length field can be a maximum `count_digits(1024) == 4` bytes long.

Thus, if you restrict your parser to a length field of 4 bytes,
it should also never parse anything longer than 1024 bytes for the value
(plus 1 byte for the type tag, 4 bytes for the length, and 2 bytes for the separator & ending character).

## Generator Libraries

The netencode ecosystem provides generator libraries in multiple languages with a unified API. All languages use identical function names for consistent cross-language development.

### Unified API Reference

All generator libraries provide these functions:

| Function | Purpose | Example Usage |
|----------|---------|---------------|
| `unit()` | Create unit value | `unit()` → `u,` |
| `natural(n)` | Create natural number (unsigned 64-bit) | `natural(42)` → `n:42,` |
| `integer(n)` | Create signed integer (64-bit) | `integer(-10)` → `i:-10,` |
| `boolean(b)` | Create boolean as tagged unit | `boolean(true)` → `<4:true\|u,` |
| `text(s)` | Create UTF-8 text string | `text("hello")` → `t5:hello,` |
| `binary(data)` | Create binary data | `binary(b"\xff\x00")` → `b2:\xff\x00,` |
| `tag(name, value)` | Create tagged value | `tag("status", text("ok"))` → `<6:status\|t2:ok,` |
| `record(fields)` | Create record from fields | `record([("name", text("Alice"))])` → `{...}` |
| `list(items)` | Create list from items | `list([text("a"), text("b")])` → `[...]` |

### Language-Specific Usage

#### Python (`lib-python/netencode.py`)
```python
import netencode as ne

# Basic types
user_record = ne.record([
    ("name", ne.text("Alice")),
    ("age", ne.natural(30)),
    ("active", ne.boolean(True))
])

# Convenience function for sorted fields
user_record = ne.simple_record(
    name=ne.text("Alice"),
    age=ne.natural(30),
    active=ne.boolean(True)
)
```

#### Rust (`lib-rust/netencode.rs`)
```rust
use netencode::T;

// Type-safe constructor functions
let user_record = T::record([
    ("name", T::text("Alice")),
    ("age", T::natural(30)),
    ("active", T::boolean(true))
]);

// Ergonomic with Into<> traits
let user_record = T::record([
    ("name", "Alice"),  // Auto-converts to T::text
    ("age", 30u64),     // Auto-converts to T::natural
    ("active", true)    // Auto-converts to T::boolean
]);
```

#### Haskell (`lib-haskell/Netencode.hs`)
```haskell
import Netencode

-- Functional style with type safety
userRecord = record [
    ("name", text "Alice"),
    ("age", natural 30),
    ("active", boolean True)
]
```

#### Nix (`lib-nix/gen.nix`)
```nix
with (import ./lib-nix/gen.nix);

# Functional Nix expressions
user-record = record {
  name = text "Alice";
  age = natural 30;
  active = boolean true;
};

# Or using dwim for automatic type detection
user-record-auto = dwim {
  name = "Alice";    # Automatically becomes text
  age = 30;          # Automatically becomes natural
  active = true;     # Automatically becomes boolean
};
```

### Cross-Language Compatibility

All generator libraries produce identical output for the same logical data:

```python
# Python
ne.simple_record(name=ne.text("Alice"), age=ne.natural(30))
```

```rust
// Rust  
T::record([("name", "Alice"), ("age", 30u64)])
```

```haskell
-- Haskell
record [("name", text "Alice"), ("age", natural 30)]
```

```nix
# Nix
record { name = text "Alice"; age = natural 30; }
```

All produce: `{25:<3:age|n:30,<4:name|t5:Alice,}`

### Directory Structure

The generator libraries are organized by language implementation:

```
lib-python/netencode.py     # Python generator library
lib-rust/netencode.rs       # Rust generator library  
lib-haskell/Netencode.hs    # Haskell generator library
lib-nix/gen.nix             # Nix generator functions
```

## CLI Tools

The netencode ecosystem provides several command-line tools for working with data pipelines:

### Core Tools
- **`json-to-netencode`**: Convert JSON to netencode format for pipeline processing
- **`netencode-record-get <field>`**: Extract a field from a netencode record
- **`netencode-plain`**: Convert scalar netencode values to plain text (eliminates the need for sed patterns)
- **`netencode-filter <field>=<value>`**: Filter netencode records by field values
- **`netencode-pretty`**: Pretty-print netencode for human reading

### Environment Integration
- **`env-to-netencode`**: Convert environment variables to a netencode record
- **`netencode-to-env <command>`**: Execute command with record fields as environment variables

### Template Processing
- **`netencode-mustache`**: Mustache template rendering with netencode data

### Example Usage Pipeline
```bash
# Complete data processing pipeline
curl -s api/users.json |
  json-to-netencode |              # JSON → netencode
  netencode-filter active=true |   # Filter records
  netencode-record-get name |                # Extract field
  netencode-plain                  # Convert to plain text
# Output: Alice
```

## motivation

Netencode bridges the gap between human-readable and machine-efficient data formats, optimized for the Unix pipeline philosophy.

### Why Netencode?

**Human-Readable Debugging**: Unlike MessagePack, Protocol Buffers, or other binary formats, you can visually inspect your data streams. When a pipeline breaks, you can see exactly what data is flowing through each stage.

**Trivial Generation**: Only requires `byte_length()` and `printf()` - you can generate valid netencode in any language with these basic primitives.

**Length-Prefixed Streaming**: Unlike JSON (which requires parsing the entire document to find boundaries), netencode uses length prefixes that allow efficient streaming and incremental parsing. You know exactly how many bytes to read.

**Type Safety**: Every value has an explicit type (`n:` for naturals, `t5:` for 5-byte text, etc.), eliminating the ambiguity found in formats like JSON where numbers might be integers or floats.

**Shell-Friendly**: Designed for command-line tools and shell scripting. No escaping nightmares, no quote handling complexity, no parsing edge cases.

**Minimal Parsing**: Complete parsers can be written in under 100 lines of code, making it easy to add netencode support to any tool.

### Ideal Use Cases

- **Data Pipelines**: Where you need to see what's happening at each stage
- **Shell Scripting**: Processing structured data with standard Unix tools
- **Log Processing**: Human-readable structured logs that are also machine-parseable
- **Inter-Process Communication**: When debugging is important

## guarantees

TODO: do I want unique representation (bijection like bencode?) This would put more restrictions on the generator, like sorting records in lexicographic order, but would make it possible to compare without decoding

[bencode]: https://en.wikipedia.org/wiki/Bencode
[netstring]: https://en.wikipedia.org/wiki/Netstring
