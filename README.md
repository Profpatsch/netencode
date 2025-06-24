# netencode 0.1-unreleased

[bencode][] and [netstring][]-inspired pipe format that should be trivial to generate correctly in every context (only requires a `byte_length()` and a `printf()`), easy to parse (100 lines of code or less), mostly human-decipherable for easy debugging, and support nested record and sum types.


## scalars

Scalars have the format `[type prefix][size]:[value],`.

where size is a natural number without leading zeroes.

### unit

The unit (`u`) has only one value.

* The unit is: `u,`

### numbers

Naturals (`n`) and Integers (`i`), with a maximum size in bits.

Bit sizes are specified in 2^n increments, 1 to 9 (`n1`..`n9`, `i1`..`n9`).

* Natural `1234` that fits in 32 bits (2^5): `n5:1234,`
* Integer `-42` that fits in 8 bits (2^3): `i3:-42,`
* Integer `23` that fits in 64 bits (2^6): `i6:23,`
* Integer `-1` that fits in 512 bits (2^9): `i9:-1,`
* Natural `0` that fits in 1 bit (2^1): `n1:0,`

An implementation can define the biggest numbers it supports, and has to throw an error for anything bigger. It has to support everything smaller, so for example if you support up to i6/n6, you have to support 1–6 as well. An implementation could support up to the current architecture’s wordsize for example.

Floats are not supported, you can implement fixed-size decimals or ratios using integers.

### booleans

A boolean is represented as `n1`.

* `n1:0,`: false
* `n1:1,`: true

TODO: should we add `f,` and `t,`?

### text

Text (`t`) that *must* be encoded as UTF-8, starting with its length in bytes:

* The string `hello world` (11 bytes): `t11:hello world,`
* The string `今日は` (9 bytes): `t9:今日は,`
* The string `:,` (2 bytes): `t2::,,`
* The empty sting `` (0 bytes): `t0:,`

### binary

Arbitrary binary strings (`b`) that can contain any data, starting with its length in bytes.

* The ASCII string `hello world` as binary data (11 bytes): `b11:hello world,`
* The empty binary string (0 bytes): `b0:,`
* The bytestring with `^D` (1 byte): `b1:,`

Since the binary strings are length-prefixd, they can contain `\0` and no escaping is required. Care has to be taken in languages with `\0`-terminated bytestrings.

Use text (`t`) if you have utf-8 encoded data.

## tagged values

### tags

A tag (`<`) gives a value a name. The tag is UTF-8 encoded, starting with its length in bytes and proceeding with the value.

* The tag `foo` (3 bytes) tagging the text `hello` (5 bytes): `<3:foo|t5:hello,`
* The tag `` (0 bytes) tagging the 8-bit integer 0: `<0:|i3:0,`

### records (products/records), also maps

A record (`{`) is a concatenation of tags (`<`). It needs to be closed with `}`.

If tag names repeat the *earlier* ones should be ignored.
Using the last tag corresponds with the way most languages handle converting a list of tuples to Maps, by using a for-loop and Map.insert without checking the contents first. Otherwise you’d have to revert the list first or remember which keys you already inserted.

Ordering of tags in a record does not matter.

Similar to text, records start with the length of their *whole encoded content*, in bytes. This makes it possible to treat their contents as opaque bytestrings.

* There is no empty record. (TODO: make the empty record the unit type, remove `u,`?)
* A record with one empty field, `foo`: `{9:<3:foo|u,}`
* A record with two fields, `foo` and `x`: `{21:<3:foo|u,<1:x|t3:baz,}`
* The same record: `{21:<1:x|t3:baz,<3:foo|u,}`
* The same record (earlier occurences of fields are ignored): `{<1:x|u,28:<1:x|t3:baz,<3:foo|u,}`

### sums (tagged unions)

Simply a tagged value. The tag marker `<` indicates it is a sum if it appears outside of a record.

## lists

A list (`[`) imposes an ordering on a sequence of values. It needs to be closed with `]`. Values in it are simply concatenated.

Similar to records, lists start with the length of their whole encoded content.

* The empty list: `[0:]`
* The list with one element, the string `foo`: `[7:t3:foo,]`
* The list with text `foo` followed by i3 `-42`: `[14:t3:foo,i3:-42,]`
* The list with `Some` and `None` tags: `[33:<4:Some|t3:foo,<4None|u,<4None|u,]`

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

## motivation

TODO

## examples

### Basic data types

```
# Unit (single value)
u,

# Booleans
n1:0,  # false
n1:1,  # true

# Numbers
n3:42,      # natural 42 (fits in 8 bits)
i3:-42,     # integer -42 (fits in 8 bits)
n6:1000000, # natural 1M (fits in 64 bits)
i3:-1,      # integer -1 (fits in 8 bits)

# Text (UTF-8)
t5:hello,
t11:hello world,
t9:今日は,     # Japanese text (9 bytes in UTF-8)
t0:,          # empty string

# Binary data
b4:test,        # 4 bytes
b0:,            # empty binary
```

### Complex data structures

```
# Tagged values (sum types)
<4:Some|t5:hello,    # Some("hello")
<4:None|u,           # None
<5:Error|t14:file not found,  # Error("file not found")

# Records (like JSON objects or maps)
{21:<3:foo|u,<1:x|t3:baz,}     # { foo: unit, x: "baz" }
{28:<4:name|t3:Bob,<3:age|n3:42,}  # { name: "Bob", age: 42 }

# Lists
[0:]                    # empty list
[7:t3:foo,]            # ["foo"]
[14:t3:foo,i3:-42,]    # ["foo", -42]
[35:<4:Some|t3:foo,<4:None|u,<4:None|u,]  # [Some("foo"), None, None]

# Nested structures
{55:<4:user|{29:<4:name|t4:Jane,<3:age|n3:30,}<5:items|[0:]}
# { user: { name: "Jane", age: 30 }, items: [] }
```

### Real-world examples

#### Configuration file
```
{104:<8:database|{37:<4:host|t9:localhost,<4:port|n5:5432,}<7:logging|{34:<5:level|t5:debug,<7:enabled|n1:1,}}
```
Represents:
```json
{
  "database": {
    "host": "localhost",
    "port": 5432
  },
  "logging": {
    "level": "debug",
    "enabled": true
  }
}
```

#### API response
```
<7:success|{91:<4:data|[64:{28:<2:id|n3:1,<4:name|t5:Alice,}{26:<2:id|n3:2,<4:name|t3:Bob,}]<5:count|n3:2,}
```
Represents a successful API response with a list of users.

#### Error handling
```
<5:error|{49:<4:code|n5:404,<7:message|t18:Resource not found,}
```
Represents an error with code and message.

## guarantees

TODO: do I want unique representation (bijection like bencode?) This would put more restrictions on the generator, like sorting records in lexicographic order, but would make it possible to compare without decoding


[bencode]: https://en.wikipedia.org/wiki/Bencode
[netstring]: https://en.wikipedia.org/wiki/Netstring
