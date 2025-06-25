# netencode 0.1-unreleased

[bencode][] and [netstring][]-inspired pipe format that should be trivial to generate correctly in every context (only requires a `byte_length()` and a `printf()`), easy to parse (100 lines of code or less), mostly human-decipherable for easy debugging, and support nested record and sum types.


## scalars

Scalars have the format `[type prefix][size]:[value],`.

where size is a natural number without leading zeroes.

### unit

The unit (`u`) has only one value.

* The unit is: `u,`

### numbers

Naturals (`n`) and Integers (`i`) are 64-bit values.


* Natural `1234`: `n:1234,`
* Integer `-42`: `i:-42,`
* Integer `23`: `i:23,`
* Natural `0`: `n:0,`


Floats are not supported, you can implement fixed-size decimals or ratios using integers.

### booleans

Booleans are conventionally represented by the tagged unit values (see below for tags).

* `<5:false|u,`: false
* `<4:true|u,`: true

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
* The tag `` (0 bytes) tagging the integer 0: `<0:|i:0,`

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
* The list with text `foo` followed by integer `-42`: `[13:t3:foo,i:-42,]`
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

## guarantees

TODO: do I want unique representation (bijection like bencode?) This would put more restrictions on the generator, like sorting records in lexicographic order, but would make it possible to compare without decoding


[bencode]: https://en.wikipedia.org/wiki/Bencode
[netstring]: https://en.wikipedia.org/wiki/Netstring
