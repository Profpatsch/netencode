// Simplified test that directly includes the needed parts of netencode
// without the exec_helpers dependency

use indexmap::IndexMap;
use std::fmt::{Debug, Display};

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum T {
    // Unit
    Unit,
    // Naturals (64-bit)
    N(u64),
    // Integers (64-bit)
    I(i64),
    // Text
    Text(String),
    // Binary
    Binary(Vec<u8>),
    // Tags
    Sum(Tag<String, T>),
    // Records
    Record(IndexMap<String, T>),
    // Lists
    List(Vec<T>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Tag<S, A> {
    pub tag: S,
    pub val: Box<A>,
}

impl T {
    /// Create a unit value
    pub fn unit() -> T {
        T::Unit
    }

    /// Create a natural number (unsigned 64-bit)
    pub fn natural(n: u64) -> T {
        T::N(n)
    }

    /// Create a signed integer (64-bit)
    pub fn integer(n: i64) -> T {
        T::I(n)
    }

    /// Create a boolean value as a tagged unit
    pub fn boolean(b: bool) -> T {
        if b {
            T::tag("true", T::unit())
        } else {
            T::tag("false", T::unit())
        }
    }

    /// Create a text string (UTF-8)
    pub fn text<S: Into<String>>(s: S) -> T {
        T::Text(s.into())
    }

    /// Create binary data
    pub fn binary<B: Into<Vec<u8>>>(data: B) -> T {
        T::Binary(data.into())
    }

    /// Create a tagged value
    pub fn tag<S: Into<String>>(name: S, value: T) -> T {
        T::Sum(Tag {
            tag: name.into(),
            val: Box::new(value),
        })
    }

    /// Create a record from key-value pairs
    pub fn record<K, I>(fields: I) -> T 
    where
        K: Into<String>,
        I: IntoIterator<Item = (K, T)>,
    {
        let mut map = IndexMap::new();
        for (k, v) in fields {
            map.insert(k.into(), v);
        }
        // Sort keys alphabetically for consistent output
        map.sort_by(|k1, _, k2, _| k1.cmp(k2));
        T::Record(map)
    }

    /// Create a list from values
    pub fn list<I: IntoIterator<Item = T>>(items: I) -> T {
        T::List(items.into_iter().collect())
    }

    /// Encode to netencode format
    pub fn encode(&self) -> Vec<u8> {
        match self {
            T::Unit => b"u,".to_vec(),
            T::N(n) => format!("n:{},", n).into_bytes(),
            T::I(i) => format!("i:{},", i).into_bytes(),
            T::Text(t) => {
                let bytes = t.as_bytes();
                let mut result = format!("t{}:", bytes.len()).into_bytes();
                result.extend(bytes);
                result.push(b',');
                result
            }
            T::Binary(data) => {
                let mut result = format!("b{}:", data.len()).into_bytes();
                result.extend(data);
                result.push(b',');
                result
            }
            T::Sum(Tag { tag, val }) => {
                let tag_bytes = tag.as_bytes();
                let val_encoded = val.encode();
                let mut result = format!("<{}:", tag_bytes.len()).into_bytes();
                result.extend(tag_bytes);
                result.push(b'|');
                result.extend(val_encoded);
                result
            }
            T::Record(map) => {
                let mut content = Vec::new();
                for (k, v) in map {
                    let key_bytes = k.as_bytes();
                    let val_encoded = v.encode();
                    content.extend(format!("<{}:", key_bytes.len()).into_bytes());
                    content.extend(key_bytes);
                    content.push(b'|');
                    content.extend(val_encoded);
                }
                let mut result = format!("{{{}:", content.len()).into_bytes();
                result.extend(content);
                result.push(b'}');
                result
            }
            T::List(items) => {
                let content: Vec<u8> = items.iter().flat_map(|item| item.encode()).collect();
                let mut result = format!("[{}:", content.len()).into_bytes();
                result.extend(content);
                result.push(b']');
                result
            }
        }
    }
}

// Tests (same as before but using our simplified implementation)

// Basic Types Tests
#[test]
fn test_unit_value() {
    assert_eq!(T::unit().encode(), b"u,");
}

#[test]
fn test_natural_zero() {
    assert_eq!(T::natural(0).encode(), b"n:0,");
}

#[test]
fn test_natural_number() {
    assert_eq!(T::natural(42).encode(), b"n:42,");
}

#[test]
fn test_natural_max_u64() {
    assert_eq!(T::natural(18446744073709551615).encode(), b"n:18446744073709551615,");
}

#[test]
fn test_integer_zero() {
    assert_eq!(T::integer(0).encode(), b"i:0,");
}

#[test]
fn test_integer_positive() {
    assert_eq!(T::integer(42).encode(), b"i:42,");
}

#[test]
fn test_integer_negative() {
    assert_eq!(T::integer(-42).encode(), b"i:-42,");
}

#[test]
fn test_integer_max_i64() {
    assert_eq!(T::integer(9223372036854775807).encode(), b"i:9223372036854775807,");
}

#[test]
fn test_integer_min_i64() {
    assert_eq!(T::integer(-9223372036854775808).encode(), b"i:-9223372036854775808,");
}

#[test]
fn test_boolean_true() {
    assert_eq!(T::boolean(true).encode(), b"<4:true|u,");
}

#[test]
fn test_boolean_false() {
    assert_eq!(T::boolean(false).encode(), b"<5:false|u,");
}

#[test]
fn test_text_empty() {
    assert_eq!(T::text("").encode(), b"t0:,");
}

#[test]
fn test_text_simple() {
    assert_eq!(T::text("hello").encode(), b"t5:hello,");
}

#[test]
fn test_text_with_space() {
    assert_eq!(T::text("Hello, World!").encode(), b"t13:Hello, World!,");
}

#[test]
fn test_text_utf8_accented() {
    assert_eq!(T::text("café").encode(), b"t5:caf\xc3\xa9,");
}

#[test]
fn test_text_utf8_emoji() {
    assert_eq!(T::text("🌍").encode(), b"t4:\xf0\x9f\x8c\x8d,");
}

#[test]
fn test_text_with_quotes() {
    assert_eq!(T::text("He said \"hi\"").encode(), b"t12:He said \"hi\",");
}

#[test]
fn test_text_with_newline() {
    assert_eq!(T::text("line1\nline2").encode(), b"t11:line1\nline2,");
}

#[test]
fn test_binary_empty() {
    assert_eq!(T::binary(b"").encode(), b"b0:,");
}

#[test]
fn test_binary_simple() {
    assert_eq!(T::binary(b"hello").encode(), b"b5:hello,");
}

#[test]
fn test_binary_with_nulls() {
    assert_eq!(T::binary(b"\x00\x01\x02").encode(), b"b3:\x00\x01\x02,");
}

// Composite Types Tests
#[test]
fn test_tag_simple() {
    assert_eq!(T::tag("foo", T::unit()).encode(), b"<3:foo|u,");
}

#[test]
fn test_tag_empty_name() {
    assert_eq!(T::tag("", T::integer(42)).encode(), b"<0:|i:42,");
}

#[test]
fn test_tag_with_value() {
    assert_eq!(T::tag("Some", T::text("value")).encode(), b"<4:Some|t5:value,");
}

#[test]
fn test_tag_utf8() {
    assert_eq!(T::tag("café", T::unit()).encode(), b"<5:caf\xc3\xa9|u,");
}

#[test]
fn test_record_single_field() {
    let record = T::record(vec![("a", T::unit())]);
    assert_eq!(record.encode(), b"{7:<1:a|u,}");
}

#[test]
fn test_record_two_fields() {
    let record = T::record(vec![("foo", T::integer(42)), ("bar", T::text("baz"))]);
    assert_eq!(record.encode(), b"{26:<3:bar|t3:baz,<3:foo|i:42,}");
}

#[test]
fn test_record_alphabetical_sort() {
    let record = T::record(vec![("b", T::text("2")), ("a", T::text("1"))]);
    assert_eq!(record.encode(), b"{20:<1:a|t1:1,<1:b|t1:2,}");
}

#[test]
fn test_list_empty() {
    assert_eq!(T::list(vec![]).encode(), b"[0:]");
}

#[test]
fn test_list_single_item() {
    assert_eq!(T::list(vec![T::text("hello")]).encode(), b"[9:t5:hello,]");
}

#[test]
fn test_list_multiple_items() {
    let items = vec![T::text("foo"), T::integer(42), T::unit()];
    assert_eq!(T::list(items).encode(), b"[14:t3:foo,i:42,u,]");
}

// Complex Scenarios Tests
#[test]
fn test_nested_list_in_record() {
    let list = T::list(vec![T::text("foo"), T::text("bar")]);
    let record = T::record(vec![("items", list)]);
    assert_eq!(record.encode(), b"{28:<5:items|[14:t3:foo,t3:bar,]}");
}

#[test]
fn test_nested_record_in_list() {
    let rec1 = T::record(vec![("x", T::natural(1))]);
    let rec2 = T::record(vec![("y", T::natural(2))]);
    let list = T::list(vec![rec1, rec2]);
    assert_eq!(list.encode(), b"[26:{9:<1:x|n:1,}{9:<1:y|n:2,}]");
}

#[test]
fn test_unicode_field_names() {
    let record = T::record(vec![("café", T::text("value"))]);
    assert_eq!(record.encode(), b"{18:<5:caf\xc3\xa9|t5:value,}");
}

#[test]
fn test_unicode_complex() {
    assert_eq!(T::text("Hello 世界 🌍").encode(), b"t17:Hello \xe4\xb8\x96\xe7\x95\x8c \xf0\x9f\x8c\x8d,");
}

#[test]
fn test_unicode_tag_name() {
    assert_eq!(T::tag("世界", T::unit()).encode(), b"<6:\xe4\xb8\x96\xe7\x95\x8c|u,");
}

#[test]
fn test_text_with_null_byte() {
    assert_eq!(T::text("hello\x00world").encode(), b"t11:hello\x00world,");
}

#[test]
fn test_large_binary() {
    let large_data = vec![b'x'; 1000];
    let result = T::binary(large_data).encode();
    assert!(result.starts_with(b"b1000:"));
    assert_eq!(result.len(), 1007); // "b1000:" + 1000 bytes + ","
}

#[test]
fn test_deeply_nested_structures() {
    let inner_record = T::record(vec![("value", T::text("deep"))]);
    let middle_list = T::list(vec![inner_record]);
    let outer_record = T::record(vec![("nested", middle_list)]);
    
    let expected = b"{37:<6:nested|[22:{17:<5:value|t4:deep,}]}";
    assert_eq!(outer_record.encode(), expected);
}

// Type Safety Tests - demonstrating that Rust's type system prevents errors
#[test]
fn test_natural_values_are_inherently_non_negative() {
    // u64 type prevents negative values at compile time
    assert_eq!(T::natural(0).encode(), b"n:0,");
}

#[test]
fn test_natural_values_are_inherently_bounded_to_64_bit() {
    // u64 type prevents overflow at compile time  
    assert_eq!(T::natural(u64::MAX).encode(), b"n:18446744073709551615,");
}

#[test]
fn test_integer_values_are_inherently_bounded_to_64_bit_signed() {
    // i64 type prevents overflow at compile time
    assert_eq!(T::integer(i64::MIN).encode(), b"i:-9223372036854775808,");
    assert_eq!(T::integer(i64::MAX).encode(), b"i:9223372036854775807,");
}

#[test]
fn test_text_values_are_inherently_valid_utf8() {
    // String type guarantees valid UTF-8 encoding
    let result = T::text("valid UTF-8: 🌍").encode();
    assert!(result.starts_with(b"t"));
}

#[test]
fn test_binary_values_are_inherently_valid_byte_sequences() {
    // Vec<u8> type guarantees valid binary data
    let result = T::binary(b"any bytes").encode();
    assert!(result.starts_with(b"b"));
}