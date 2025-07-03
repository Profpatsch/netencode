
use netencode::T;

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
    // Fixed: expecting correct alphabetical order
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
