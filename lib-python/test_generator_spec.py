"""
Test suite for netencode generator functions based on GENERATOR_TEST_SPEC.md

This module implements all test cases directly without runtime parsing,
ensuring consistency with other language implementations.
"""

import pytest
import netencode as ne


class TestBasicTypes:
    """Test basic netencode types: unit, natural, integer, boolean, text, binary."""
    
    def test_unit_value(self):
        """Validates unit value construction."""
        assert ne.unit() == b"u,"
    
    def test_natural_zero(self):
        """Natural number zero."""
        assert ne.natural(0) == b"n:0,"
    
    def test_natural_number(self):
        """Typical natural number."""
        assert ne.natural(42) == b"n:42,"
    
    def test_natural_max_u64(self):
        """Maximum 64-bit unsigned value."""
        assert ne.natural(18446744073709551615) == b"n:18446744073709551615,"
    
    def test_integer_zero(self):
        """Signed integer zero."""
        assert ne.integer(0) == b"i:0,"
    
    def test_integer_positive(self):
        """Positive signed integer."""
        assert ne.integer(42) == b"i:42,"
    
    def test_integer_negative(self):
        """Negative signed integer."""
        assert ne.integer(-42) == b"i:-42,"
    
    def test_integer_max_i64(self):
        """Maximum 64-bit signed value."""
        assert ne.integer(9223372036854775807) == b"i:9223372036854775807,"
    
    def test_integer_min_i64(self):
        """Minimum 64-bit signed value."""
        assert ne.integer(-9223372036854775808) == b"i:-9223372036854775808,"
    
    def test_boolean_true(self):
        """Boolean true as tagged unit."""
        assert ne.boolean(True) == b"<4:true|u,"
    
    def test_boolean_false(self):
        """Boolean false as tagged unit."""
        assert ne.boolean(False) == b"<5:false|u,"
    
    def test_text_empty(self):
        """Empty text string."""
        assert ne.text("") == b"t0:,"
    
    def test_text_simple(self):
        """Simple ASCII text."""
        assert ne.text("hello") == b"t5:hello,"
    
    def test_text_with_space(self):
        """Text with spaces and punctuation."""
        assert ne.text("Hello, World!") == b"t13:Hello, World!,"
    
    def test_text_utf8_accented(self):
        """UTF-8 text with accented characters."""
        assert ne.text("café") == b"t5:caf\xc3\xa9,"
    
    def test_text_utf8_emoji(self):
        """UTF-8 text with emoji."""
        assert ne.text("🌍") == b"t4:\xf0\x9f\x8c\x8d,"
    
    def test_text_with_quotes(self):
        """Text containing quote characters."""
        assert ne.text('He said "hi"') == b't12:He said "hi",'
    
    def test_text_with_newline(self):
        """Text with newline character."""
        assert ne.text("line1\nline2") == b"t11:line1\nline2,"
    
    def test_text_with_null_byte(self):
        """Text containing null byte."""
        assert ne.text("hello\x00world") == b"t11:hello\x00world,"
    
    def test_binary_empty(self):
        """Empty binary data."""
        assert ne.binary(b"") == b"b0:,"
    
    def test_binary_simple(self):
        """Simple binary data."""
        assert ne.binary(b"hello") == b"b5:hello,"
    
    def test_binary_with_nulls(self):
        """Binary data with null bytes."""
        assert ne.binary(b"\x00\x01\x02") == b"b3:\x00\x01\x02,"
    
    def test_large_binary(self):
        """Large binary data (1000 bytes)."""
        large_data = b"x" * 1000
        result = ne.binary(large_data)
        assert result.startswith(b"b1000:")
        assert len(result) == 1007  # "b1000:" + 1000 bytes + ","


class TestCompositeTypes:
    """Test composite netencode types: tag, record, list."""
    
    def test_tag_simple(self):
        """Simple tag with unit value."""
        assert ne.tag("foo", ne.unit()) == b"<3:foo|u,"
    
    def test_tag_empty_name(self):
        """Tag with empty name."""
        assert ne.tag("", ne.integer(42)) == b"<0:|i:42,"
    
    def test_tag_with_value(self):
        """Tag with non-unit value."""
        assert ne.tag("Some", ne.text("value")) == b"<4:Some|t5:value,"
    
    def test_tag_utf8(self):
        """Tag with UTF-8 name."""
        assert ne.tag("café", ne.unit()) == b"<5:caf\xc3\xa9|u,"
    
    def test_unicode_tag_name(self):
        """Tag with Unicode characters."""
        assert ne.tag("世界", ne.unit()) == b"<6:\xe4\xb8\x96\xe7\x95\x8c|u,"
    
    def test_record_single_field(self):
        """Record with single field."""
        assert ne.record([("a", ne.unit())]) == b"{7:<1:a|u,}"
    
    def test_record_two_fields(self):
        """Record with two fields (alphabetical order)."""
        record = ne.record({"foo": ne.integer(42), "bar": ne.text("baz")})
        assert record == b"{26:<3:bar|t3:baz,<3:foo|i:42,}"
    
    def test_record_alphabetical_sort(self):
        """Record fields sorted alphabetically."""
        record = ne.record({"b": ne.text("2"), "a": ne.text("1")})
        assert record == b"{20:<1:a|t1:1,<1:b|t1:2,}"
    
    def test_record_explicit_order(self):
        """Record with explicit field ordering."""
        record = ne.record_ordered("foo", ne.integer(42), "bar", ne.text("baz"))
        assert record == b"{26:<3:foo|i:42,<3:bar|t3:baz,}"
    
    def test_unicode_field_names(self):
        """Record with Unicode field names."""
        assert ne.record({"café": ne.text("value")}) == b"{18:<5:caf\xc3\xa9|t5:value,}"
    
    def test_list_empty(self):
        """Empty list."""
        assert ne.list([]) == b"[0:]"
    
    def test_list_single_item(self):
        """List with single item."""
        assert ne.list([ne.text("hello")]) == b"[9:t5:hello,]"
    
    def test_list_multiple_items(self):
        """List with multiple items of different types."""
        items = [ne.text("foo"), ne.integer(42), ne.unit()]
        assert ne.list(items) == b"[14:t3:foo,i:42,u,]"


class TestComplexScenarios:
    """Test complex nested structures and edge cases."""
    
    def test_nested_list_in_record(self):
        """Record containing a list."""
        items_list = ne.list([ne.text("foo"), ne.text("bar")])
        record = ne.record({"items": items_list})
        assert record == b"{28:<5:items|[14:t3:foo,t3:bar,]}"
    
    def test_nested_record_in_list(self):
        """List containing records."""
        rec1 = ne.record({"x": ne.natural(1)})
        rec2 = ne.record({"y": ne.natural(2)})
        result_list = ne.list([rec1, rec2])
        assert result_list == b"[26:{9:<1:x|n:1,}{9:<1:y|n:2,}]"
    
    def test_unicode_complex(self):
        """Complex Unicode text with multiple scripts."""
        assert ne.text("Hello 世界 🌍") == b"t17:Hello \xe4\xb8\x96\xe7\x95\x8c \xf0\x9f\x8c\x8d,"
    
    def test_deeply_nested_structures(self):
        """Deeply nested records and lists."""
        inner_record = ne.record({"value": ne.text("deep")})
        middle_list = ne.list([inner_record])
        outer_record = ne.record({"nested": middle_list})
        
        expected = b"{37:<6:nested|[22:{17:<5:value|t4:deep,}]}"
        assert outer_record == expected
    
    def test_record_with_all_types(self):
        """Record containing all basic types."""
        record = ne.record({
            "unit": ne.unit(),
            "natural": ne.natural(42),
            "integer": ne.integer(-42),
            "boolean": ne.boolean(True),
            "text": ne.text("hello"),
            "binary": ne.binary(b"data"),
            "tag": ne.tag("example", ne.unit()),
            "list": ne.list([ne.text("item")]),
            "record": ne.record({"nested": ne.unit()})
        })
        
        # Verify it's valid netencode (starts with record length)
        assert record.startswith(b"{")
        assert record.endswith(b"}")
        # Contains all expected field names in alphabetical order
        assert b"<6:binary|" in record
        assert b"<7:boolean|" in record
        assert b"<7:integer|" in record
        assert b"<4:list|" in record
        assert b"<7:natural|" in record
        assert b"<6:record|" in record
        assert b"<3:tag|" in record
        assert b"<4:text|" in record
        assert b"<4:unit|" in record


class TestErrorCases:
    """Test error conditions and edge cases."""
    
    def test_natural_negative_error(self):
        """Natural numbers cannot be negative."""
        with pytest.raises(ValueError, match="Natural.*negative"):
            ne.natural(-1)
    
    def test_natural_too_large_error(self):
        """Natural numbers have maximum value."""
        with pytest.raises(ValueError, match="Natural.*too large"):
            # This might not actually fail depending on implementation
            # but included for completeness
            ne.natural(2**64)


class TestSimpleRecordFunction:
    """Test the simple_record convenience function."""
    
    def test_simple_record_basic(self):
        """Basic simple_record usage."""
        record = ne.simple_record(a=ne.text("hello"), b=ne.integer(42))
        # Should contain text and integer representations
        assert b"<1:a|t5:hello," in record
        assert b"<1:b|i:42," in record
    
    def test_simple_record_empty(self):
        """Empty simple_record."""
        record = ne.simple_record()
        assert record == b"{0:}"