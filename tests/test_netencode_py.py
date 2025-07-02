"""
Test the netencode_py module itself.
"""
import pytest
import netencode as ne
from collections import OrderedDict


class TestBasicTypes:
    """Test basic netencode type constructors."""
    
    def test_unit(self):
        """Test unit value construction."""
        assert ne.unit() == b"u,"
    
    def test_natural_numbers(self):
        """Test natural number construction."""
        assert ne.natural(0) == b"n:0,"
        assert ne.natural(42) == b"n:42,"
        assert ne.natural(2**64 - 1) == b"n:18446744073709551615,"
        
        # Test error cases
        with pytest.raises(ValueError, match="Natural numbers must be non-negative"):
            ne.natural(-1)
        
        with pytest.raises(ValueError, match="Natural number too large"):
            ne.natural(2**64)
    
    def test_integers(self):
        """Test signed integer construction."""
        assert ne.integer(0) == b"i:0,"
        assert ne.integer(42) == b"i:42,"
        assert ne.integer(-42) == b"i:-42,"
        assert ne.integer(2**63 - 1) == b"i:9223372036854775807,"
        assert ne.integer(-(2**63)) == b"i:-9223372036854775808,"
        
        # Test error cases
        with pytest.raises(ValueError, match="Integer out of 64-bit signed range"):
            ne.integer(2**63)
        
        with pytest.raises(ValueError, match="Integer out of 64-bit signed range"):
            ne.integer(-(2**63) - 1)
    
    def test_booleans(self):
        """Test boolean construction."""
        assert ne.boolean(True) == b"<4:true|u,"
        assert ne.boolean(False) == b"<5:false|u,"
    
    def test_text(self):
        """Test text string construction."""
        assert ne.text("") == b"t0:,"
        assert ne.text("hello") == b"t5:hello,"
        assert ne.text("Hello, World!") == b"t13:Hello, World!,"
        
        # Test UTF-8 encoding
        assert ne.text("café") == b"t5:caf\xc3\xa9,"  # é is 2 bytes in UTF-8
        assert ne.text("🌍") == b"t4:\xf0\x9f\x8c\x8d,"      # emoji is 4 bytes in UTF-8
        
        # Test special characters
        assert ne.text("He said \"hi\"") == b"t12:He said \"hi\","
        assert ne.text("line1\nline2") == b"t11:line1\nline2,"
    
    def test_binary(self):
        """Test binary data construction."""
        assert ne.binary(b"") == b"b0:,"
        assert ne.binary(b"hello") == b"b5:hello,"
        assert ne.binary(b"\x00\x01\x02") == b"b3:\x00\x01\x02,"


class TestCompositeTypes:
    """Test composite type constructors."""
    
    def test_tag(self):
        """Test tagged value construction."""
        assert ne.tag("foo", b"u,") == b"<3:foo|u,"
        assert ne.tag("", b"i:42,") == b"<0:|i:42,"
        assert ne.tag("Some", b"t5:value,") == b"<4:Some|t5:value,"
        
        # Test UTF-8 tag names
        assert ne.tag("café", b"u,") == b"<5:caf\xc3\xa9|u,"
    
    def test_record_from_dict(self):
        """Test record construction from regular dict (sorted)."""
        # Regular dicts are sorted by key
        record = ne.record({"b": b"t1:2,", "a": b"t1:1,"})
        
        # Should be sorted alphabetically
        assert b"<1:a|t1:1," in record
        assert b"<1:b|t1:2," in record
        
        # Check ordering
        a_pos = record.find(b"<1:a|")
        b_pos = record.find(b"<1:b|")
        assert a_pos < b_pos
    
    def test_record_from_ordered_dict(self):
        """Test record construction from OrderedDict."""
        fields = OrderedDict()
        fields["zebra"] = b"t1:1,"
        fields["alpha"] = b"t1:2,"
        
        record = ne.record(fields)
        
        # Should preserve order from OrderedDict
        zebra_pos = record.find(b"<5:zebra|")
        alpha_pos = record.find(b"<5:alpha|")
        assert zebra_pos < alpha_pos
    
    def test_record_from_list(self):
        """Test record construction from list of tuples."""
        fields = [("third", b"t1:c,"), ("first", b"t1:a,"), ("second", b"t1:b,")]
        record = ne.record(fields)
        
        # Should preserve order from list
        third_pos = record.find(b"<5:third|")
        first_pos = record.find(b"<5:first|")
        second_pos = record.find(b"<6:second|")
        
        assert third_pos < first_pos < second_pos
    
    def test_record_length_calculation(self):
        """Test that record length is calculated correctly."""
        # Simple single field record
        record = ne.record([("a", b"u,")])
        # Content: <1:a|u, = 7 bytes
        assert record.startswith(b"{7:")
        
        # Multi-field record
        record = ne.record([("foo", b"i:42,"), ("bar", b"t3:baz,")])
        # Content: <3:foo|i:42,<3:bar|t3:baz, = 26 bytes
        assert record.startswith(b"{26:")
    
    def test_list_values(self):
        """Test list construction."""
        # Empty list
        empty_list = ne.list_values([])
        assert empty_list == b"[0:]"
        
        # Single item list
        single_list = ne.list_values([b"t5:hello,"])
        assert single_list == b"[9:t5:hello,]"
        
        # Multi-item list
        multi_list = ne.list_values([b"t3:foo,", b"i:42,", b"u,"])
        # Content: t3:foo,i:42,u, = 14 bytes
        assert multi_list == b"[14:t3:foo,i:42,u,]"
    
    def test_record_ordered(self):
        """Test record_ordered convenience function."""
        record = ne.record_ordered(
            "second", b"t1:2,",
            "first", b"t1:1,"
        )
        
        # Should maintain explicit order
        second_pos = record.find(b"<6:second|")
        first_pos = record.find(b"<5:first|")
        assert second_pos < first_pos
        
        # Test error case
        with pytest.raises(ValueError, match="even number of arguments"):
            ne.record_ordered("key1", "value1", "key2")
    
    def test_simple_record(self):
        """Test simple_record convenience function."""
        record = ne.simple_record(zebra=b"t1:1,", alpha=b"t1:2,")
        
        # Should be alphabetically sorted
        alpha_pos = record.find(b"<5:alpha|")
        zebra_pos = record.find(b"<5:zebra|")
        assert alpha_pos < zebra_pos


class TestComplexScenarios:
    """Test complex netencode construction scenarios."""
    
    def test_nested_structures(self):
        """Test nested records and lists."""
        # Create nested record containing a list
        inner_list = ne.list_values([ne.text("foo"), ne.text("bar")])
        record = ne.record([("items", inner_list)])
        
        assert b"<5:items|[" in record
        assert b"t3:foo," in record
        assert b"t3:bar," in record
    
    def test_all_types_in_record(self):
        """Test record containing all netencode types."""
        record = ne.record_ordered(
            "unit_field", ne.unit(),
            "nat_field", ne.natural(42),
            "int_field", ne.integer(-10),
            "bool_field", ne.boolean(True),
            "text_field", ne.text("hello"),
            "binary_field", ne.binary(b"data"),
            "tag_field", ne.tag("Some", ne.text("value")),
            "list_field", ne.list_values([ne.text("a"), ne.text("b")])
        )
        
        # Verify all field types are present
        assert b"<10:unit_field|u," in record
        assert b"<9:nat_field|n:42," in record
        assert b"<9:int_field|i:-10," in record
        assert b"<10:bool_field|<4:true|u," in record
        assert b"<10:text_field|t5:hello," in record
        assert b"<12:binary_field|b4:data," in record
        assert b"<9:tag_field|<4:Some|t5:value," in record
        assert b"<10:list_field|[" in record
    
    def test_field_ordering_edge_cases(self):
        """Test field ordering with edge cases."""
        # Fields with same prefix but different lengths
        record = ne.record_ordered(
            "a", ne.text("1"),
            "aa", ne.text("2"),
            "aaa", ne.text("3")
        )
        
        # Verify ordering is maintained
        a_pos = record.find(b"<1:a|t1:1,")
        aa_pos = record.find(b"<2:aa|t1:2,")
        aaa_pos = record.find(b"<3:aaa|t1:3,")
        
        assert a_pos < aa_pos < aaa_pos
    
    def test_unicode_edge_cases(self):
        """Test Unicode handling in various contexts."""
        # Unicode in field names
        record = ne.record([("café", ne.text("value"))])
        assert b"<5:caf\xc3\xa9|t5:value," in record
        
        # Unicode in text values
        text_val = ne.text("Hello 世界 🌍")
        assert b"t17:Hello \xe4\xb8\x96\xe7\x95\x8c \xf0\x9f\x8c\x8d," == text_val  # 17 bytes in UTF-8
        
        # Unicode in tags
        tagged = ne.tag("世界", ne.unit())
        assert b"<6:\xe4\xb8\x96\xe7\x95\x8c|u," == tagged  # 6 bytes for "世界" in UTF-8
    
    def test_large_structures(self):
        """Test handling of larger structures."""
        # Create a record with many fields
        fields = []
        for i in range(100):
            fields.append((f"field_{i:03d}", ne.natural(i)))
        
        record = ne.record(fields)
        
        # Should handle large structures correctly
        assert record.startswith(b"{")
        assert record.endswith(b"}")
        
        # Verify some fields are present
        assert b"<9:field_000|n:0," in record
        assert b"<9:field_099|n:99," in record


class TestErrorHandling:
    """Test error handling and validation."""
    
    def test_record_invalid_input_types(self):
        """Test record construction with invalid input types."""
        with pytest.raises(TypeError, match="fields must be dict, OrderedDict, or list"):
            ne.record("invalid")
        
        with pytest.raises(TypeError, match="fields must be dict, OrderedDict, or list"):
            ne.record(42)
    
    def test_text_encoding_edge_cases(self):
        """Test text encoding with edge cases."""
        # Empty string
        assert ne.text("") == b"t0:,"
        
        # String with null bytes (should work)
        assert ne.text("hello\x00world") == b"t11:hello\x00world,"
        
        # Very long string
        long_string = "a" * 1000
        result = ne.text(long_string)
        assert result == f"t1000:{long_string},".encode('utf-8')
    
    def test_binary_edge_cases(self):
        """Test binary data handling edge cases."""
        # Empty binary
        assert ne.binary(b"") == b"b0:,"
        
        # Binary with null bytes
        assert ne.binary(b"\x00\xff") == b"b2:\x00\xff,"
        
        # Large binary data
        large_data = b"x" * 1000
        result = ne.binary(large_data)
        assert result.startswith(b"b1000:")
        assert len(result) == 1007  # "b1000:" + data + ","