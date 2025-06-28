"""
Test the netencode_py module itself.
"""
import pytest
import netencode_py as ne
from collections import OrderedDict


class TestBasicTypes:
    """Test basic netencode type constructors."""
    
    def test_unit(self):
        """Test unit value construction."""
        assert ne.unit() == "u,"
    
    def test_natural_numbers(self):
        """Test natural number construction."""
        assert ne.natural(0) == "n:0,"
        assert ne.natural(42) == "n:42,"
        assert ne.natural(2**64 - 1) == "n:18446744073709551615,"
        
        # Test error cases
        with pytest.raises(ValueError, match="Natural numbers must be non-negative"):
            ne.natural(-1)
        
        with pytest.raises(ValueError, match="Natural number too large"):
            ne.natural(2**64)
    
    def test_integers(self):
        """Test signed integer construction."""
        assert ne.integer(0) == "i:0,"
        assert ne.integer(42) == "i:42,"
        assert ne.integer(-42) == "i:-42,"
        assert ne.integer(2**63 - 1) == "i:9223372036854775807,"
        assert ne.integer(-(2**63)) == "i:-9223372036854775808,"
        
        # Test error cases
        with pytest.raises(ValueError, match="Integer out of 64-bit signed range"):
            ne.integer(2**63)
        
        with pytest.raises(ValueError, match="Integer out of 64-bit signed range"):
            ne.integer(-(2**63) - 1)
    
    def test_booleans(self):
        """Test boolean construction."""
        assert ne.boolean(True) == "<4:true|u,"
        assert ne.boolean(False) == "<5:false|u,"
    
    def test_text(self):
        """Test text string construction."""
        assert ne.text("") == "t0:,"
        assert ne.text("hello") == "t5:hello,"
        assert ne.text("Hello, World!") == "t13:Hello, World!,"
        
        # Test UTF-8 encoding
        assert ne.text("café") == "t5:café,"  # é is 2 bytes in UTF-8
        assert ne.text("🌍") == "t4:🌍,"      # emoji is 4 bytes in UTF-8
        
        # Test special characters
        assert ne.text("He said \"hi\"") == "t12:He said \"hi\","
        assert ne.text("line1\nline2") == "t11:line1\nline2,"
    
    def test_binary(self):
        """Test binary data construction."""
        assert ne.binary(b"") == "b0:,"
        assert ne.binary(b"hello") == "b5:hello,"
        assert ne.binary(b"\x00\x01\x02") == "b3:\x00\x01\x02,"


class TestCompositeTypes:
    """Test composite type constructors."""
    
    def test_tag(self):
        """Test tagged value construction."""
        assert ne.tag("foo", "u,") == "<3:foo|u,"
        assert ne.tag("", "i:42,") == "<0:|i:42,"
        assert ne.tag("Some", "t5:value,") == "<4:Some|t5:value,"
        
        # Test UTF-8 tag names
        assert ne.tag("café", "u,") == "<5:café|u,"
    
    def test_record_from_dict(self):
        """Test record construction from regular dict (sorted)."""
        # Regular dicts are sorted by key
        record = ne.record({"b": "t1:2,", "a": "t1:1,"})
        
        # Should be sorted alphabetically
        assert "<1:a|t1:1," in record
        assert "<1:b|t1:2," in record
        
        # Check ordering
        a_pos = record.find("<1:a|")
        b_pos = record.find("<1:b|")
        assert a_pos < b_pos
    
    def test_record_from_ordered_dict(self):
        """Test record construction from OrderedDict."""
        fields = OrderedDict()
        fields["zebra"] = "t1:1,"
        fields["alpha"] = "t1:2,"
        
        record = ne.record(fields)
        
        # Should preserve order from OrderedDict
        zebra_pos = record.find("<5:zebra|")
        alpha_pos = record.find("<5:alpha|")
        assert zebra_pos < alpha_pos
    
    def test_record_from_list(self):
        """Test record construction from list of tuples."""
        fields = [("third", "t1:c,"), ("first", "t1:a,"), ("second", "t1:b,")]
        record = ne.record(fields)
        
        # Should preserve order from list
        third_pos = record.find("<5:third|")
        first_pos = record.find("<5:first|")
        second_pos = record.find("<6:second|")
        
        assert third_pos < first_pos < second_pos
    
    def test_record_length_calculation(self):
        """Test that record length is calculated correctly."""
        # Simple single field record
        record = ne.record([("a", "u,")])
        # Content: <1:a|u, = 7 bytes
        assert record.startswith("{7:")
        
        # Multi-field record
        record = ne.record([("foo", "i:42,"), ("bar", "t3:baz,")])
        # Content: <3:foo|i:42,<3:bar|t3:baz, = 26 bytes
        assert record.startswith("{26:")
    
    def test_list_values(self):
        """Test list construction."""
        # Empty list
        empty_list = ne.list_values([])
        assert empty_list == "[0:]"
        
        # Single item list
        single_list = ne.list_values(["t5:hello,"])
        assert single_list == "[9:t5:hello,]"
        
        # Multi-item list
        multi_list = ne.list_values(["t3:foo,", "i:42,", "u,"])
        # Content: t3:foo,i:42,u, = 14 bytes
        assert multi_list == "[14:t3:foo,i:42,u,]"
    
    def test_record_ordered(self):
        """Test record_ordered convenience function."""
        record = ne.record_ordered(
            "second", "t1:2,",
            "first", "t1:1,"
        )
        
        # Should maintain explicit order
        second_pos = record.find("<6:second|")
        first_pos = record.find("<5:first|")
        assert second_pos < first_pos
        
        # Test error case
        with pytest.raises(ValueError, match="even number of arguments"):
            ne.record_ordered("key1", "value1", "key2")
    
    def test_simple_record(self):
        """Test simple_record convenience function."""
        record = ne.simple_record(zebra="t1:1,", alpha="t1:2,")
        
        # Should be alphabetically sorted
        alpha_pos = record.find("<5:alpha|")
        zebra_pos = record.find("<5:zebra|")
        assert alpha_pos < zebra_pos


class TestComplexScenarios:
    """Test complex netencode construction scenarios."""
    
    def test_nested_structures(self):
        """Test nested records and lists."""
        # Create nested record containing a list
        inner_list = ne.list_values([ne.text("foo"), ne.text("bar")])
        record = ne.record([("items", inner_list)])
        
        assert "<5:items|[" in record
        assert "t3:foo," in record
        assert "t3:bar," in record
    
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
        assert "<10:unit_field|u," in record
        assert "<9:nat_field|n:42," in record
        assert "<9:int_field|i:-10," in record
        assert "<10:bool_field|<4:true|u," in record
        assert "<10:text_field|t5:hello," in record
        assert "<12:binary_field|b4:data," in record
        assert "<9:tag_field|<4:Some|t5:value," in record
        assert "<10:list_field|[" in record
    
    def test_field_ordering_edge_cases(self):
        """Test field ordering with edge cases."""
        # Fields with same prefix but different lengths
        record = ne.record_ordered(
            "a", ne.text("1"),
            "aa", ne.text("2"),
            "aaa", ne.text("3")
        )
        
        # Verify ordering is maintained
        a_pos = record.find("<1:a|t1:1,")
        aa_pos = record.find("<2:aa|t1:2,")
        aaa_pos = record.find("<3:aaa|t1:3,")
        
        assert a_pos < aa_pos < aaa_pos
    
    def test_unicode_edge_cases(self):
        """Test Unicode handling in various contexts."""
        # Unicode in field names
        record = ne.record([("café", ne.text("value"))])
        assert "<5:café|t5:value," in record
        
        # Unicode in text values
        text_val = ne.text("Hello 世界 🌍")
        assert "t17:Hello 世界 🌍," == text_val  # 17 bytes in UTF-8
        
        # Unicode in tags
        tagged = ne.tag("世界", ne.unit())
        assert "<6:世界|u," == tagged  # 6 bytes for "世界" in UTF-8
    
    def test_large_structures(self):
        """Test handling of larger structures."""
        # Create a record with many fields
        fields = []
        for i in range(100):
            fields.append((f"field_{i:03d}", ne.natural(i)))
        
        record = ne.record(fields)
        
        # Should handle large structures correctly
        assert record.startswith("{")
        assert record.endswith("}")
        
        # Verify some fields are present
        assert "<9:field_000|n:0," in record
        assert "<9:field_099|n:99," in record


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
        assert ne.text("") == "t0:,"
        
        # String with null bytes (should work)
        assert ne.text("hello\x00world") == "t11:hello\x00world,"
        
        # Very long string
        long_string = "a" * 1000
        result = ne.text(long_string)
        assert result == f"t1000:{long_string},"
    
    def test_binary_edge_cases(self):
        """Test binary data handling edge cases."""
        # Empty binary
        assert ne.binary(b"") == "b0:,"
        
        # Binary with null bytes
        assert ne.binary(b"\x00\xff") == "b2:\x00\xff,"
        
        # Large binary data
        large_data = b"x" * 1000
        result = ne.binary(large_data)
        assert result.startswith("b1000:")
        assert len(result) == 1007  # "b1000:" + data + ","