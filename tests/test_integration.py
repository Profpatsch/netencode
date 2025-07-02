"""
Integration tests for netencode pipeline tools using Python netencode module.
Refactored to use direct netencode construction instead of JSON conversion.
"""
import pytest
import subprocess
from conftest import run_tool, get_tool_path
import netencode as ne


class TestJSONToNetencode:
    """Test JSON to netencode conversion functionality."""
    
    def test_converts_simple_object(self):
        """json-to-netencode converts simple object."""
        json_input = '{"name": "Alice", "age": 30}'
        result = run_tool('json-to-netencode', stdin=json_input)
        
        # Should contain both fields in netencode format
        assert b'<4:name|t5:Alice' in result.stdout
        assert b'<3:age|i:30' in result.stdout
    
    def test_converts_boolean_values(self):
        """json-to-netencode converts boolean values."""
        json_input = '{"active": true, "disabled": false}'
        result = run_tool('json-to-netencode', stdin=json_input)
        
        # Booleans should be converted to tagged units
        assert b'<6:active|<4:true|u' in result.stdout
        assert b'<8:disabled|<5:false|u' in result.stdout
    
    def test_converts_arrays(self):
        """json-to-netencode converts arrays."""
        json_input = '{"items": ["foo", "bar"]}'
        result = run_tool('json-to-netencode', stdin=json_input)
        
        # Should contain list with text elements
        assert b'<5:items|[' in result.stdout
        assert b't3:foo,' in result.stdout
        assert b't3:bar,' in result.stdout
    
    def test_handles_numbers_correctly(self):
        """json-to-netencode handles numbers correctly."""
        json_input = '{"positive": 42, "negative": -17, "zero": 0}'
        result = run_tool('json-to-netencode', stdin=json_input)
        
        # Should handle different number types
        assert b'<8:positive|i:42' in result.stdout
        assert b'<8:negative|i:-17' in result.stdout
        assert b'<4:zero|i:0' in result.stdout
    
    def test_handles_null_values(self):
        """json-to-netencode handles null values."""
        json_input = '{"empty": null}'
        result = run_tool('json-to-netencode', stdin=json_input)
        
        # Null should become unit type
        assert b'<5:empty|u' in result.stdout
    
    def test_rejects_malformed_json(self):
        """json-to-netencode rejects malformed JSON."""
        result = run_tool('json-to-netencode', 
                         stdin='{"invalid": json}', 
                         expect_success=False)
        
        # Should fail with non-zero exit code
        assert result.returncode != 0


class TestNetencodeFilter:
    """Test netencode filtering functionality using Python-generated records."""
    
    def test_filters_by_text_field(self):
        """netencode-filter filters by text field."""
        # Create test data using Python module
        record1 = ne.simple_record(name=ne.text("Alice"), role=ne.text("admin"))
        record2 = ne.simple_record(name=ne.text("Bob"), role=ne.text("user"))
        input_stream = record1 + b"\n" + record2
        
        # Filter by role=admin
        result = run_tool('netencode-filter', 'role=admin', stdin=input_stream)
        
        # Should only contain Alice's record
        assert b'Alice' in result.stdout
        assert b'Bob' not in result.stdout
    
    def test_filters_by_number_field(self):
        """netencode-filter filters by number field."""
        record1 = ne.simple_record(name=ne.text("Alice"), age=ne.integer(30))
        record2 = ne.simple_record(name=ne.text("Bob"), age=ne.integer(25))
        input_stream = record1 + b"\n" + record2
        
        # Filter by age=30
        result = run_tool('netencode-filter', 'age=30', stdin=input_stream)
        
        # Should only contain Alice's record
        assert b'Alice' in result.stdout
        assert b'Bob' not in result.stdout
    
    def test_filters_by_boolean_field(self):
        """netencode-filter filters by boolean field."""
        record1 = ne.simple_record(name=ne.text("Alice"), active=ne.boolean(True))
        record2 = ne.simple_record(name=ne.text("Bob"), active=ne.boolean(False))
        input_stream = record1 + b"\n" + record2
        
        # Filter by active=true
        result = run_tool('netencode-filter', 'active=true', stdin=input_stream)
        
        # Should only contain Alice's record
        assert b'Alice' in result.stdout
        assert b'Bob' not in result.stdout
    
    def test_produces_no_output_for_non_matching_records(self):
        """netencode-filter produces no output for non-matching records."""
        record = ne.simple_record(name=ne.text("Alice"), role=ne.text("user"))
        
        result = run_tool('netencode-filter', 'role=admin', stdin=record)
        
        # Should produce no output
        assert result.stdout.strip() == b""
    
    def test_passes_through_non_record_values(self):
        """netencode-filter passes through non-record values."""
        # Test with a simple text value
        text_value = ne.text("hello")
        result = run_tool('netencode-filter', 'field=value', stdin=text_value)
        
        # Should pass through unchanged
        assert result.stdout.strip() == text_value
    
    def test_handles_missing_fields_gracefully(self):
        """netencode-filter handles missing fields gracefully."""
        record = ne.simple_record(name=ne.text("Alice"))
        
        result = run_tool('netencode-filter', 'missing=value', stdin=record)
        
        # Should produce no output for missing field
        assert result.stdout.strip() == b""


class TestPipelineIntegration:
    """Test complete pipeline integration using Python-generated data."""
    
    def test_complete_filter_to_extract_pipeline(self):
        """Test the full pipeline: Python record -> filter -> extract field."""
        record = ne.simple_record(name=ne.text("Alice"), role=ne.text("admin"))
        
        # Filter -> extract name
        filtered = run_tool('netencode-filter', 'role=admin', stdin=record).stdout.strip()
        name = run_tool('netencode-record-get', 'name', stdin=filtered).stdout.strip()
        
        # Should extract Alice's name in netencode format
        assert name == ne.text("Alice")
    
    def test_pipeline_handles_multiple_records_correctly(self):
        """Pipeline handles multiple records correctly."""
        # Create multiple records using Python module
        record1 = ne.simple_record(name=ne.text("Alice"), active=ne.boolean(True))
        record2 = ne.simple_record(name=ne.text("Bob"), active=ne.boolean(False))
        record3 = ne.simple_record(name=ne.text("Charlie"), active=ne.boolean(True))
        
        # Filter for active users
        input_stream = record1 + b"\n" + record2 + b"\n" + record3
        result = run_tool('netencode-filter', 'active=true', stdin=input_stream)
        
        # Should contain Alice and Charlie, but not Bob
        assert b'Alice' in result.stdout
        assert b'Charlie' in result.stdout
        assert b'Bob' not in result.stdout


class TestToolCompatibility:
    """Test compatibility between tools using Python-generated data."""
    
    def test_python_generated_records_work_with_record_get(self):
        """Python-generated records work with netencode-record-get."""
        record = ne.simple_record(name=ne.text("Alice"), age=ne.integer(30))
        
        name_result = run_tool('netencode-record-get', 'name', stdin=record).stdout.strip()
        age_result = run_tool('netencode-record-get', 'age', stdin=record).stdout.strip()
        
        # Should extract fields correctly
        assert name_result == ne.text("Alice")
        assert age_result == ne.integer(30)
    
    def test_filtered_records_work_with_existing_tools(self):
        """Filtered records work with existing tools."""
        record1 = ne.simple_record(name=ne.text("Alice"), role=ne.text("admin"))
        record2 = ne.simple_record(name=ne.text("Bob"), role=ne.text("user"))
        
        # Create netencode stream
        input_stream = record1 + b"\n" + record2
        
        # Filter and extract names
        filtered = run_tool('netencode-filter', 'role=admin', stdin=input_stream).stdout.strip()
        name = run_tool('netencode-record-get', 'name', stdin=filtered).stdout.strip()
        
        # Should get Alice's name
        assert name == ne.text("Alice")
    
    def test_python_module_produces_valid_netencode(self):
        """Test that Python module produces valid netencode that existing tools can consume."""
        record = ne.simple_record(test=ne.text("value"))
        
        # Should be able to extract the field with existing tool
        result = run_tool('netencode-record-get', 'test', stdin=record).stdout.strip()
        assert result == ne.text("value")


class TestFieldOrdering:
    """Test field ordering preservation using Python module for comprehensive control."""
    
    def test_explicit_field_ordering_with_python_module(self):
        """Test creating records with explicit field ordering using Python module."""
        # Create a record with specific field order: zebra, alpha, beta
        record_data = ne.record_ordered(
            "zebra", ne.natural(1),
            "alpha", ne.natural(2), 
            "beta", ne.natural(3)
        )
        
        # Verify the record can be parsed and fields extracted
        zebra_result = run_tool('netencode-record-get', 'zebra', stdin=record_data)
        alpha_result = run_tool('netencode-record-get', 'alpha', stdin=record_data)
        beta_result = run_tool('netencode-record-get', 'beta', stdin=record_data)
        
        assert zebra_result.stdout.strip() == ne.natural(1)
        assert alpha_result.stdout.strip() == ne.natural(2)
        assert beta_result.stdout.strip() == ne.natural(3)
        
        # Verify the field order in the encoded string
        zebra_pos = record_data.find(b'<5:zebra|')
        alpha_pos = record_data.find(b'<5:alpha|')
        beta_pos = record_data.find(b'<4:beta|')
        
        # Fields should appear in the order we specified
        assert zebra_pos < alpha_pos < beta_pos
    
    def test_roundtrip_ordering_preservation_with_python_module(self):
        """Test that Python-generated records preserve ordering through roundtrip."""
        # Create record with non-alphabetical ordering
        original_record = ne.record_ordered(
            "third", ne.text("c"),
            "first", ne.text("a"),
            "second", ne.text("b")
        )
        
        # Pass through netencode-plain (which does passthrough for records)
        passthrough_result = run_tool('netencode-plain', stdin=original_record)
        roundtrip_record = passthrough_result.stdout.strip()
        
        # Should be identical
        assert original_record == roundtrip_record
        
        # Verify field order is preserved
        third_pos = roundtrip_record.find(b'<5:third|')
        first_pos = roundtrip_record.find(b'<5:first|')
        second_pos = roundtrip_record.find(b'<6:second|')
        
        assert third_pos < first_pos < second_pos
    
    def test_different_ordering_methods_produce_different_results(self):
        """Test that different ordering methods produce different encodings."""
        # Method 1: Alphabetical order using simple_record
        record1 = ne.simple_record(
            zebra=ne.text("first"),
            alpha=ne.text("second")
        )
        
        # Method 2: Explicit reverse alphabetical using record_ordered
        record2 = ne.record_ordered(
            "zebra", ne.text("first"),
            "alpha", ne.text("second")
        )
        
        # simple_record sorts alphabetically: alpha before zebra
        alpha_pos1 = record1.find(b'<5:alpha|')
        zebra_pos1 = record1.find(b'<5:zebra|')
        assert alpha_pos1 < zebra_pos1
        
        # record_ordered preserves explicit order: zebra before alpha
        zebra_pos2 = record2.find(b'<5:zebra|')
        alpha_pos2 = record2.find(b'<5:alpha|')
        assert zebra_pos2 < alpha_pos2
        
        # The two records should be different due to different field ordering
        assert record1 != record2
    
    def test_complex_field_ordering_scenario(self):
        """Test complex field ordering with mixed data types."""
        # Create a record with fields that would hash very differently
        record_data = ne.record_ordered(
            "field_999", ne.boolean(True),
            "field_001", ne.integer(-42),
            "field_zzz", ne.text("hello"),
            "field_aaa", ne.unit()
        )
        
        # Verify all fields are accessible
        results = {}
        for field in ["field_999", "field_001", "field_zzz", "field_aaa"]:
            result = run_tool('netencode-record-get', field, stdin=record_data)
            results[field] = result.stdout.strip()
        
        assert results["field_999"] == ne.boolean(True)
        assert results["field_001"] == ne.integer(-42)
        assert results["field_zzz"] == ne.text("hello")
        assert results["field_aaa"] == ne.unit()
        # Verify the explicit ordering is preserved
        positions = {}
        for field in ["field_999", "field_001", "field_zzz", "field_aaa"]:
            field_tag = f'<{len(field)}:{field}|'.encode('utf-8')
            positions[field] = record_data.find(field_tag)
        
        # Should be in the order we specified
        assert positions["field_999"] < positions["field_001"]
        assert positions["field_001"] < positions["field_zzz"] 
        assert positions["field_zzz"] < positions["field_aaa"]
    
    def test_record_field_ordering_preserved_through_roundtrip(self):
        """Test that field ordering is preserved when parsing and re-encoding records."""
        # Create a record with a specific field order using Python module
        original_record = ne.record_ordered(
            "zebra", ne.integer(1),
            "alpha", ne.integer(2),
            "beta", ne.integer(3),
            "gamma", ne.integer(4)
        )
        
        # Parse and re-encode through netencode-plain (which does passthrough for records)
        passthrough_result = run_tool('netencode-plain', stdin=original_record)
        final_record = passthrough_result.stdout.strip()
        
        # The field order should be preserved
        assert original_record == final_record
        
        # Additionally, verify the actual order by extracting each field
        zebra_field = run_tool('netencode-record-get', 'zebra', stdin=original_record)
        alpha_field = run_tool('netencode-record-get', 'alpha', stdin=original_record)
        beta_field = run_tool('netencode-record-get', 'beta', stdin=original_record)
        gamma_field = run_tool('netencode-record-get', 'gamma', stdin=original_record)
        
        # All fields should be accessible
        assert zebra_field.stdout.strip() == ne.integer(1)
        assert alpha_field.stdout.strip() == ne.integer(2)
        assert beta_field.stdout.strip() == ne.integer(3)
        assert gamma_field.stdout.strip() == ne.integer(4)

class TestNetencodePlain:
    """Test the netencode-plain tool for extracting scalar values."""

    def test_text_values(self):
        """Test that text values are output as plain text."""
        text_value = ne.text("Alice")
        result = run_tool("netencode-plain", stdin=text_value)
        assert result.stdout == b"Alice"

    def test_natural_numbers(self):
        """Test that natural numbers are output as decimal."""
        nat_value = ne.natural(42)
        result = run_tool("netencode-plain", stdin=nat_value)
        assert result.stdout == b"42"

    def test_signed_integers(self):
        """Test that signed integers are output as decimal."""
        int_value = ne.integer(-10)
        result = run_tool("netencode-plain", stdin=int_value)
        assert result.stdout == b"-10"

    def test_boolean_true(self):
        """Test that boolean true is output as 'true'."""
        bool_value = ne.boolean(True)
        result = run_tool("netencode-plain", stdin=bool_value)
        assert result.stdout == b"true"

    def test_boolean_false(self):
        """Test that boolean false is output as 'false'."""
        bool_value = ne.boolean(False)
        result = run_tool("netencode-plain", stdin=bool_value)
        assert result.stdout == b"false"

    def test_record_passthrough(self):
        """Test that records pass through unchanged."""
        record = ne.simple_record(name=ne.text("Alice"), age=ne.integer(30))
        result = run_tool("netencode-plain", stdin=record)
        assert result.stdout == record

    def test_list_passthrough(self):
        """Test that lists pass through unchanged."""
        list_value = ne.list([ne.text("Alice"), ne.integer(30)])
        result = run_tool("netencode-plain", stdin=list_value)
        assert result.stdout == list_value

    def test_unit_value(self):
        """Test that unit values produce no output."""
        unit_value = ne.unit()
        result = run_tool("netencode-plain", stdin=unit_value)
        assert result.stdout == b""

    def test_binary_data_passthrough(self):
        """Test that binary data is output as raw bytes."""
        binary_value = ne.binary(b"hello")
        result = run_tool("netencode-plain", stdin=binary_value)
        assert result.stdout == b"hello"


class TestEdgeCases:
    """Test edge cases and error handling using Python-generated data."""
    
    def test_empty_input_handling(self):
        """Test how tools handle empty input."""
        # JSON conversion should fail on empty input
        result = run_tool('json-to-netencode', stdin='', expect_success=False)
        assert result.returncode != 0
        
        # Filter should handle empty input gracefully
        result = run_tool('netencode-filter', 'field=value', stdin='')
        assert result.returncode == 0
    
    def test_unicode_handling_with_python_module(self):
        """Unicode handling using Python module."""
        record = ne.simple_record(
            message=ne.text("Hello 世界"), 
            emoji=ne.text("🌍")
        )
        
        # Should handle unicode properly
        message_result = run_tool('netencode-record-get', 'message', stdin=record)
        emoji_result = run_tool('netencode-record-get', 'emoji', stdin=record)
        
        assert message_result.stdout.strip() == ne.text("Hello 世界")
        assert emoji_result.stdout.strip() == ne.text("🌍")
    
    def test_special_characters_in_field_names_and_values(self):
        """Special characters in field names and values using Python module."""
        # Use record_ordered to specify field with special characters
        record = ne.record_ordered(
            "field:with:colons", ne.text("value,with,commas")
        )
        
        # Should handle special characters correctly (length-prefixed format)
        result = run_tool('netencode-record-get', 'field:with:colons', stdin=record)
        assert result.stdout.strip() == ne.text("value,with,commas")
    
    def test_filter_expression_validation(self):
        """Filter expression validation."""
        record = ne.simple_record(name=ne.text("Alice"))
        
        # Test invalid filter expressions
        result = run_tool('netencode-filter', 'invalid_expression', 
                         stdin=record, expect_success=False)
        assert result.returncode != 0
        
        # This should handle by taking first = as separator
        result = run_tool('netencode-filter', 'field=value=extra', stdin=record)
        assert result.returncode == 0


class TestEnvironmentIntegration:
    """Test integration with environment variable tools."""
    
    def test_env_splice_record_deterministic_ordering(self):
        """Test that env-to-netencode produces deterministic output."""
        import subprocess
        
        env = {
            'first': 'value1',
            'second': 'value2', 
            'third': 'value3'
        }
        
        tool_path = get_tool_path('env-to-netencode')
        result = subprocess.run([tool_path], capture_output=True, env=env)
        encoding = result.stdout.strip()
        
        # Verify that each field is accessible
        for field in ['first', 'second', 'third']:
            field_result = run_tool('netencode-record-get', field, stdin=encoding)
            assert field_result.returncode == 0
            # The value should be the text-encoded environment value
            expected_value = ne.text(env[field])
            assert field_result.stdout.strip() == expected_value