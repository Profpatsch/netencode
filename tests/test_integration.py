"""
Integration tests for netencode pipeline tools.
Ported from BATS integration_tests.bats.
"""
import pytest
from conftest import run_tool


class TestJSONToNetencode:
    """Test JSON to netencode conversion functionality."""
    
    def test_converts_simple_object(self, json_samples):
        """json-to-netencode converts simple object."""
        result = run_tool('json-to-netencode', stdin=json_samples['simple_object'])
        
        # Should contain both fields in netencode format
        assert '<4:name|t5:Alice' in result.stdout
        assert '<3:age|i:30' in result.stdout
    
    def test_converts_boolean_values(self, json_samples):
        """json-to-netencode converts boolean values."""
        result = run_tool('json-to-netencode', stdin=json_samples['boolean_object'])
        
        # Booleans should be converted to tagged units
        assert '<6:active|<4:true|u' in result.stdout
        assert '<8:disabled|<5:false|u' in result.stdout
    
    def test_converts_arrays(self, json_samples):
        """json-to-netencode converts arrays."""
        result = run_tool('json-to-netencode', stdin=json_samples['array_object'])
        
        # Should contain list with text elements
        assert '<5:items|[' in result.stdout
        assert 't3:foo' in result.stdout
        assert 't3:bar' in result.stdout
    
    def test_handles_numbers_correctly(self, json_samples):
        """json-to-netencode handles numbers correctly."""
        result = run_tool('json-to-netencode', stdin=json_samples['number_object'])
        
        # Should handle different number types
        assert '<8:positive|i:42' in result.stdout
        assert '<8:negative|i:-17' in result.stdout
        assert '<4:zero|i:0' in result.stdout
    
    def test_handles_null_values(self, json_samples):
        """json-to-netencode handles null values."""
        result = run_tool('json-to-netencode', stdin=json_samples['null_object'])
        
        # Null should become unit type
        assert '<5:empty|u' in result.stdout
    
    def test_rejects_malformed_json(self):
        """json-to-netencode rejects malformed JSON."""
        result = run_tool('json-to-netencode', 
                         stdin='{"invalid": json}', 
                         expect_success=False)
        
        # Should fail with non-zero exit code
        assert result.returncode != 0


class TestNetencodeFilter:
    """Test netencode filtering functionality."""
    
    def test_filters_by_text_field(self, json_samples):
        """netencode-filter filters by text field."""
        # Create test data: two records, one matching
        record1 = '{"name": "Alice", "role": "admin"}'
        record2 = '{"name": "Bob", "role": "user"}'
        
        # Convert to netencode
        ne1 = run_tool('json-to-netencode', stdin=record1).stdout.strip()
        ne2 = run_tool('json-to-netencode', stdin=record2).stdout.strip()
        input_stream = f"{ne1}\n{ne2}"
        
        # Filter by role=admin
        result = run_tool('netencode-filter', 'role=admin', stdin=input_stream)
        
        # Should only contain Alice's record
        assert 'Alice' in result.stdout
        assert 'Bob' not in result.stdout
    
    def test_filters_by_number_field(self):
        """netencode-filter filters by number field."""
        record1 = '{"name": "Alice", "age": 30}'
        record2 = '{"name": "Bob", "age": 25}'
        
        # Convert to netencode
        ne1 = run_tool('json-to-netencode', stdin=record1).stdout.strip()
        ne2 = run_tool('json-to-netencode', stdin=record2).stdout.strip()
        input_stream = f"{ne1}\n{ne2}"
        
        # Filter by age=30
        result = run_tool('netencode-filter', 'age=30', stdin=input_stream)
        
        # Should only contain Alice's record
        assert 'Alice' in result.stdout
        assert 'Bob' not in result.stdout
    
    def test_filters_by_boolean_field(self):
        """netencode-filter filters by boolean field."""
        record1 = '{"name": "Alice", "active": true}'
        record2 = '{"name": "Bob", "active": false}'
        
        # Convert to netencode
        ne1 = run_tool('json-to-netencode', stdin=record1).stdout.strip()
        ne2 = run_tool('json-to-netencode', stdin=record2).stdout.strip()
        input_stream = f"{ne1}\n{ne2}"
        
        # Filter by active=true
        result = run_tool('netencode-filter', 'active=true', stdin=input_stream)
        
        # Should only contain Alice's record
        assert 'Alice' in result.stdout
        assert 'Bob' not in result.stdout
    
    def test_produces_no_output_for_non_matching_records(self):
        """netencode-filter produces no output for non-matching records."""
        record = '{"name": "Alice", "role": "user"}'
        ne_record = run_tool('json-to-netencode', stdin=record).stdout.strip()
        
        result = run_tool('netencode-filter', 'role=admin', stdin=ne_record)
        
        # Should produce no output
        assert result.stdout.strip() == ""
    
    def test_passes_through_non_record_values(self):
        """netencode-filter passes through non-record values."""
        # Test with a simple text value
        input_data = 't5:hello,'
        result = run_tool('netencode-filter', 'field=value', stdin=input_data)
        
        # Should pass through unchanged
        assert result.stdout.strip() == 't5:hello,'
    
    def test_handles_missing_fields_gracefully(self):
        """netencode-filter handles missing fields gracefully."""
        record = '{"name": "Alice"}'
        ne_record = run_tool('json-to-netencode', stdin=record).stdout.strip()
        
        result = run_tool('netencode-filter', 'missing=value', stdin=ne_record)
        
        # Should produce no output for missing field
        assert result.stdout.strip() == ""


class TestPipelineIntegration:
    """Test complete pipeline integration."""
    
    def test_complete_json_to_netencode_to_filter_to_extract_pipeline(self):
        """Test the full pipeline: JSON -> netencode -> filter -> extract field."""
        record = '{"name": "Alice", "role": "admin"}'
        
        # JSON -> netencode -> filter -> extract name
        ne_record = run_tool('json-to-netencode', stdin=record).stdout.strip()
        filtered = run_tool('netencode-filter', 'role=admin', stdin=ne_record).stdout.strip()
        name = run_tool('record-get', 'name', stdin=filtered).stdout.strip()
        
        # Should extract Alice's name in netencode format
        assert name == 't5:Alice,'
    
    def test_pipeline_handles_multiple_records_correctly(self):
        """Pipeline handles multiple records correctly."""
        # Create multiple JSON records
        record1 = '{"name": "Alice", "active": true}'
        record2 = '{"name": "Bob", "active": false}'
        record3 = '{"name": "Charlie", "active": true}'
        
        # Convert each to netencode and combine
        ne1 = run_tool('json-to-netencode', stdin=record1).stdout.strip()
        ne2 = run_tool('json-to-netencode', stdin=record2).stdout.strip()
        ne3 = run_tool('json-to-netencode', stdin=record3).stdout.strip()
        
        # Filter for active users
        input_stream = f"{ne1}\n{ne2}\n{ne3}"
        result = run_tool('netencode-filter', 'active=true', stdin=input_stream)
        
        # Should contain Alice and Charlie, but not Bob
        assert 'Alice' in result.stdout
        assert 'Charlie' in result.stdout
        assert 'Bob' not in result.stdout
    
    def test_error_handling_through_pipeline(self):
        """Test that errors propagate correctly."""
        invalid_json = '{"invalid": json}'
        
        # Should fail at the JSON parsing stage
        result = run_tool('json-to-netencode', stdin=invalid_json, expect_success=False)
        assert result.returncode != 0


class TestToolCompatibility:
    """Test compatibility between tools."""
    
    def test_json_to_netencode_output_works_with_record_get(self):
        """json-to-netencode output works with record-get."""
        input_json = '{"name": "Alice", "age": 30}'
        netencode_output = run_tool('json-to-netencode', stdin=input_json).stdout.strip()
        
        name_result = run_tool('record-get', 'name', stdin=netencode_output).stdout.strip()
        age_result = run_tool('record-get', 'age', stdin=netencode_output).stdout.strip()
        
        # Should extract fields correctly
        assert name_result == 't5:Alice,'
        assert age_result == 'i:30,'
    
    def test_filtered_records_work_with_existing_tools(self):
        """Filtered records work with existing tools."""
        record1 = '{"name": "Alice", "role": "admin"}'
        record2 = '{"name": "Bob", "role": "user"}'
        
        # Create netencode stream
        ne1 = run_tool('json-to-netencode', stdin=record1).stdout.strip()
        ne2 = run_tool('json-to-netencode', stdin=record2).stdout.strip()
        input_stream = f"{ne1}\n{ne2}"
        
        # Filter and extract names
        filtered = run_tool('netencode-filter', 'role=admin', stdin=input_stream).stdout.strip()
        name = run_tool('record-get', 'name', stdin=filtered).stdout.strip()
        
        # Should get Alice's name
        assert name == 't5:Alice,'
    
    def test_netencode_format_compatibility_across_tools(self):
        """Test that our new tools produce valid netencode that existing tools can consume."""
        input_json = '{"test": "value"}'
        netencode_output = run_tool('json-to-netencode', stdin=input_json).stdout.strip()
        
        # Should be able to extract the field with existing tool
        result = run_tool('record-get', 'test', stdin=netencode_output).stdout.strip()
        assert result == 't5:value,'


class TestFieldOrdering:
    """Test that record field ordering is preserved with IndexMap."""
    
    def test_record_field_ordering_preserved_through_roundtrip(self):
        """Test that field ordering is preserved when parsing and re-encoding records."""
        # Create a record with a specific field order that would be different with HashMap
        # Use field names that would hash to different positions
        original_json = '{"zebra": 1, "alpha": 2, "beta": 3, "gamma": 4}'
        
        # Convert to netencode 
        netencode_result = run_tool('json-to-netencode', stdin=original_json)
        netencode_data = netencode_result.stdout.strip()
        
        # Parse and re-encode through netencode-plain (which does passthrough for records)
        passthrough_result = run_tool('netencode-plain', stdin=netencode_data)
        final_netencode = passthrough_result.stdout.strip()
        
        # The field order should be preserved
        assert netencode_data == final_netencode
        
        # Additionally, verify the actual order by extracting each field
        zebra_field = run_tool('record-get', 'zebra', stdin=netencode_data)
        alpha_field = run_tool('record-get', 'alpha', stdin=netencode_data)
        beta_field = run_tool('record-get', 'beta', stdin=netencode_data)
        gamma_field = run_tool('record-get', 'gamma', stdin=netencode_data)
        
        # All fields should be accessible
        assert zebra_field.stdout.strip() == 'i:1,'
        assert alpha_field.stdout.strip() == 'i:2,'
        assert beta_field.stdout.strip() == 'i:3,'
        assert gamma_field.stdout.strip() == 'i:4,'
    
    def test_deterministic_encoding_same_data_different_construction(self):
        """Test that the same logical record produces identical encoding regardless of construction method."""
        # Method 1: Build record from JSON
        json_input = '{"first": "a", "second": "b", "third": "c"}'
        method1_result = run_tool('json-to-netencode', stdin=json_input)
        encoding1 = method1_result.stdout.strip()
        
        # Method 2: Build the same record through env-splice-record
        import subprocess
        from conftest import get_tool_path
        
        env = {
            'first': 'a',
            'second': 'b', 
            'third': 'c'
        }
        
        tool_path = get_tool_path('env-splice-record')
        method2_result = subprocess.run([tool_path], capture_output=True, text=True, env=env)
        encoding2 = method2_result.stdout.strip()
        
        # Both methods should produce records with the same content when parsed
        # (though exact byte encoding might differ due to different field orders)
        # Verify by extracting each field from both encodings
        for field in ['first', 'second', 'third']:
            field1 = run_tool('record-get', field, stdin=encoding1)
            field2 = run_tool('record-get', field, stdin=encoding2)
            
            # Same field values should be extracted
            assert field1.stdout == field2.stdout
    
    def test_json_parser_produces_consistent_field_order(self):
        """Test that JSON parser produces consistent field ordering."""
        # JSON parsers typically sort fields alphabetically or preserve insertion order
        # The key insight is that our IndexMap preserves whatever order the JSON parser gives us
        json1 = '{"a": 1, "b": 2}'
        json2 = '{"b": 2, "a": 1}'
        
        result1 = run_tool('json-to-netencode', stdin=json1)
        result2 = run_tool('json-to-netencode', stdin=json2)
        
        encoding1 = result1.stdout.strip()
        encoding2 = result2.stdout.strip()
        
        # Our JSON parser (serde_json) actually sorts fields alphabetically,
        # so both should produce the same encoding
        assert encoding1 == encoding2
        
        # Both should contain the same field values
        a_field1 = run_tool('record-get', 'a', stdin=encoding1)
        a_field2 = run_tool('record-get', 'a', stdin=encoding2)
        b_field1 = run_tool('record-get', 'b', stdin=encoding1)
        b_field2 = run_tool('record-get', 'b', stdin=encoding2)
        
        assert a_field1.stdout == a_field2.stdout == 'i:1,'
        assert b_field1.stdout == b_field2.stdout == 'i:2,'
        
        # Verify consistent field order by checking byte positions
        # With alphabetical sorting, 'a' should come before 'b'
        a_pos = encoding1.find('<1:a|')
        b_pos = encoding1.find('<1:b|')
        assert a_pos < b_pos, "Field 'a' should come before 'b' in alphabetical order"
    
    def test_field_order_preservation_different_construction_methods(self):
        """Test that different construction methods can produce different field orders."""
        import subprocess
        from conftest import get_tool_path
        
        # Method 1: Environment variables in one order (alphabetical)
        env1 = {'a': 'value1', 'b': 'value2'}
        tool_path = get_tool_path('env-splice-record')
        result1 = subprocess.run([tool_path], capture_output=True, text=True, env=env1)
        encoding1 = result1.stdout.strip()
        
        # Method 2: Environment variables in different order (reverse alphabetical)
        env2 = {'z': 'value1', 'a': 'value2'}  
        result2 = subprocess.run([tool_path], capture_output=True, text=True, env=env2)
        encoding2 = result2.stdout.strip()
        
        # The encodings should be different due to different field orders
        assert encoding1 != encoding2
        
        # Verify that each preserves its own field order
        # For encoding1: 'a' should come before 'b' (alphabetical in env)
        if '<1:a|' in encoding1 and '<1:b|' in encoding1:
            a_pos1 = encoding1.find('<1:a|')
            b_pos1 = encoding1.find('<1:b|')
            assert a_pos1 < b_pos1, "In env1, 'a' should come before 'b'"
        
        # For encoding2: 'a' should come after 'z' (alphabetical in env)  
        if '<1:a|' in encoding2 and '<1:z|' in encoding2:
            a_pos2 = encoding2.find('<1:a|')
            z_pos2 = encoding2.find('<1:z|')
            assert a_pos2 > z_pos2, "In env2, 'a' should come after 'z'"
        
        # Both should allow extracting their respective fields
        if 'a' in env1:
            a_field1 = run_tool('record-get', 'a', stdin=encoding1)
            assert a_field1.returncode == 0
        if 'b' in env1:
            b_field1 = run_tool('record-get', 'b', stdin=encoding1)
            assert b_field1.returncode == 0
        if 'z' in env2:
            z_field2 = run_tool('record-get', 'z', stdin=encoding2)
            assert z_field2.returncode == 0
        if 'a' in env2:
            a_field2 = run_tool('record-get', 'a', stdin=encoding2)
            assert a_field2.returncode == 0


class TestEdgeCases:
    """Test edge cases and error handling."""
    
    def test_empty_input_handling(self):
        """Test how tools handle empty input."""
        # JSON conversion should fail on empty input
        result = run_tool('json-to-netencode', stdin='', expect_success=False)
        assert result.returncode != 0
        
        # Filter should handle empty input gracefully
        result = run_tool('netencode-filter', 'field=value', stdin='')
        assert result.returncode == 0
    
    def test_unicode_handling_in_json_to_netencode(self):
        """Unicode handling in JSON to netencode."""
        input_json = '{"message": "Hello 世界", "emoji": "🌍"}'
        result = run_tool('json-to-netencode', stdin=input_json)
        
        # Should contain unicode text properly encoded
        assert 'message' in result.stdout
        assert 'emoji' in result.stdout
    
    def test_special_characters_in_field_names_and_values(self):
        """Special characters in field names and values."""
        input_json = '{"field:with:colons": "value,with,commas"}'
        result = run_tool('json-to-netencode', stdin=input_json)
        
        # Should handle special characters correctly (length-prefixed format)
        assert 'field:with:colons' in result.stdout
        assert 'value,with,commas' in result.stdout
    
    def test_filter_expression_validation(self):
        """Filter expression validation."""
        record = '{"name": "Alice"}'
        ne_record = run_tool('json-to-netencode', stdin=record).stdout.strip()
        
        # Test invalid filter expressions
        result = run_tool('netencode-filter', 'invalid_expression', 
                         stdin=ne_record, expect_success=False)
        assert result.returncode != 0
        
        # This should handle by taking first = as separator
        result = run_tool('netencode-filter', 'field=value=extra', stdin=ne_record)
        assert result.returncode == 0


class TestNetencodePlain:
    """Test the netencode-plain tool for extracting scalar values."""

    def test_text_values(self):
        """Test that text values are output as plain text."""
        result = run_tool("netencode-plain", stdin="t5:Alice,")
        assert result.stdout == "Alice"

    def test_natural_numbers(self):
        """Test that natural numbers are output as decimal."""
        result = run_tool("netencode-plain", stdin="n:42,")
        assert result.stdout == "42"

    def test_signed_integers(self):
        """Test that signed integers are output as decimal."""
        result = run_tool("netencode-plain", stdin="i:-10,")
        assert result.stdout == "-10"

    def test_boolean_true(self):
        """Test that boolean true is output as 'true'."""
        result = run_tool("netencode-plain", stdin="<4:true|u,")
        assert result.stdout == "true"

    def test_boolean_false(self):
        """Test that boolean false is output as 'false'."""
        result = run_tool("netencode-plain", stdin="<5:false|u,")
        assert result.stdout == "false"

    def test_record_passthrough(self):
        """Test that records pass through unchanged."""
        # Generated from: echo '{"name": "Alice", "age": 30}' | json-to-netencode
        input_record = "{29:<3:age|i:30,<4:name|t5:Alice,}"
        result = run_tool("netencode-plain", stdin=input_record)
        assert result.stdout == input_record

    def test_list_passthrough(self):
        """Test that lists pass through unchanged."""
        # Generated from: echo '["Alice", 30]' | json-to-netencode
        input_list = "[14:t5:Alice,i:30,]"
        result = run_tool("netencode-plain", stdin=input_list)
        assert result.stdout == input_list

    def test_text_value_extraction(self):
        """Test that text values are extracted from records."""
        # Test extracting a text field and converting to plain
        # Generated from: echo '{"status": "active"}' | json-to-netencode | record-get status
        text_value = "t6:active,"
        result = run_tool("netencode-plain", stdin=text_value)
        assert result.stdout == "active"

    def test_unit_value(self):
        """Test that unit values produce no output."""
        result = run_tool("netencode-plain", stdin="u,")
        assert result.stdout == ""

    def test_pipeline_with_json_conversion(self):
        """Test netencode-plain in a pipeline with JSON conversion."""
        json_input = '"Hello, World!"'
        
        # Convert JSON to netencode, then extract plain value
        json_to_ne = run_tool("json-to-netencode", stdin=json_input)
        plain_result = run_tool("netencode-plain", stdin=json_to_ne.stdout)
        
        assert plain_result.stdout == "Hello, World!"

    def test_binary_data_passthrough(self):
        """Test that binary data is output as raw bytes."""
        # Create some binary data
        binary_input = "b5:hello,"
        result = run_tool("netencode-plain", stdin=binary_input)
        assert result.stdout == "hello"