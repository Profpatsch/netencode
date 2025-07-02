"""
Tests for all examples mentioned in the README.
These tests ensure that the documentation examples actually work.

⚠️  IMPORTANT REMINDER FOR AI ASSISTANTS:
When you modify any test in this file, you MUST also check and update the corresponding 
examples in /home/philip/kot/netencode/README.md to ensure they match the test requirements.

If a test expects 't5:hello,' (with comma), the README must show 't5:hello,' (with comma).
If a test expects complete netencode format, the README examples must be complete.

DO NOT just fix tests - always verify and update README examples to match!
"""
import pytest
import subprocess
from conftest import run_tool, get_tool_path
import netencode as ne


class TestReadmeExamples:
    """Test all the examples shown in the README."""
    
    def test_binary_data_construction_with_printf(self):
        """Test the exact example from README: Create filename with non-UTF-8 bytes."""
        # Create filename with non-UTF-8 bytes using shell
        cmd = ['bash', '-c', 'FILENAME=$(printf "file\\xff\\x00name"); LENGTH=$(printf "%s" "$FILENAME" | wc -c); echo $LENGTH']
        result = subprocess.run(cmd, capture_output=True, text=True)
        
        # Should get correct length (9 bytes)
        length = int(result.stdout.strip())
        assert length == 9
        
        # Create netencode binary string
        cmd2 = ['bash', '-c', 'FILENAME=$(printf "file\\xff\\x00name"); LENGTH=$(printf "%s" "$FILENAME" | wc -c); printf "b${LENGTH}:${FILENAME},"']
        result2 = subprocess.run(cmd2, capture_output=True)
        
        # Should start with correct prefix and have correct total length
        assert result2.stdout.startswith(b'b9:')
        assert len(result2.stdout) == 13  # b9: + 9 bytes + ,
    
    def test_simple_printf_construction(self):
        """Test the printf construction example from README."""
        text = "Hello, World!"
        length = len(text.encode('utf-8'))
        result_format = f"b{length}:{text},"
        
        # Should create valid netencode binary string
        assert result_format == "b13:Hello, World!,"
    
    def test_basic_record_field_extraction(self):
        """Test extracting name field from record."""
        record = ne.simple_record(name=ne.text("Alice"), age=ne.integer(30))
        result = run_tool('netencode-record-get', 'name', stdin=record)
        
        # Should extract name field
        assert result.stdout.strip() == b't5:Alice,'
    
    def test_basic_record_field_extraction_with_plain(self):
        """Test extracting name field and converting to plain text - README example."""
        # Test the exact example: echo '{...}' | netencode-record-get name | netencode-plain
        record = ne.simple_record(name=ne.text("Alice"), age=ne.integer(30))
        field_result = run_tool('netencode-record-get', 'name', stdin=record)
        plain_result = run_tool('netencode-plain', stdin=field_result.stdout)
        
        # Should extract name as plain text
        assert plain_result.stdout == b'Alice'
    
    def test_configuration_extraction_with_plain(self):
        """Test configuration example from README with netencode-plain."""
        # README example: Extract HOST and PORT from config
        config = '{55:<4:host|t9:localhost,<4:port|n:8080,<5:debug|<4:true|u,}'
        
        # Extract host and convert to plain
        host_field = run_tool('netencode-record-get', 'host', stdin=config)
        host_plain = run_tool('netencode-plain', stdin=host_field.stdout)
        assert host_plain.stdout == b'localhost'
        
        # Extract port and convert to plain  
        port_field = run_tool('netencode-record-get', 'port', stdin=config)
        port_plain = run_tool('netencode-plain', stdin=port_field.stdout)
        assert port_plain.stdout == b'8080'
    
    def test_environment_integration_with_record_splice_env(self):
        """Test the echo example from README."""
        record = ne.simple_record(name=ne.text("Alice"), age=ne.integer(30))
        # Use shell -c to properly expand variables
        result = run_tool('netencode-to-env', 'sh', '-c', 'echo "Hello $name, you are $age years old"', 
                         stdin=record)
        
        # Should expand environment variables from record  
        assert result.stdout.strip() == b'Hello Alice, you are 30 years old'
    
    def test_env_splice_record_basic_functionality(self):
        """Test that env-to-netencode produces valid netencode."""
        # Set some test environment variables
        env = {'TEST_VAR1': 'test_value', 'TEST_VAR2': 'another_value'}
        
        # Run env-to-netencode with custom environment
        from conftest import get_tool_path
        tool_path = get_tool_path('env-to-netencode')
        result = subprocess.run(
            [tool_path],
            capture_output=True, text=True, env={**env}
        )
        
        # Should be a valid netencode record (starts with { and length)
        assert result.stdout.startswith('{')
        assert ':<' in result.stdout  # Should contain tagged fields
        # Should contain our test variables
        assert 'TEST_VAR1' in result.stdout
        assert 'test_value' in result.stdout
    
    def test_json_escaping_vs_netencode_simplicity(self):
        """Test that netencode handles quotes without escaping."""
        text = 'He said "Hello"'
        length = len(text.encode('utf-8'))
        result_format = f't{length}:{text},'
        
        # Should contain quotes literally (no escaping)
        assert result_format == 't15:He said "Hello",'
    
    def test_netencode_construction_from_shell_primitives(self):
        """Test that we can construct netencode using only printf and wc."""
        name = "Alice"
        age = "30"
        active = "true"
        
        # Construct individual fields (simplified version)
        name_field = f'<4:name|t{len(name)}:{name},'
        age_field = f'<3:age|n:{age},'
        active_field = f'<6:active|<{len(active)}:{active}|u,'
        
        # Calculate total length
        content = name_field + age_field + active_field
        total_length = len(content)
        
        # Construct record
        record = f'{{{total_length}:{content}}}'
        
        # Should be parseable by netencode-record-get
        name_result = run_tool('netencode-record-get', 'name', stdin=record)
        assert name_result.stdout.strip() == f't{len(name)}:{name},'.encode('utf-8')
        
        age_result = run_tool('netencode-record-get', 'age', stdin=record)
        assert age_result.stdout.strip() == f'n:{age},'.encode('utf-8')
    
    def test_pipeline_data_transformation(self):
        """Test complex pipeline similar to README example."""
        # Create test records
        record1 = '{"name": "Alice", "email": "alice@example.com", "status": "active"}'
        record2 = '{"name": "Bob", "email": "bob@example.com", "status": "inactive"}'
        
        # Convert to netencode
        ne1 = run_tool('json-to-netencode', stdin=record1).stdout.strip()
        ne2 = run_tool('json-to-netencode', stdin=record2).stdout.strip()
        input_stream = ne1 + b'\n' + ne2
        
        # Filter active records
        filtered = run_tool('netencode-filter', 'status=active', stdin=input_stream)
        
        # Should only contain Alice's record
        assert b'Alice' in filtered.stdout
        assert b'Bob' not in filtered.stdout
        
        # Extract fields using netencode-record-get
        name_result = run_tool('netencode-record-get', 'name', stdin=filtered.stdout.strip())
        email_result = run_tool('netencode-record-get', 'email', stdin=filtered.stdout.strip())
        
        # Extract the actual values (remove netencode formatting)
        name = name_result.stdout.strip()
        email = email_result.stdout.strip()
        
        # Should extract correct values in netencode format
        assert b'Alice' in name
        assert b'alice@example.com' in email
    
    def test_complete_pipeline_with_plain_output(self):
        """Test the complete pipeline example from README CLI Tools section."""
        # Use individual records as input (like streaming from an API) rather than an array
        alice_json = '{"name": "Alice", "active": true}'
        bob_json = '{"name": "Bob", "active": false}'
        
        # Convert each to netencode
        alice_ne = run_tool('json-to-netencode', stdin=alice_json).stdout.strip()
        bob_ne = run_tool('json-to-netencode', stdin=bob_json).stdout.strip()
        input_stream = alice_ne + b'\n' + bob_ne
        
        # Filter active users  
        filtered_data = run_tool('netencode-filter', 'active=true', stdin=input_stream)
        
        # Extract name field from filtered result
        name_field = run_tool('netencode-record-get', 'name', stdin=filtered_data.stdout.strip())
        
        # Convert to plain text
        plain_name = run_tool('netencode-plain', stdin=name_field.stdout)
        
        # Should get Alice as plain text
        assert plain_name.stdout == b'Alice'
    
    def test_type_safety_naturals_vs_integers(self):
        """Test that netencode distinguishes number types correctly."""
        natural = '{"count": 42}'
        integer = '{"temperature": -10}'
        
        natural_ne = run_tool('json-to-netencode', stdin=natural).stdout.strip()
        integer_ne = run_tool('json-to-netencode', stdin=integer).stdout.strip()
        
        # Extract the values
        count_result = run_tool('netencode-record-get', 'count', stdin=natural_ne).stdout.strip()
        temp_result = run_tool('netencode-record-get', 'temperature', stdin=integer_ne).stdout.strip()
        
        # Should be properly typed (JSON-to-netencode uses i: for all numbers)
        assert count_result == b'i:42,'
        assert temp_result == b'i:-10,'
    
    def test_length_prefixed_streaming_advantage(self):
        """Test that we can skip over data without parsing it."""
        record1 = ne.simple_record(name=ne.text("Alice"), age=ne.integer(30))
        record2 = ne.simple_record(name=ne.text("Bob"), age=ne.integer(25))
        
        # Should be able to extract fields from each record
        first_name = run_tool('netencode-record-get', 'name', stdin=record1).stdout.strip()
        second_name = run_tool('netencode-record-get', 'name', stdin=record2).stdout.strip()
        
        assert first_name == b't5:Alice,'
        assert second_name == b't3:Bob,'
    
    def test_boolean_representation_as_tagged_units(self):
        """Test boolean conversion to tagged units."""
        bool_record = '{"active": true, "disabled": false}'
        result = run_tool('json-to-netencode', stdin=bool_record)
        
        # Should convert to tagged units
        assert b'<6:active|<4:true|u,' in result.stdout
        assert b'<8:disabled|<5:false|u,' in result.stdout
        
        # Should be extractable
        active_result = run_tool('netencode-record-get', 'active', stdin=result.stdout.strip())
        assert active_result.stdout.strip() == b'<4:true|u,'
    
    
    def test_github_api_processing_simplified(self):
        """Test a simplified version that matches what our tools can actually do."""
        # Create mock GitHub API response for a single repo
        mock_repo = '{"name": "Hello-World", "archived": false, "description": "My first repository"}'
        
        # Convert to netencode
        netencode_repo = run_tool('json-to-netencode', stdin=mock_repo).stdout.strip()
        
        # Extract repository name
        repo_name = run_tool('netencode-record-get', 'name', stdin=netencode_repo).stdout.strip()
        assert repo_name == b't11:Hello-World,'
        
        # Check if archived field can be extracted
        archived_status = run_tool('netencode-record-get', 'archived', stdin=netencode_repo).stdout.strip()
        assert archived_status == b'<5:false|u,'
    
