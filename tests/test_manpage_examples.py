"""
Tests for all examples mentioned in the man pages.
These tests ensure that the man page documentation examples actually work.

⚠️  IMPORTANT REMINDER FOR AI ASSISTANTS:
When you modify any test in this file, you MUST also check and update the corresponding 
examples in the man pages (/home/philip/kot/netencode/man/*.scd) to ensure they match the test requirements.

If a test expects 't5:hello,' (with comma), the man page must show 't5:hello,' (with comma).
If a test expects complete netencode format, the man page examples must be complete.

DO NOT just fix tests - always verify and update man page examples to match!
"""
import pytest
import subprocess
import os
from conftest import run_tool, get_tool_path
import netencode as ne


class TestNetencodeFormat:
    """Test examples from netencode.5.scd format specification."""
    
    def test_main_synopsis_record_example(self):
        """Test the main record example from netencode.5.scd SYNOPSIS section."""
        # From man page: {49:<6:active|<4:true|u,<3:age|i:30,<4:name|t5:Alice,}
        record = '{49:<6:active|<4:true|u,<3:age|i:30,<4:name|t5:Alice,}'
        
        # Should parse correctly with netencode-pretty
        result = run_tool('netencode-pretty', stdin=record)
        assert result.returncode == 0
        
        # Should be extractable with record-get
        name_result = run_tool('netencode-record-get', 'name', stdin=record)
        assert name_result.stdout.strip() == b't5:Alice,'
        
        age_result = run_tool('netencode-record-get', 'age', stdin=record)
        assert age_result.stdout.strip() == b'i:30,'
        
        active_result = run_tool('netencode-record-get', 'active', stdin=record)
        assert active_result.stdout.strip() == b'<4:true|u,'
    
    def test_main_synopsis_list_example(self):
        """Test the main list example from netencode.5.scd SYNOPSIS section."""
        # From man page: [23:t5:hello,t5:world,i:42,]
        list_data = '[23:t5:hello,t5:world,i:42,]'
        
        # Should parse correctly with netencode-pretty
        result = run_tool('netencode-pretty', stdin=list_data)
        assert result.returncode == 0
    
    def test_binary_data_example(self):
        """Test binary data example from netencode.5.scd SYNOPSIS section."""
        # From man page: b13:Hello, World!,
        binary_data = 'b13:Hello, World!,'
        
        # Should parse correctly with netencode-pretty
        result = run_tool('netencode-pretty', stdin=binary_data)
        assert result.returncode == 0
    
    def test_scalar_type_examples(self):
        """Test scalar type examples from netencode.5.scd."""
        # Unit type
        unit_data = 'u,'
        result = run_tool('netencode-pretty', stdin=unit_data)
        assert result.returncode == 0
        
        # Natural number
        natural_data = 'n:42,'
        result = run_tool('netencode-pretty', stdin=natural_data)
        assert result.returncode == 0
        
        # Signed integer
        integer_data = 'i:-42,'
        result = run_tool('netencode-pretty', stdin=integer_data)
        assert result.returncode == 0
        
        # Boolean true
        bool_true = '<4:true|u,'
        result = run_tool('netencode-pretty', stdin=bool_true)
        assert result.returncode == 0
        
        # Boolean false
        bool_false = '<5:false|u,'
        result = run_tool('netencode-pretty', stdin=bool_false)
        assert result.returncode == 0
    
    def test_text_examples(self):
        """Test text examples from netencode.5.scd."""
        # Regular text
        text_data = 't11:hello world,'
        result = run_tool('netencode-pretty', stdin=text_data)
        assert result.returncode == 0
        
        # Text with quotes
        text_quotes = 't12:He said "hi",'
        result = run_tool('netencode-pretty', stdin=text_quotes)
        assert result.returncode == 0
        
        # Empty string
        empty_text = 't0:,'
        result = run_tool('netencode-pretty', stdin=empty_text)
        assert result.returncode == 0
    
    def test_binary_examples(self):
        """Test binary examples from netencode.5.scd."""
        # Binary data
        binary_data = 'b5:hello,'
        result = run_tool('netencode-pretty', stdin=binary_data)
        assert result.returncode == 0
        
        # Empty binary
        empty_binary = 'b0:,'
        result = run_tool('netencode-pretty', stdin=empty_binary)
        assert result.returncode == 0
    
    def test_tagged_value_examples(self):
        """Test tagged value examples from netencode.5.scd."""
        # Basic tagged value
        tagged_data = '<3:foo|t5:hello,'
        result = run_tool('netencode-pretty', stdin=tagged_data)
        assert result.returncode == 0
        
        # Empty tag
        empty_tag = '<0:|i:42,'
        result = run_tool('netencode-pretty', stdin=empty_tag)
        assert result.returncode == 0
        
        # Nested tag
        nested_tag = '<4:Some|<4:data|t6:secret,'
        result = run_tool('netencode-pretty', stdin=nested_tag)
        assert result.returncode == 0
    
    def test_record_examples(self):
        """Test record examples from netencode.5.scd."""
        # Multiple fields
        multi_record = '{21:<3:foo|u,<1:x|t3:baz,}'
        result = run_tool('netencode-pretty', stdin=multi_record)
        assert result.returncode == 0
        
        # Single field
        single_record = '{9:<3:foo|u,}'
        result = run_tool('netencode-pretty', stdin=single_record)
        assert result.returncode == 0
    
    def test_list_examples(self):
        """Test list examples from netencode.5.scd."""
        # Basic list
        list_data = '[13:t3:foo,i:-42,]'
        result = run_tool('netencode-pretty', stdin=list_data)
        assert result.returncode == 0
        
        # Empty list
        empty_list = '[0:]'
        result = run_tool('netencode-pretty', stdin=empty_list)
        assert result.returncode == 0
        
        # Nested list
        nested_list = '[20:[7:t3:foo,]t5:hello,]'
        result = run_tool('netencode-pretty', stdin=nested_list)
        assert result.returncode == 0
    
    def test_complex_examples(self):
        """Test complex examples from netencode.5.scd."""
        # User record
        user_record = '{49:<6:active|<4:true|u,<3:age|i:30,<4:name|t5:Alice,}'
        result = run_tool('netencode-pretty', stdin=user_record)
        assert result.returncode == 0
        
        # List of names
        names_list = '[27:t5:Alice,t3:Bob,t7:Charlie,]'
        result = run_tool('netencode-pretty', stdin=names_list)
        assert result.returncode == 0
        
        # Optional value (Some)
        optional_some = '<4:Some|t5:value,'
        result = run_tool('netencode-pretty', stdin=optional_some)
        assert result.returncode == 0
        
        # Optional value (None)
        optional_none = '<4:None|u,'
        result = run_tool('netencode-pretty', stdin=optional_none)
        assert result.returncode == 0
    
    def test_nested_structure_examples(self):
        """Test nested structure examples from netencode.5.scd."""
        # Record containing a list (from json-to-netencode output)
        record_with_list = '{29:<5:items|[14:t3:foo,t3:bar,],}'
        result = run_tool('netencode-pretty', stdin=record_with_list)
        assert result.returncode == 0
        
        # List of records (from json-to-netencode output)
        list_of_records = '[40:{15:<4:name|t3:foo,}{15:<4:name|t3:bar,}]'
        result = run_tool('netencode-pretty', stdin=list_of_records)
        assert result.returncode == 0


class TestNetencodePretty:
    """Test examples from netencode-pretty.1.scd."""
    
    def test_format_compact_record(self):
        """Test formatting compact record example."""
        # From man page: {49:<6:active|<4:true|u,<3:age|i:30,<4:name|t5:Alice,}
        record = '{49:<6:active|<4:true|u,<3:age|i:30,<4:name|t5:Alice,}'
        
        result = run_tool('netencode-pretty', stdin=record)
        assert result.returncode == 0
        # Output should be formatted (not compact) and different from input
        assert result.stdout != record.encode('utf-8')
        assert b'active' in result.stdout
        assert b'Alice' in result.stdout
    
    def test_format_nested_structures(self):
        """Test formatting nested structures example."""
        # From man page: [42:{12:<4:name|t3:foo,}{12:<4:name|t3:bar,}]
        nested_data = '[40:{15:<4:name|t3:foo,}{15:<4:name|t3:bar,}]'
        
        result = run_tool('netencode-pretty', stdin=nested_data)
        assert result.returncode == 0
        # Should contain the text content
        assert b'foo' in result.stdout
        assert b'bar' in result.stdout


class TestNetencodePlain:
    """Test examples from netencode-plain.1.scd."""
    
    def test_convert_text_to_plain(self):
        """Test converting text to plain output."""
        # From man page: t5:hello,
        text_data = 't5:hello,'
        
        result = run_tool('netencode-plain', stdin=text_data)
        assert result.returncode == 0
        assert result.stdout == b'hello'
    
    def test_convert_numbers(self):
        """Test converting numbers to plain output."""
        # Positive integer
        pos_int = 'i:42,'
        result = run_tool('netencode-plain', stdin=pos_int)
        assert result.returncode == 0
        assert result.stdout == b'42'
        
        # Negative integer  
        neg_int = 'i:-100,'
        result = run_tool('netencode-plain', stdin=neg_int)
        assert result.returncode == 0
        assert result.stdout == b'-100'
    
    def test_convert_booleans(self):
        """Test converting booleans to plain output."""
        # Boolean true
        bool_true = '<4:true|u,'
        result = run_tool('netencode-plain', stdin=bool_true)
        assert result.returncode == 0
        assert result.stdout == b'true'
        
        # Boolean false
        bool_false = '<5:false|u,'
        result = run_tool('netencode-plain', stdin=bool_false)
        assert result.returncode == 0
        assert result.stdout == b'false'
    
    def test_pipeline_field_extraction(self):
        """Test pipeline example extracting field values."""
        # From man page: {29:<3:age|i:30,<4:name|t5:Alice,} | record-get name | plain
        record = '{29:<3:age|i:30,<4:name|t5:Alice,}'
        
        # Extract name field
        name_result = run_tool('netencode-record-get', 'name', stdin=record)
        assert name_result.returncode == 0
        
        # Convert to plain text
        plain_result = run_tool('netencode-plain', stdin=name_result.stdout)
        assert plain_result.returncode == 0
        assert plain_result.stdout == b'Alice'


class TestNetencodeRecordGet:
    """Test examples from netencode-record-get.1.scd."""
    
    def test_extract_name_field(self):
        """Test extracting name field from record."""
        # From man page: {49:<6:active|<4:true|u,<3:age|i:30,<4:name|t5:Alice,}
        record = '{49:<6:active|<4:true|u,<3:age|i:30,<4:name|t5:Alice,}'
        
        result = run_tool('netencode-record-get', 'name', stdin=record)
        assert result.returncode == 0
        assert result.stdout.strip() == b't5:Alice,'
    
    def test_extract_numeric_field(self):
        """Test extracting numeric field."""
        # From man page: {49:<6:active|<4:true|u,<3:age|i:30,<4:name|t5:Alice,}
        record = '{49:<6:active|<4:true|u,<3:age|i:30,<4:name|t5:Alice,}'
        
        result = run_tool('netencode-record-get', 'age', stdin=record)
        assert result.returncode == 0
        assert result.stdout.strip() == b'i:30,'
    
    def test_chain_with_plain(self):
        """Test chaining with netencode-plain."""
        # From man page: {49:<6:active|<4:true|u,<3:age|i:30,<4:name|t5:Alice,}
        record = '{49:<6:active|<4:true|u,<3:age|i:30,<4:name|t5:Alice,}'
        
        # Extract name and convert to plain
        name_result = run_tool('netencode-record-get', 'name', stdin=record)
        plain_result = run_tool('netencode-plain', stdin=name_result.stdout)
        
        assert plain_result.returncode == 0
        assert plain_result.stdout == b'Alice'


class TestNetencodeToEnv:
    """Test examples from netencode-to-env.1.scd."""
    
    def test_execute_with_user_data(self):
        """Test executing command with user data."""
        # From man page: {49:<6:active|<4:true|u,<3:age|i:30,<4:name|t5:Alice,}
        record = '{49:<6:active|<4:true|u,<3:age|i:30,<4:name|t5:Alice,}'
        
        result = run_tool('netencode-to-env', 'sh', '-c', 'echo "Hello $name, you are $age years old"', stdin=record)
        assert result.returncode == 0
        assert result.stdout.strip() == b'Hello Alice, you are 30 years old'
    
    def test_database_credentials_example(self):
        """Test database credentials example."""
        # Create valid record with database credentials (from json-to-netencode)
        record = '{57:<8:database|t5:myapp,<4:host|t9:localhost,<4:port|i:5432,}'
        
        # Test that variables are set correctly
        result = run_tool('netencode-to-env', 'sh', '-c', 'echo "host=$host port=$port database=$database"', stdin=record)
        assert result.returncode == 0
        assert b'host=localhost' in result.stdout
        assert b'port=5432' in result.stdout
        assert b'database=myapp' in result.stdout
    
    def test_configuration_generation(self):
        """Test configuration generation example."""
        # Create valid record with server config (from json-to-netencode)
        record = '{36:<4:port|i:8080,<6:server|t7:example,}'
        
        # Test that variables are accessible
        result = run_tool('netencode-to-env', 'sh', '-c', 'echo "server=$server port=$port"', stdin=record)
        assert result.returncode == 0
        assert b'server=example' in result.stdout
        assert b'port=8080' in result.stdout


class TestNetencodeFilter:
    """Test examples from netencode-filter.1.scd."""
    
    def test_filter_by_boolean_field(self):
        """Test filtering by boolean field."""
        # Create test data with active and inactive users (using correct netencode format)
        active_user = '{49:<6:active|<4:true|u,<3:age|i:30,<4:name|t5:Alice,}'
        inactive_user = '{48:<6:active|<5:false|u,<3:age|i:25,<4:name|t3:Bob,}'
        input_data = active_user + '\n' + inactive_user
        
        result = run_tool('netencode-filter', 'active=true', stdin=input_data)
        assert result.returncode == 0
        assert b'Alice' in result.stdout
        assert b'Bob' not in result.stdout
    
    def test_filter_by_numeric_field(self):
        """Test filtering by numeric field."""
        # Create test data with different prices (using correct netencode format)
        product1 = '{30:<4:name|t3:foo,<5:price|i:100,}'
        product2 = '{30:<4:name|t3:bar,<5:price|i:200,}'
        input_data = product1 + '\n' + product2
        
        result = run_tool('netencode-filter', 'price=100', stdin=input_data)
        assert result.returncode == 0
        assert b'foo' in result.stdout
        assert b'bar' not in result.stdout
    
    def test_filter_by_text_field(self):
        """Test filtering by text field."""
        # Create test data with different levels (using correct netencode format)
        error_log = '{33:<5:level|t5:error,<4:text|t3:bad,}'
        info_log = '{33:<5:level|t4:info,<4:text|t4:good,}'
        input_data = error_log + '\n' + info_log
        
        result = run_tool('netencode-filter', 'level=error', stdin=input_data)
        assert result.returncode == 0
        assert b'error' in result.stdout
        assert b'info' not in result.stdout
    
    def test_filter_pipeline_chain(self):
        """Test chaining filter with other tools."""
        # Create test data (using correct netencode format)
        published = '{39:<6:status|t9:published,<5:title|t3:foo,}'
        draft = '{35:<6:status|t5:draft,<5:title|t3:bar,}'
        input_data = published + '\n' + draft
        
        # Filter and extract title
        filter_result = run_tool('netencode-filter', 'status=published', stdin=input_data)
        assert filter_result.returncode == 0
        assert b'published' in filter_result.stdout
        assert b'draft' not in filter_result.stdout
        
        # Test chaining with record-get
        title_result = run_tool('netencode-record-get', 'title', stdin=filter_result.stdout)
        assert title_result.returncode == 0
        assert b'foo' in title_result.stdout


class TestNetencodeMustache:
    """Test examples from netencode-mustache.1.scd."""
    
    def test_simple_template_rendering(self):
        """Test simple template rendering."""
        # Create valid mustache template data (matching json-to-netencode output)
        template_data = '{29:<3:age|i:30,<4:name|t5:Alice,}'
        template = 'Hello {{name}}, you are {{age}} years old!'
        
        # Set up environment with TEMPLATE_DATA
        env = os.environ.copy()
        env['TEMPLATE_DATA'] = template_data
        
        # Run mustache directly with the environment
        tool_path = get_tool_path('netencode-mustache')
        result = subprocess.run(
            [tool_path],
            input=template.encode('utf-8'),
            capture_output=True,
            env=env
        )
        
        assert result.returncode == 0
        assert result.stdout.strip() == b'Hello Alice, you are 30 years old!'
    
    def test_configuration_generation(self):
        """Test configuration generation."""
        # Create valid mustache template data for config (matching json-to-netencode)
        template_data = '{36:<4:port|i:8080,<6:server|t7:example,}'
        template = 'server={{server}}\nport={{port}}'
        
        # Set up environment with TEMPLATE_DATA
        env = os.environ.copy()
        env['TEMPLATE_DATA'] = template_data
        
        # Run mustache directly with the environment
        tool_path = get_tool_path('netencode-mustache')
        result = subprocess.run(
            [tool_path],
            input=template.encode('utf-8'),
            capture_output=True,
            env=env
        )
        
        assert result.returncode == 0
        assert b'server=example' in result.stdout
        assert b'port=8080' in result.stdout
    
    def test_list_processing(self):
        """Test list processing with mustache."""
        # Create valid mustache list data (from json-to-netencode output)
        template_data = '[27:t5:Alice,t3:Bob,t7:Charlie,]'
        template = 'Users: {{#.}}{{.}} {{/.}}'
        
        # Set up environment with TEMPLATE_DATA
        env = os.environ.copy()
        env['TEMPLATE_DATA'] = template_data
        
        # Run mustache directly with the environment
        tool_path = get_tool_path('netencode-mustache')
        result = subprocess.run(
            [tool_path],
            input=template.encode('utf-8'),
            capture_output=True,
            env=env
        )
        
        assert result.returncode == 0
        assert b'Users: Alice Bob Charlie' in result.stdout


class TestJsonToNetencode:
    """Test examples from json-to-netencode.1.scd."""
    
    def test_convert_simple_json_object(self):
        """Test converting simple JSON object."""
        # From man page: {"name": "Alice", "age": 30}
        json_data = '{"name": "Alice", "age": 30}'
        
        result = run_tool('json-to-netencode', stdin=json_data)
        assert result.returncode == 0
        # Should be a record containing Alice and age 30
        assert b'Alice' in result.stdout
        assert b'i:30' in result.stdout  # JSON numbers become integers
        assert result.stdout.startswith(b'{')
    
    def test_convert_json_array(self):
        """Test converting JSON array."""
        # From man page: ["hello", "world", 42]
        json_data = '["hello", "world", 42]'
        
        result = run_tool('json-to-netencode', stdin=json_data)
        assert result.returncode == 0
        # Should be a list containing the elements
        assert b'hello' in result.stdout
        assert b'world' in result.stdout
        assert b'i:42' in result.stdout  # JSON numbers become integers
        assert result.stdout.startswith(b'[')
    
    def test_convert_nested_json(self):
        """Test converting nested JSON structures."""
        # From man page: {"nested": {"data": [1, 2, 3]}}
        json_data = '{"nested": {"data": [1, 2, 3]}}'
        
        result = run_tool('json-to-netencode', stdin=json_data)
        assert result.returncode == 0
        # Should contain nested structure
        assert b'nested' in result.stdout
        assert b'data' in result.stdout
        assert result.stdout.startswith(b'{')
    
    def test_json_chain_with_filtering(self):
        """Test chaining JSON conversion with filtering."""
        # From man page concept: API data converted and filtered
        json_data = '{"name": "Alice", "active": true}'
        
        # Convert to netencode
        convert_result = run_tool('json-to-netencode', stdin=json_data)
        assert convert_result.returncode == 0
        
        # Filter by active=true
        filter_result = run_tool('netencode-filter', 'active=true', stdin=convert_result.stdout.strip())
        assert filter_result.returncode == 0
        assert b'Alice' in filter_result.stdout


class TestEnvToNetencode:
    """Test examples from env-to-netencode.1.scd."""
    
    def test_capture_environment(self):
        """Test capturing environment variables."""
        # Set some test environment variables
        env = {'TEST_VAR': 'test_value', 'ANOTHER_VAR': 'another_value'}
        
        tool_path = get_tool_path('env-to-netencode')
        result = subprocess.run(
            [tool_path],
            capture_output=True,
            env=env
        )
        
        assert result.returncode == 0
        # Should be a record containing our test variables
        assert result.stdout.startswith(b'{')
        assert b'TEST_VAR' in result.stdout
        assert b'test_value' in result.stdout
        assert b'ANOTHER_VAR' in result.stdout
        assert b'another_value' in result.stdout
    
    def test_extract_specific_variable(self):
        """Test extracting specific environment variables."""
        # Set test environment
        env = {'HOME': '/home/testuser', 'PATH': '/usr/bin:/bin'}
        
        tool_path = get_tool_path('env-to-netencode')
        result = subprocess.run(
            [tool_path],
            capture_output=True,
            env=env
        )
        
        assert result.returncode == 0
        
        # Extract HOME variable
        home_result = run_tool('netencode-record-get', 'HOME', stdin=result.stdout.strip())
        plain_result = run_tool('netencode-plain', stdin=home_result.stdout)
        
        assert plain_result.returncode == 0
        assert plain_result.stdout == b'/home/testuser'
    
    def test_chain_with_to_env(self):
        """Test chaining env-to-netencode with netencode-to-env."""
        # Set test environment
        env = {'TEST_USER': 'alice', 'TEST_HOME': '/home/alice'}
        
        tool_path = get_tool_path('env-to-netencode')
        env_result = subprocess.run(
            [tool_path],
            capture_output=True,
            env=env
        )
        
        assert env_result.returncode == 0
        
        # Use with netencode-to-env
        result = run_tool('netencode-to-env', 'sh', '-c', 'echo "User $TEST_USER in $TEST_HOME"', stdin=env_result.stdout.strip())
        assert result.returncode == 0
        assert b'User alice in /home/alice' in result.stdout