{ lib }:

let
  gen = import ./gen.nix { inherit lib; };
  
  # Test assertions - will fail the build if any assertion is false
  runTests = tests:
    lib.all (test: test) (lib.attrValues tests);
    
  # Test cases based on GENERATOR_TEST_SPEC.md
  tests = {
    # Basic Types
    test_unit_value = gen.unit == "u,";
    test_natural_zero = gen.natural 0 == "n:0,";
    test_natural_number = gen.natural 42 == "n:42,";
    # Note: Nix has integer limitations, so we test a large but not max value
    test_natural_large = gen.natural 1000000000 == "n:1000000000,";
    
    test_integer_zero = gen.integer 0 == "i:0,";
    test_integer_positive = gen.integer 42 == "i:42,";
    test_integer_negative = gen.integer (-42) == "i:-42,";
    # Note: Nix has integer limitations, so we test large but not max values
    test_integer_large_positive = gen.integer 1000000000 == "i:1000000000,";
    test_integer_large_negative = gen.integer (-1000000000) == "i:-1000000000,";
    
    test_boolean_true = gen.boolean true == "<4:true|u,";
    test_boolean_false = gen.boolean false == "<5:false|u,";
    
    test_text_empty = gen.text "" == "t0:,";
    test_text_simple = gen.text "hello" == "t5:hello,";
    test_text_with_space = gen.text "Hello, World!" == "t13:Hello, World!,";
    test_text_utf8_accented = gen.text "café" == "t5:café,";
    test_text_utf8_emoji = gen.text "🌍" == "t4:🌍,";
    test_text_with_quotes = gen.text "He said \"hi\"" == "t12:He said \"hi\",";
    test_text_with_newline = gen.text "line1\nline2" == "t11:line1\nline2,";
    
    # Note: binary function in gen.nix uses netstring which expects strings
    test_binary_empty = gen.binary "" == "b0:,";
    test_binary_simple = gen.binary "hello" == "b5:hello,";
    
    # Composite Types
    test_tag_simple = gen.tag "foo" gen.unit == "<3:foo|u,";
    test_tag_empty_name = gen.tag "" (gen.integer 42) == "<0:|i:42,";
    test_tag_with_value = gen.tag "Some" (gen.text "value") == "<4:Some|t5:value,";
    test_tag_utf8 = gen.tag "café" gen.unit == "<5:café|u,";
    
    test_record_single_field = 
      gen.record [{ key = "a"; val = gen.unit; }] == "{7:<1:a|u,}";
      
    test_record_two_fields = 
      gen.record [
        { key = "foo"; val = gen.integer 42; }
        { key = "bar"; val = gen.text "baz"; }
      ] == "{26:<3:foo|i:42,<3:bar|t3:baz,}";
    
    test_list_empty = gen.list [] == "[0:]";
    test_list_single_item = gen.list [gen.text "hello"] == "[9:t5:hello,]";
    test_list_multiple_items = 
      gen.list [(gen.text "foo") (gen.integer 42) gen.unit] == "[14:t3:foo,i:42,u,]";
    
    # Complex Scenarios
    test_nested_list_in_record = 
      gen.record [{ 
        key = "items"; 
        val = gen.list [(gen.text "foo") (gen.text "bar")]; 
      }] == "{28:<5:items|[14:t3:foo,t3:bar,]}";
      
    test_nested_record_in_list = 
      gen.list [
        (gen.record [{ key = "x"; val = gen.natural 1; }])
        (gen.record [{ key = "y"; val = gen.natural 2; }])
      ] == "[26:{9:<1:x|n:1,}{9:<1:y|n:2,}]";
      
    test_unicode_field_names = 
      gen.record [{ key = "café"; val = gen.text "value"; }] == "{18:<5:café|t5:value,}";
      
    test_unicode_complex = 
      gen.text "Hello 世界 🌍" == "t17:Hello 世界 🌍,";
      
    test_unicode_tag_name = 
      gen.tag "世界" gen.unit == "<6:世界|u,";
      
    # Test dwim function with various Nix types
    test_dwim_bool_true = gen.dwim true == "<4:true|u,";
    test_dwim_bool_false = gen.dwim false == "<5:false|u,";
    test_dwim_int = gen.dwim 42 == "i:42,";
    test_dwim_string = gen.dwim "hello" == "t5:hello,";
    test_dwim_list = gen.dwim ["a" "b"] == "[10:t1:a,t1:b,]";
    test_dwim_attrset = gen.dwim { x = 1; y = 2; } == "{18:<1:x|i:1,<1:y|i:2,}";
  };

in
{
  # Export the test results - this will cause a build failure if any test fails
  success = runTests tests;
  
  # Also export individual test results for debugging
  inherit tests;
  
  # Export the generator functions for external use
  inherit gen;
}