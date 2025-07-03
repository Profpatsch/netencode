json-to-netencode(1) "netencode manual"

# NAME

json-to-netencode - Convert JSON data to netencode format

# SYNOPSIS

*json-to-netencode*

# DESCRIPTION

*json-to-netencode* reads JSON data from standard input and converts it to 
netencode format. This tool enables integration with existing JSON APIs and 
data sources, allowing them to be processed with netencode pipeline tools.

The conversion preserves data types and structure while adapting to netencode's 
explicit type system.

# OPTIONS

*json-to-netencode* takes no command-line arguments.

# INPUT

*json-to-netencode* reads a single JSON value from standard input. The input 
must be valid JSON.

# OUTPUT

The tool outputs equivalent netencode data to standard output.

# TYPE MAPPING

JSON types are converted to netencode as follows:

*null*
	Becomes unit value: *u,*

*true*
	Becomes tagged unit: *<4:true|u,*

*false*  
	Becomes tagged unit: *<5:false|u,*

*numbers*
	Integers become natural (*n:*) or signed integer (*i:*) types
	Floating-point numbers become text strings

*strings*
	Become text strings: *t[len]:[text],*

*arrays*
	Become lists: *[[len]:[item1][item2]...]*

*objects*
	Become records: *{[len]:<key1>|[value1],<key2>|[value2]...}*

# EXIT STATUS

*0*
	Success

*1*
	Error: invalid JSON input or I/O error

# EXAMPLES

Convert a simple JSON object:

```
echo '{"name": "Alice", "age": 30}' | json-to-netencode
# Output: {33:<4:name|t5:Alice,<3:age|n:30,}
```

Convert JSON array:

```
echo '["hello", "world", 42]' | json-to-netencode  
# Output: [21:t5:hello,t5:world,n:42,]
```

Process API response:

```
curl -s https://api.example.com/users | json-to-netencode | netencode-record-get name
```

Convert and format for debugging:

```
echo '{"nested": {"data": [1, 2, 3]}}' | json-to-netencode | netencode-pretty
```

Chain with filtering:

```
curl -s api/users.json | json-to-netencode | netencode-filter active=true
```

# NOTES

- Floating-point numbers are converted to text to avoid precision loss
- Object key order is not preserved (netencode records are unordered)
- Large numbers may lose precision if they exceed 64-bit integer range
- JSON nested structures are preserved in netencode format

# SEE ALSO

*netencode*(5), *netencode-pretty*(1), *netencode-record-get*(1), *netencode-filter*(1), *netencode-plain*(1), *netencode-mustache*(1), *env-to-netencode*(1)