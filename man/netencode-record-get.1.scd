netencode-record-get(1) "netencode manual"

# NAME

netencode-record-get - Extract a field from a netencode record

# SYNOPSIS

*netencode-record-get* _field_

# DESCRIPTION

*netencode-record-get* extracts a specified field from a netencode record read from 
standard input. The field value is output as netencode data, which can be 
further processed by other tools or converted to plain text.

This tool is fundamental for accessing structured data in netencode pipelines,
allowing you to extract specific values from records without manual parsing.

# OPTIONS

*field*
	The name of the field to extract from the record. Must be valid UTF-8.

# INPUT

*netencode-record-get* reads a single netencode record from standard input. The input 
must be a valid netencode record (starting with *{*).

# OUTPUT

The tool outputs the value of the specified field as netencode data. If the 
field does not exist in the record, the program exits with an error.

# EXIT STATUS

*0*
	Success - field found and extracted

*1*
	Error: invalid input, field not found, or input is not a record

# EXAMPLES

Extract the name field from a user record:

```
echo '{33:<4:name|t5:Alice,<3:age|n:30,<6:active|<4:true|u,}' | netencode-record-get name
# Output: t5:Alice,
```

Extract a numeric field:

```
echo '{33:<4:name|t5:Alice,<3:age|n:30,<6:active|<4:true|u,}' | netencode-record-get age  
# Output: n:30,
```

Chain with netencode-plain to get raw values:

```
echo '{33:<4:name|t5:Alice,<3:age|n:30,<6:active|<4:true|u,}' | netencode-record-get name | netencode-plain
# Output: Alice
```

Use in data processing pipelines:

```
cat users.netencode | netencode-record-get email | netencode-plain > email-list.txt
```

# ERRORS

The tool will exit with an error message if:
- The field name contains non-UTF-8 bytes
- The input is not a valid netencode record
- The specified field does not exist in the record

# SEE ALSO

*netencode*(5), *netencode-plain*(1), *netencode-to-env*(1), *env-to-netencode*(1)