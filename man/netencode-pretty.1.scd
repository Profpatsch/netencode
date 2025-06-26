netencode-pretty(1) "netencode manual"

# NAME

netencode-pretty - Format netencode data for human readability

# SYNOPSIS

*netencode-pretty*

# DESCRIPTION

*netencode-pretty* reads netencode data from standard input and formats it 
as human-readable text with proper indentation and line breaks for debugging 
and inspection purposes.

This tool is essential for debugging netencode data streams and understanding 
the structure of complex nested data.

# OPTIONS

*netencode-pretty* takes no command-line arguments.

# INPUT

*netencode-pretty* reads a single netencode value from standard input. The 
input must be valid netencode format.

# OUTPUT

The tool outputs human-readable formatted text (not valid netencode) with:
- Proper indentation for nested structures
- Line breaks between record fields and list elements  
- Consistent spacing for readability

# EXIT STATUS

*0*
	Success

*1*
	Error reading input or invalid netencode format

# EXAMPLES

Format a compact netencode record:

```
echo '{33:<4:name|t5:Alice,<3:age|n:30,<6:active|<4:true|u,}' | netencode-pretty
```

Format nested structures:

```
echo '[42:{12:<4:name|t3:foo,}{12:<4:name|t3:bar,}]' | netencode-pretty
```

Use to inspect data during pipeline development:

```
some-data-source | tee >(netencode-pretty >&2) | next-step
```

# SEE ALSO

*netencode*(5), *record-get*(1), *json-to-netencode*(1)