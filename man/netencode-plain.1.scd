netencode-plain(1) "netencode manual"

# NAME

netencode-plain - Convert netencode scalar values to plain text

# SYNOPSIS

*netencode-plain*

# DESCRIPTION

*netencode-plain* reads a netencode value from standard input and outputs its 
contents as plain text without netencode formatting. This tool is essential 
for converting netencode data to formats usable by traditional Unix tools that 
expect plain text.

Only scalar values (text, numbers, booleans, binary data, unit) are converted. 
Complex structures (records, lists) pass through as netencode.

# OPTIONS

*netencode-plain* takes no command-line arguments.

# INPUT

*netencode-plain* reads a single netencode value from standard input.

# OUTPUT

The output depends on the input type:

*Text values*
	Raw UTF-8 text content without length prefix or quotes

*Natural numbers*
	Decimal representation (e.g., *n:42,* becomes *42*)

*Signed integers*
	Decimal representation with sign (e.g., *i:-42,* becomes *-42*)

*Binary data*
	Raw bytes output directly

*Booleans*
	Text "true" or "false"

*Unit values*
	Empty output (nothing)

*Records/Lists*
	Passed through unchanged as netencode

*Other tagged values*
	Passed through unchanged as netencode

# EXIT STATUS

*0*
	Success

*1*
	Error: invalid netencode input or I/O error

# EXAMPLES

Convert text to plain output:

```
echo 't5:hello,' | netencode-plain
# Output: hello
```

Convert numbers:

```
echo 'i:42,' | netencode-plain
# Output: 42
echo 'i:-100,' | netencode-plain
# Output: -100
```

Convert booleans:

```
echo '<4:true|u,' | netencode-plain
# Output: true
echo '<5:false|u,' | netencode-plain
# Output: false
```

Use in pipelines to extract field values:

```
echo '{29:<3:age|i:30,<4:name|t5:Alice,}' | netencode-record-get name | netencode-plain
# Output: Alice
```

Process lists of values:

```
cat users.netencode | netencode-record-get email | netencode-plain > emails.txt
```

Chain with shell tools:

```
cat config.netencode | netencode-record-get port | netencode-plain | xargs netstat -ln | grep :
```

# NOTES

- Binary data is output as raw bytes, which may not be safe for terminal display
- Complex structures are not converted and remain in netencode format
- Unit values produce no output
- This tool eliminates the need for sed/awk patterns to extract simple values

# SEE ALSO

*netencode*(5), *netencode-record-get*(1), *netencode-pretty*(1), *netencode-filter*(1), *netencode-mustache*(1), *json-to-netencode*(1), *env-to-netencode*(1)