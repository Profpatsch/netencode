env-to-netencode(1) "netencode manual"

# NAME

env-to-netencode - Convert environment variables to a netencode record

# SYNOPSIS

*env-to-netencode*

# DESCRIPTION

*env-to-netencode* reads the current environment variables and converts them 
to a netencode record, outputting the result to standard output. Each environment 
variable becomes a field in the record, with the variable name as the field name 
and the value as a text string.

This tool is useful for capturing the current environment state in structured 
form for processing by other netencode tools or for debugging environment setup.

# OPTIONS

*env-to-netencode* takes no command-line arguments.

# INPUT

*env-to-netencode* reads environment variables from the current process 
environment. No standard input is processed.

# OUTPUT

A netencode record containing all environment variables as text fields. The 
output format is:

```
{<size>:<name1>|t<len1>:<value1>,<name2>|t<len2>:<value2>,...}
```

# ENVIRONMENT

All environment variables are processed. Variables with non-UTF-8 names or 
values are ignored for safety.

# EXIT STATUS

*0*
	Success

*1*
	Error writing output

# EXAMPLES

Capture current environment:

```
env-to-netencode
# Output: {200:<4:HOME|t10:/home/user,<4:PATH|t50:/usr/bin:/bin,...}
```

Use with record-get to extract specific variables:

```
env-to-netencode | netencode-record-get HOME | netencode-plain
# Output: /home/user
```

Filter environment variables:

```
env-to-netencode | netencode-pretty | grep USER
```

Chain with other tools:

```
env-to-netencode | netencode-to-env echo "User $USER in $HOME"
```

Serialize environment for later use:

```
env-to-netencode > environment.netencode
```

# NOTES

- Only UTF-8 environment variables are included
- Variable names and values are stored as text strings
- The output order is not guaranteed to be consistent
- Binary environment variable values are not supported

# SEE ALSO

*netencode*(5), *netencode-to-env*(1), *netencode-record-get*(1), *netencode-pretty*(1), *netencode-filter*(1), *json-to-netencode*(1)