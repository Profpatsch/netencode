env-splice-record(1) "netencode manual"

# NAME

env-splice-record - Convert environment variables to a netencode record

# SYNOPSIS

*env-splice-record*

# DESCRIPTION

*env-splice-record* reads the current environment variables and converts them 
to a netencode record, outputting the result to standard output. Each environment 
variable becomes a field in the record, with the variable name as the field name 
and the value as a text string.

This tool is useful for capturing the current environment state in structured 
form for processing by other netencode tools or for debugging environment setup.

# OPTIONS

*env-splice-record* takes no command-line arguments.

# INPUT

*env-splice-record* reads environment variables from the current process 
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
env-splice-record
# Output: {200:<4:HOME|t10:/home/user,<4:PATH|t50:/usr/bin:/bin,...}
```

Use with record-get to extract specific variables:

```
env-splice-record | record-get HOME | netencode-plain
# Output: /home/user
```

Filter environment variables:

```
env-splice-record | netencode-pretty | grep USER
```

Chain with other tools:

```
env-splice-record | record-splice-env echo "User $USER in $HOME"
```

Serialize environment for later use:

```
env-splice-record > environment.netencode
```

# NOTES

- Only UTF-8 environment variables are included
- Variable names and values are stored as text strings
- The output order is not guaranteed to be consistent
- Binary environment variable values are not supported

# SEE ALSO

*netencode*(5), *record-splice-env*(1), *record-get*(1), *netencode-pretty*(1)