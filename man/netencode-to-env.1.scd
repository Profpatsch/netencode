netencode-to-env(1) "netencode manual"

# NAME

netencode-to-env - Execute command with record fields as environment variables

# SYNOPSIS

*netencode-to-env* _command_ [_arguments_...]

# DESCRIPTION

*netencode-to-env* reads a netencode record from standard input and executes 
the specified command with the record's fields set as environment variables. 
Only scalar values (text, numbers, unit) are converted to environment variables; 
complex values like nested records or lists are ignored.

This tool enables using structured netencode data to parameterize shell commands 
and scripts in a type-safe way.

# OPTIONS

_command_
	The command to execute

_arguments_
	Optional arguments to pass to the command

# INPUT

*netencode-to-env* reads a single netencode record from standard input. Only 
scalar fields that can be represented as byte strings are used as environment 
variables.

# OUTPUT

The tool executes the specified command and replaces the current process. The 
command's output goes directly to the terminal.

# ENVIRONMENT

The record fields are made available as environment variables:
- Text fields: Set to their string value
- Number fields: Set to their decimal representation  
- Boolean fields: Set to "true" or "false"
- Complex fields (records, lists): Ignored

Field names become environment variable names directly.

# EXIT STATUS

The exit status is that of the executed command.

# EXAMPLES

Execute a command with user data:

```
echo '{49:<6:active|<4:true|u,<3:age|i:30,<4:name|t5:Alice,}' | 
  netencode-to-env echo "Hello $name, you are $age years old"
# Output: Hello Alice, you are 30 years old
```

Run a script with database credentials:

```
echo '{45:<4:host|t9:localhost,<4:port|i:5432,<8:database|t6:myapp,}' |
  netencode-to-env psql -h "$host" -p "$port" -d "$database"
```

Generate configuration files:

```
echo '{30:<6:server|t7:example,<4:port|i:8080,}' |
  netencode-to-env envsubst < config.template > config.conf
```

Chain with other netencode tools:

```
json-to-netencode < config.json |
  netencode-to-env ./deploy-script.sh
```

# ERRORS

The tool will exit with an error if:
- The input is not a valid netencode record
- The command cannot be executed
- Field names contain null bytes (invalid for environment variables)

# NOTES

Non-UTF-8 field names and values are skipped and not set as environment variables for safety. Environment variable names must be valid shell identifiers.

# SEE ALSO

*netencode*(5), *netencode-record-get*(1), *env-to-netencode*(1), *json-to-netencode*(1), *netencode-filter*(1), *netencode-mustache*(1)