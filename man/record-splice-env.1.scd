record-splice-env(1) "netencode manual"

# NAME

record-splice-env - Execute command with record fields as environment variables

# SYNOPSIS

*record-splice-env* _command_ [_arguments_...]

# DESCRIPTION

*record-splice-env* reads a netencode record from standard input and executes 
the specified command with the record's fields set as environment variables. 
Only scalar values (text, numbers, unit) are converted to environment variables; 
complex values like nested records or lists are ignored.

This tool enables using structured netencode data to parameterize shell commands 
and scripts in a type-safe way.

# OPTIONS

*command*
	The command to execute

*arguments*
	Optional arguments to pass to the command

# INPUT

*record-splice-env* reads a single netencode record from standard input. Only 
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
echo '{33:<4:name|t5:Alice,<3:age|n:30,<6:active|<4:true|u,}' | 
  record-splice-env echo "Hello $name, you are $age years old"
# Output: Hello Alice, you are 30 years old
```

Run a script with database credentials:

```
echo '{45:<4:host|t9:localhost,<4:port|n:5432,<8:database|t6:myapp,}' |
  record-splice-env psql -h "$host" -p "$port" -d "$database"
```

Generate configuration files:

```
echo '{30:<6:server|t7:example,<4:port|n:8080,}' |
  record-splice-env envsubst < config.template > config.conf
```

Chain with other netencode tools:

```
json-to-netencode < config.json |
  record-splice-env ./deploy-script.sh
```

# ERRORS

The tool will exit with an error if:
- The input is not a valid netencode record
- The command cannot be executed
- Field names contain null bytes (invalid for environment variables)

# NOTES

Non-UTF-8 field names and values are ignored for safety. Environment variable 
names must be valid shell identifiers.

# SEE ALSO

*netencode*(5), *record-get*(1), *env-splice-record*(1), *json-to-netencode*(1)