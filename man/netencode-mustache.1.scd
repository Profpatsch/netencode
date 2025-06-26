netencode-mustache(1) "netencode manual"

# NAME

netencode-mustache - Render Mustache templates with netencode data

# SYNOPSIS

*netencode-mustache*

# DESCRIPTION

*netencode-mustache* renders Mustache templates using netencode data as the 
template context. The tool reads a Mustache template from standard input and 
uses netencode data from an environment variable to populate the template.

This enables generating configuration files, documentation, or any text output 
from structured netencode data using the Mustache templating language.

# OPTIONS

*netencode-mustache* takes no command-line arguments.

# INPUT

*Template*
	Mustache template read from standard input

*Data*
	Netencode data read from the *TEMPLATE_DATA* environment variable

# OUTPUT

The rendered template is written to standard output.

# ENVIRONMENT

*TEMPLATE_DATA*
	Environment variable containing netencode data to use as template context. 
	This variable must be set and contain valid netencode.

# DATA MAPPING

Netencode values are converted to Mustache data types:

*Text values*
	Become Mustache strings

*Numbers*
	Become Mustache strings (decimal representation)

*Booleans*
	Become Mustache booleans (true/false)

*Unit values*
	Become Mustache null

*Records*
	Become Mustache objects (hash maps)

*Lists*
	Become Mustache arrays

*Tagged values*
	Currently not supported (will cause error)

*Binary data*
	Currently not supported (will cause error)

# EXIT STATUS

*0*
	Success

*1*
	Error: invalid template, missing environment variable, or invalid netencode data

# EXAMPLES

Render a simple template:

```
export TEMPLATE_DATA='{25:<4:name|t5:Alice,<3:age|n:30,}'
echo "Hello {{name}}, you are {{age}} years old!" | netencode-mustache
# Output: Hello Alice, you are 30 years old!
```

Generate configuration files:

```
export TEMPLATE_DATA='{30:<6:server|t7:example,<4:port|n:8080,}'
cat config.template | netencode-mustache > config.conf
```

Use with record data:

```
env-splice-record | record-get DATABASE_CONFIG | \
  env TEMPLATE_DATA="$(cat)" netencode-mustache < db.conf.template
```

Process lists:

```
export TEMPLATE_DATA='[21:t5:Alice,t3:Bob,t7:Charlie,]'
echo "Users: {{#.}}{{.}} {{/.}}" | netencode-mustache
# Output: Users: Alice Bob Charlie
```

Chain with other tools:

```
json-to-netencode < data.json | \
  env TEMPLATE_DATA="$(cat)" netencode-mustache < report.template
```

# TEMPLATE SYNTAX

*netencode-mustache* supports standard Mustache template syntax:

- *{{variable}}* - Variable substitution
- *{{#section}}...{{/section}}* - Sections (conditionals/loops)  
- *{{^section}}...{{/section}}* - Inverted sections
- *{{!comment}}* - Comments
- *{{>partial}}* - Partials (not recommended for simple use cases)

# NOTES

- The *TEMPLATE_DATA* environment variable must contain valid netencode
- Complex netencode structures (tagged values, binary data) are not supported
- Template syntax errors will cause the program to exit with error
- Use *env* command to set *TEMPLATE_DATA* in pipelines

# SEE ALSO

*netencode*(5), *env-splice-record*(1), *record-get*(1), *json-to-netencode*(1)

Mustache specification: https://mustache.github.io/