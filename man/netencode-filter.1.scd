netencode-filter(1) "netencode manual"

# NAME

netencode-filter - Filter netencode records by field values

# SYNOPSIS

*netencode-filter* _field_=_value_

# DESCRIPTION

*netencode-filter* reads netencode data from standard input line by line and 
outputs only records where the specified field matches the given value. This 
tool is designed for stream processing of netencode records, making it easy 
to filter large datasets.

Non-record values pass through unchanged. Invalid netencode is silently skipped.

# OPTIONS

*field=value*
	Filter expression specifying the field name and required value. The field 
	must exist in the record and match the value exactly.

# INPUT

*netencode-filter* reads netencode values from standard input, processing each 
line separately. Each line should contain a complete netencode value.

# OUTPUT

The tool outputs only those netencode records where the specified field matches 
the filter value. Non-record values are passed through unchanged.

# MATCHING RULES

Field values are matched according to their type:

*Text values*
	Exact string match

*Natural numbers*  
	Numeric equality (value is parsed as u64)

*Signed integers*
	Numeric equality (value is parsed as i64)

*Tagged values*
	Tag name exact match (for booleans: "true" or "false")

*Other types*
	Never match

# EXIT STATUS

*0*
	Success (filter completed, regardless of matches found)

*1*
	Error: invalid filter expression or I/O error

# EXAMPLES

Filter active users:

```
cat users.netencode | netencode-filter active=true
```

Filter by numeric field:

```
cat products.netencode | netencode-filter price=100
```

Filter by text field:

```
cat logs.netencode | netencode-filter level=error
```

Chain with other tools:

```
json-to-netencode < api-data.json | 
  netencode-filter status=published |
  record-get title |
  netencode-plain
```

Process streaming data with inspection:

```
tail -f application.log |
  json-to-netencode |
  netencode-filter severity=critical |
  tee >(netencode-pretty >&2)
```

# NOTES

- Records without the specified field are filtered out
- Filter expressions must be in the exact format "field=value"
- Field names are case-sensitive
- Empty input produces empty output
- Invalid netencode lines are silently ignored

# SEE ALSO

*netencode*(5), *record-get*(1), *json-to-netencode*(1), *netencode-pretty*(1)