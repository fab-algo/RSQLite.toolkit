# From R class names to SQLite data types

The `R2SQL_types()` function returns a character vector with the names
of SQLite data types corresponding to the R classes passed through the
`x` parameter.

If any class is not recognized, it will be replaced with `TEXT` data
type.

## Usage

``` r
R2SQL_types(x)
```

## Arguments

- x:

  character, a vector containing the strings with the R class names.

## Value

a character vector with the names of SQLite data types.

## Examples

``` r
# Convert R data types to SQLite types
r_types <- c("character", "integer", "numeric", "logical", "Date")
sql_types <- R2SQL_types(r_types)

# Display the mapping
data.frame(
  R_type = r_types,
  SQLite_type = sql_types,
  row.names = NULL
)
#>      R_type SQLite_type
#> 1 character        TEXT
#> 2   integer     INTEGER
#> 3   numeric        REAL
#> 4   logical     INTEGER
#> 5      Date        DATE

# Handle unknown types (converted to TEXT)
mixed_types <- c("character", "unknown_type", "integer")
R2SQL_types(mixed_types)
#> character      <NA>   integer 
#>    "TEXT"    "TEXT" "INTEGER" 
```
