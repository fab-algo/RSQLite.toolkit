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
