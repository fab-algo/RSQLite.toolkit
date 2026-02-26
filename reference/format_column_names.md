# Format column names for SQLite

The `format_column_names()` function formats a vector of strings to be
used as columns' names for a table in a SQLite database.

## Usage

``` r
format_column_names(
  x,
  quote_method = "DB_NAMES",
  unique_names = TRUE,
  encoding = ""
)
```

## Arguments

- x:

  character vector with the identifiers' names to be quoted.

- quote_method:

  character, used to specify how to build the SQLite columns' names from
  the identifiers passed through the `x` parameter. Supported values for
  `quote_method`:

  - `DB_NAMES` tries to build a valid SQLite column name: a.
    substituting all characters, that are not letters or digits or the
    `_` character, with the `_` character; b. prefixing `N_` to all
    strings starting with a digit; c. prefixing `F_` to all strings
    equal to any SQL92 keyword.

  - `SINGLE_QUOTES` encloses each string in single quotes.

  - `SQL_SERVER` encloses each string in square brackets.

  - `MYSQL` encloses each string in back ticks. Defaults to `DB_NAMES`.

- unique_names:

  logical, checks for any duplicate name after applying the selected
  quote methods. If duplicates exist, they will be made unique by adding
  a postfix `_[n]`, where `n` is a progressive integer. Defaults to
  `TRUE`.

- encoding:

  character, encoding to be assumed for input strings. It is used to
  re-encode the input in order to process it to build column
  identifiers. Defaults to ‘""’ (for the encoding of the current
  locale).

## Value

A data frame containing the columns' identifiers in two formats:

- `quoted`: the quoted names, as per the selected `quote_method`;

- `unquoted`: the cleaned names, without any quoting.

## Examples

``` r
# Example with DB_NAMES method
col_names <- c("column 1", "column-2", "3rd_column", "SELECT")

formatted_names <- format_column_names(col_names, quote_method = "DB_NAMES")
print(formatted_names)
#>         quoted     unquoted
#> 1     column_1     column_1
#> 2     column_2     column_2
#> 3 N_3rd_column N_3rd_column
#> 4     F_SELECT     F_SELECT

# Example with SINGLE_QUOTES method
formatted_names_sq <- format_column_names(col_names, quote_method = "SINGLE_QUOTES")
print(formatted_names_sq)
#>         quoted   unquoted
#> 1   'column 1'   column 1
#> 2   'column-2'   column-2
#> 3 '3rd_column' 3rd_column
#> 4     'SELECT'     SELECT

# Example with SQL_SERVER method
formatted_names_sqlsrv <- format_column_names(col_names, quote_method = "SQL_SERVER")
print(formatted_names_sqlsrv)
#>         quoted   unquoted
#> 1   [column 1]   column 1
#> 2   [column-2]   column-2
#> 3 [3rd_column] 3rd_column
#> 4     [SELECT]     SELECT
```
