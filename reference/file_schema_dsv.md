# Preview the table structure contained in a DSV file.

The `file_schema_dsv()` function returns a data frame with the schema of
a DSV file reading only the first `max_lines` of a delimiter separated
values (DSV) text file to infer column names and data types (it does not
read the full dataset into memory). Then it converts them to the
candidate data frame columns' names and data types.

## Usage

``` r
file_schema_dsv(
  input_file,
  header = TRUE,
  sep = ",",
  dec = ".",
  grp = "",
  id_quote_method = "DB_NAMES",
  max_lines = 2000,
  null_columns = FALSE,
  force_num_cols = TRUE,
  ...
)
```

## Arguments

- input_file:

  character, file name (including path) to be read.

- header:

  logical, if `TRUE` the first line contains the fields' names. If
  `FALSE`, the column names will be formed sing a "V" followed by the
  column number (as specified in
  [`utils::read.table()`](https://rdrr.io/r/utils/read.table.html)).

- sep:

  character, field delimiter (e.g., "," for CSV, "\t" for TSV) in the
  input file. Defaults to ",".

- dec:

  character, decimal separator (e.g., "." or "," depending on locale) in
  the input file. Defaults to ".".

- grp:

  character, character used for digit grouping. It defaults to `""`
  (i.e. no grouping).

- id_quote_method:

  character, used to specify how to build the SQLite columns' names
  using the fields' identifiers read from the input file. For details
  see the description of the `quote_method` parameter of the
  [`format_column_names()`](https://fab-algo.github.io/RSQLite.toolkit/reference/format_column_names.md)
  function. Defautls to `DB_NAMES`.

- max_lines:

  integer, number of lines (excluding the header) to be read to infer
  columns' data types. Defaults to 2000.

- null_columns:

  logical, if `TRUE` the col_type of columuns consisting only of NAs or
  zero-length strings will be marked as `"NULL"`, otherwise they will be
  marked as `character`. Defaults to `FALSE`

- force_num_cols:

  logical, if `TRUE` the returned schema will have all rows with
  `n_cols` columns (i.e. the guessed number of columns determined
  inspecting the first `max_lines` lines of the input file), even if
  there are rows in the input file with fewer or greater columns than
  `n_cols`. If `FALSE` and any of the tested lines has a number of
  columns not equal to `n_cols`, the function will return a list without
  the `schema` element. It defaults to `TRUE`.

- ...:

  Additional arguments for quoting and data interpretation as described
  in the [`base::scan()`](https://rdrr.io/r/base/scan.html) function.
  The parameters used by `file_schema_dsv` are:

  - `quote`, character, the set of quoting characters. Defaults to `""`
    (i.e., no quoting).

  - `comment.char`, character, the comment character. Defaults to `""`
    (i.e., no comments).

  - `skip`, integer, the number of lines to skip before reading data.
    Defaults to `0`.

  - `fileEncoding`, character, the name of the encoding of the input
    file. Defaults to `""`.

  - `na.strings`, character vector, the strings to be interpreted as
    NAs. Defaults to `c("NA")`.

## Value

a list with the following named elements:

- `schema`, a data frame with these columns:

  - `col_names`: columns' names, after applying the selected quote
    method;

  - `col_names_unquoted`: columns' names, unquoted; if `id_quote_method`
    is set to `DB_NAMES` they will be the same as `col_names`; for other
    quote methods they will be the unquoted versions of `col_names`,that
    is generally the same as `src_names` unless `src_names` contain the
    quoting characters;

  - `col_types`: columns' R data types;

  - `sql_types`: columns' SQLite data types;

  - `src_names`: columns' names as they appear in the input file.

  - `src_types`: defaults to `text` for all columns.

  - `src_is_quoted`: logical vector indicating if each column has at
    least one value enclosed in quotes.

- `col_counts`, a data frame with these columns:

  - `num_col`: number of columns,

  - `Freq`: number of rows (within `max_lines`) that have the number of
    colums shown in `num_col`.

- `n_cols`, integer, the number of columns selected for the file.

- `num_col`, a vector of integers of length `max_lines` with the number
  of detected columns in each row tested.

- `col_fill`, logical, it is set to `TRUE` if there are lines with less
  columns than `n_cols`.

- `col_flush`, logical, it is set to `TRUE` if there are lines with more
  columns than `n_cols`.

## Examples

``` r
# Inspect CSV file schema without loading full dataset
data_path <- system.file("extdata", package = "RSQLite.toolkit")

# Get schema information for abalone CSV
schema_info <- file_schema_dsv(
  input_file = file.path(data_path, "abalone.csv"),
  header = TRUE,
  sep = ",",
  dec = ".",
  max_lines = 50
)

# Display schema information
print(schema_info$schema[, c("col_names", "col_types", "sql_types")])
#>   col_names col_types sql_types
#> 1       Sex character      TEXT
#> 2    Length   numeric      REAL
#> 3      Diam   numeric      REAL
#> 4    Height   numeric      REAL
#> 5     Whole   numeric      REAL
#> 6   Shucked   numeric      REAL
#> 7   Viscera   numeric      REAL
#> 8     Shell   numeric      REAL
#> 9     Rings   integer   INTEGER

# Check column consistency
print(schema_info$col_counts)
#>   num_col Freq
#> 1       9   50
print(paste("Guessed columns:", schema_info$n_cols))
#> [1] "Guessed columns: 9"

# Example with different parameters
schema_custom <- file_schema_dsv(
  input_file = file.path(data_path, "abalone.csv"),
  header = TRUE,
  sep = ",",
  dec = ".",
  max_lines = 50,
  id_quote_method = "SQL_SERVER"
)

print(schema_custom$schema[, c("col_names", "col_types", "src_names")])
#>   col_names col_types src_names
#> 1     [Sex] character       Sex
#> 2  [Length]   numeric    Length
#> 3    [Diam]   numeric      Diam
#> 4  [Height]   numeric    Height
#> 5   [Whole]   numeric     Whole
#> 6 [Shucked]   numeric   Shucked
#> 7 [Viscera]   numeric   Viscera
#> 8   [Shell]   numeric     Shell
#> 9   [Rings]   integer     Rings
```
