# Preview the structure of a range of an Excel worksheet.

The `file_schema_xlsx()` function returns a data frame with the schema
of an Excel data table. It will read only a range of the specified
worksheet to infer column names and data types. Then it converts them to
the candidate data frame columns' names and data types.

## Usage

``` r
file_schema_xlsx(
  input_file,
  sheet_name,
  first_row,
  cols_range,
  header = TRUE,
  id_quote_method = "DB_NAMES",
  max_lines = 100,
  null_columns = FALSE,
  ...
)
```

## Arguments

- input_file:

  character, file name (including path) to be read.

- sheet_name:

  character, the name of the worksheet containing the data table.

- first_row:

  integer, the row number where the data table starts. If present, it is
  the row number of the header row, otherwise it is the row number of
  the first row of data.

- cols_range:

  integer, a numeric vector specifying which columns in the worksheet to
  be read.

- header:

  logical, if `TRUE` the first row contains the fields' names. If
  `FALSE`, the column names will be the column names of the Excel
  worksheet (i.e. letters).

- id_quote_method:

  character, used to specify how to build the SQLite columns' names
  using the fields' identifiers read from the input file. For details
  see the description of the `quote_method` parameter of the
  [`format_column_names()`](https://fab-algo.github.io/RSQLite.toolkit/reference/format_column_names.md)
  function. Defaults to `DB_NAMES`.

- max_lines:

  integer, number of lines (excluding the header) to be read to infer
  columns' data types. Defaults to 100.

- null_columns:

  logical, if `TRUE` the col_type of columuns consisting only of NAs or
  zero-length strings will be marked as `NA`, otherwise they will be
  marked as `character`. Defaults to `FALSE`

- ...:

  Additional parameters passed to
  [`openxlsx2::wb_to_df()`](https://janmarvin.github.io/openxlsx2/reference/wb_to_df.html)
  function.

## Value

a data frame with these columns:

- `col_names`: columns' names, after applying the selected quote method;

- `col_names_unquoted`: columns' names, unquoted; if `id_quote_method`
  is set to `DB_NAMES` they will be the same as `col_names`; for other
  quote methods they will be the unquoted versions of `col_names`,that
  is generally the same as `src_names` unless `src_names` contain the
  quoting characters;

- `col_types`: columns' R data types;

- `sql_types`: columns' SQLite data types;

- `src_names`: columns' names as they appear in the input file;

- `src_types`: data type attribute of each column, as determined by the
  [`openxlsx2::wb_to_df()`](https://janmarvin.github.io/openxlsx2/reference/wb_to_df.html)
  function.

## Examples

``` r
# Inspect xlsx file schema
data_path <- system.file("extdata", package = "RSQLite.toolkit")

# Get schema information for Excel file
schema_info <- file_schema_xlsx(
  input_file = file.path(data_path, "stock_portfolio.xlsx"),
  sheet_name = "all period",
  first_row = 2,
  cols_range = "A:S",
  header = TRUE,
  id_quote_method = "DB_NAMES",
  max_lines = 10
)

# Display schema information
head(schema_info[, c("col_names", "src_names")])
#>                               col_names                               src_names
#> 1                                    ID                                      ID
#> 2                             Large_B_P                              Large B/P 
#> 3                             Large_ROE                              Large ROE 
#> 4                             Large_S_P                              Large S/P 
#> 5 Large_Return_Rate_in_the_last_quarter  Large Return Rate in the last quarter 
#> 6                    Large_Market_Value                     Large Market Value 

# Check specific columns
print(paste("Number of columns:", nrow(schema_info)))
#> [1] "Number of columns: 19"
```
