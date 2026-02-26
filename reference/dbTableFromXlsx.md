# Create a table in a SQLite database from an Excel worksheet

The `dbTableFromXlsx()` function creates a table in a SQLite database
from a range of an Excel worksheet.

The `dbTableFromXlsx()` function reads the data from a range of an Excel
worksheet. If table does not exist, it will create it.

## Usage

``` r
dbTableFromXlsx(
  input_file,
  dbcon,
  table_name,
  sheet_name,
  first_row,
  cols_range,
  header = TRUE,
  id_quote_method = "DB_NAMES",
  col_names = NULL,
  col_types = NULL,
  col_import = NULL,
  drop_table = FALSE,
  auto_pk = FALSE,
  build_pk = FALSE,
  pk_fields = NULL,
  constant_values = NULL,
  ...
)
```

## Arguments

- input_file:

  character, the file name (including path) to be read.

- dbcon:

  database connection, as created by the dbConnect function.

- table_name:

  character, the name of the table.

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
  function. Defautls to `DB_NAMES`.

- col_names:

  character vector, names of the columuns in the input file. Used to
  override the field names derived from the input file (using the quote
  method selected by `id_quote_method`). Must be of the same length of
  the number of columns in the input file. If `NULL` the column names
  coming from the input file (after quoting) will be used. Defaults to
  `NULL`.

- col_types:

  character vector of classes to be assumed for the columns of the input
  file. Must be of the same length of the number of columns in the input
  file. If not null, it will override the data types guessed from the
  input file. If `NULL` the data type inferred from the input files will
  be used. Defaults to `NULL`.

- col_import:

  can be either:

  - a numeric vector (coherced to integers) with the columns' positions
    in the input file that will be imported in the SQLite table;

  - a character vector with the columns' names to be imported. The names
    are those in the input file (after quoting with `id_quote_method`),
    if `col_names` is NULL, or those expressed in `col_names` vector.
    Defaults to NULL, i.e. all columns will be imported.

- drop_table:

  logical, if `TRUE` the target table will be dropped (if exists) and
  recreated before importing the data. if `FALSE`, data from input file
  will be appended to an existing table. Defaults to `FALSE`.

- auto_pk:

  logical, if `TRUE`, and `pk_fields` parameter is `NULL`, an additional
  column named `SEQ` will be added to the table and it will be defined
  to be `INTEGER PRIMARY KEY` (i.e. in effect an alias for `ROWID`).
  Defaults to `FALSE`.

- build_pk:

  logical, if `TRUE` creates a `UNIQUE INDEX` named `<table_name>_PK`
  defined by the combination of fields specified in the `pk_fields`
  parameter. It will be effective only if `pk_fields` is not null.
  Defaults to `FALSE`.

- pk_fields:

  character vector, the list of the fields' names that define the
  `UNIQUE INDEX`. Defults to `NULL`.

- constant_values:

  a one row data frame whose columns will be added to the table in the
  database. The additional table columns will be named as the data frame
  columns, and the corresponding values will be associeted to each
  record imported from the input file. It is useful to keep track of
  additional information (e.g., the input file name, additional context
  data not available in the data set, ...) when loading the content of
  multiple input files in the same table. Defults to `NULL`.

- ...:

  additional arguments passed to
  [`openxlsx2::wb_to_df()`](https://janmarvin.github.io/openxlsx2/reference/wb_to_df.html)
  function used to read input data.

## Value

integer, the number of records in `table_name` after reading data from
`input_file`.

## Examples

``` r
# Create a temporary database and load Excel data
library(RSQLite.toolkit)

# Set up database connection
dbcon <- dbConnect(RSQLite::SQLite(), file.path(tempdir(), "example.sqlite"))

# Get path to example data
data_path <- system.file("extdata", package = "RSQLite.toolkit")

# Check if Excel file exists (may not be available in all installations)
xlsx_file <- file.path(data_path, "stock_portfolio.xlsx")

fschema <- file_schema_xlsx(xlsx_file, sheet_name="all period", 
                            first_row=2, cols_range="A:S", header=TRUE,
                            id_quote_method="DB_NAMES", max_lines=10)

fschema[, c("col_names", "src_names")]
#>                                col_names
#> 1                                     ID
#> 2                              Large_B_P
#> 3                              Large_ROE
#> 4                              Large_S_P
#> 5  Large_Return_Rate_in_the_last_quarter
#> 6                     Large_Market_Value
#> 7                  Small_systematic_Risk
#> 8                        Annual_Return_1
#> 9                        Excess_Return_2
#> 10                     Systematic_Risk_3
#> 11                          Total_Risk_4
#> 12                        Abs_Win_Rate_5
#> 13                        Rel_Win_Rate_6
#> 14                       Annual_Return_7
#> 15                       Excess_Return_8
#> 16                     Systematic_Risk_9
#> 17                         Total_Risk_10
#> 18                       Abs_Win_Rate_11
#> 19                       Rel_Win_Rate_12
#>                                  src_names
#> 1                                       ID
#> 2                               Large B/P 
#> 3                               Large ROE 
#> 4                               Large S/P 
#> 5   Large Return Rate in the last quarter 
#> 6                      Large Market Value 
#> 7                    Small systematic Risk
#> 8                            Annual Return
#> 9                            Excess Return
#> 10                         Systematic Risk
#> 11                              Total Risk
#> 12                           Abs. Win Rate
#> 13                           Rel. Win Rate
#> 14                           Annual Return
#> 15                           Excess Return
#> 16                         Systematic Risk
#> 17                              Total Risk
#> 18                           Abs. Win Rate
#> 19                           Rel. Win Rate

# Load Excel data from specific sheet and range
dbTableFromXlsx(
  input_file = xlsx_file,
  dbcon = dbcon,
  table_name = "PORTFOLIO_PERF",
  sheet_name = "all period",
  first_row = 2,
  cols_range = "A:S",
  drop_table = TRUE,
  col_import = c("ID", "Large_B_P", "Large_ROE", "Large_S_P",
                 "Annual_Return_7", "Excess_Return_8", "Systematic_Risk_9")
)
#> [1] 63

# Check the imported data
dbListFields(dbcon, "PORTFOLIO_PERF")
#> [1] "ID"                "Large_B_P"         "Large_ROE"        
#> [4] "Large_S_P"         "Annual_Return_7"   "Excess_Return_8"  
#> [7] "Systematic_Risk_9"
head(dbGetQuery(dbcon, "SELECT * FROM PORTFOLIO_PERF"))
#>   ID Large_B_P Large_ROE Large_S_P Annual_Return_7 Excess_Return_8
#> 1  1         1         0         0       0.5318748       0.4781162
#> 2  2         0         1         0       0.5497116       0.4875952
#> 3  3         0         0         1       0.6926254       0.6298950
#> 4  4         0         0         0       0.3243514       0.2556341
#> 5  5         0         0         0       0.3266149       0.3065006
#> 6  6         0         0         0       0.2000000       0.2000000
#>   Systematic_Risk_9
#> 1         0.7380152
#> 2         0.5715793
#> 3         0.7030514
#> 4         0.8000000
#> 5         0.4324519
#> 6         0.4908823

# Clean up
dbDisconnect(dbcon)
```
