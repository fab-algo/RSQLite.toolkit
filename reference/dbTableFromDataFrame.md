# Create a table in a SQLite database from a data frame

The `dbTableFromDataFrame()` function reads the data from a rectangula
region of a sheet in an Excel file and copies it to a table in a SQLite
database. If table does not exist, it will create it.

## Usage

``` r
dbTableFromDataFrame(
  df,
  dbcon,
  table_name,
  id_quote_method = "DB_NAMES",
  col_names = NULL,
  col_types = NULL,
  drop_table = FALSE,
  auto_pk = FALSE,
  build_pk = FALSE,
  pk_fields = NULL
)
```

## Arguments

- df:

  the data frame to be saved in the SQLite table.

- dbcon:

  database connection, as created by the dbConnect function.

- table_name:

  character, the name of the table.

- id_quote_method:

  character, used to specify how to build the SQLite columns' names
  using the fields' identifiers read from the input file. For details
  see the description of the `quote_method` parameter of the
  [`format_column_names()`](https://fab-algo.github.io/RSQLite.toolkit/reference/format_column_names.md)
  function. Defautls to `DB_NAMES`.

- col_names:

  character vector, names of the columuns to be imported. Used to
  override the field names derived from the data frame (using the quote
  method selected by `id_quote_method`). Must be of the same length of
  the number of columns in the data frame. If `NULL` the column names
  coming from the input (after quoting) will be used. Defaults to
  `NULL`.

- col_types:

  character vector of classes to be assumed for the columns. If not
  null, it will override the data types inferred from the input data
  frame. Must be of the same length of the number of columns in the
  input. If `NULL` the data type inferred from the input will be used.
  Defaults to `NULL`.

- drop_table:

  logical, if `TRUE` the target table will be dropped (if exists) and
  recreated before importing the data. if `FALSE`, data from input data
  frame will be appended to an existing table. Defaults to `FALSE`.

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
  `UNIQUE INDEX`. Defaults to `NULL`.

## Value

integer, the number of records in `table_name` after reading data from
the data frame.

## Examples

``` r
# Create a temporary database and load data frame
# Set up database connection
dbcon <- dbConnect(RSQLite::SQLite(), file.path(tempdir(), "example.sqlite"))

# Create a sample data frame
sample_data <- data.frame(
  id = 1:10,
  name = paste0("Item_", 1:10),
  value = runif(10, 1, 100),
  active = c(TRUE, FALSE),
  date = Sys.Date() + 0:9,
  stringsAsFactors = FALSE,
  row.names = NULL
)

# Load data frame with automatic primary key
dbTableFromDataFrame(
  df = sample_data,
  dbcon = dbcon,
  table_name = "SAMPLE_DATA",
  drop_table = TRUE,
  auto_pk = TRUE  
)
#> [1] 10

# Check the imported data
dbListFields(dbcon, "SAMPLE_DATA")
#> [1] "id"      "name"    "F_value" "active"  "F_date"  "SEQ"    
dbGetQuery(dbcon, "SELECT * FROM SAMPLE_DATA LIMIT 5")
#>   id   name  F_value active F_date SEQ
#> 1  1 Item_1 20.27480      1  20512   1
#> 2  2 Item_2 17.29236      0  20513   2
#> 3  3 Item_3 66.65745      1  20514   3
#> 4  4 Item_4 85.80093      0  20515   4
#> 5  5 Item_5 92.72810      1  20516   5

# Load with column selection and custom naming
dbTableFromDataFrame(
  df = sample_data,
  dbcon = dbcon,
  table_name = "SAMPLE_SUBSET",
  drop_table = TRUE,
  col_names = c("ID", "ITEM_NAME", "ITEM_VALUE", "IS_ACTIVE", "DATE_CREATED")
)
#> [1] 10

dbGetQuery(dbcon, "SELECT * FROM SAMPLE_SUBSET LIMIT 5")
#>   ID ITEM_NAME ITEM_VALUE IS_ACTIVE DATE_CREATED
#> 1  1    Item_1   20.27480         1        20512
#> 2  2    Item_2   17.29236         0        20513
#> 3  3    Item_3   66.65745         1        20514
#> 4  4    Item_4   85.80093         0        20515
#> 5  5    Item_5   92.72810         1        20516

# Clean up
dbDisconnect(dbcon)
```
