# Create a table from a delimiter separated values (DSV) text file

The `dbTableFromDSV()` function reads the data from a DSV file and
copies it to a table in a SQLite database. If table does not exist, it
will create it.

The `dbTableFromDSV()` function reads the data from a DSV file and
copies it to a table in a SQLite database. If table does not exist, it
will create it.

## Usage

``` r
dbTableFromDSV(
  input_file,
  dbcon,
  table_name,
  header = TRUE,
  sep = ",",
  dec = ".",
  grp = "",
  id_quote_method = "DB_NAMES",
  col_names = NULL,
  col_types = NULL,
  col_import = NULL,
  drop_table = FALSE,
  auto_pk = FALSE,
  build_pk = FALSE,
  pk_fields = NULL,
  constant_values = NULL,
  chunk_size = 0,
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

- header:

  logical, if `TRUE` the first line contains the columns' names. If
  `FALSE`, the columns' names will be formed sing a "V" followed by the
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
  `UNIQUE INDEX`. Defaults to `NULL`.

- constant_values:

  a one row data frame whose columns will be added to the table in the
  database. The additional table columns will be named as the data frame
  columns, and the corresponding values will be associeted to each
  record imported from the input file. It is useful to keep track of
  additional information (e.g., the input file name, additional context
  data not available in the data set, ...) when loading the content of
  multiple input files in the same table.

- chunk_size:

  integer, the number of lines in each "chunk" (i.e. block of lines from
  the input file). Setting its value to a positive integer number, will
  process the input file by blocks of `chunk_size` lines, avoiding to
  read all the data in memory at once. It can be useful for very large
  size files. If set to zero, it will process the whole text file in one
  pass. Default to zero.

- ...:

  additional arguments passed to
  [`base::scan()`](https://rdrr.io/r/base/scan.html) function used to
  read input data. Please note that if the `quote` parameter is not
  specified, it will be set to `""` (i.e., no quoting) by default.

## Value

integer, the number of records in `table_name` after reading data from
`input_file`.

## Examples

``` r
# Create a temporary database and load CSV data
library(RSQLite.toolkit)

# Set up database connection
dbcon <- dbConnect(RSQLite::SQLite(), file.path(tempdir(), "example.sqlite"))

# Get path to example data
data_path <- system.file("extdata", package = "RSQLite.toolkit")

# Load abalone CSV data with automatic primary key
dbTableFromDSV(
  input_file = file.path(data_path, "abalone.csv"),
  dbcon = dbcon,
  table_name = "ABALONE",
  drop_table = TRUE,
  auto_pk = TRUE,
  header = TRUE,
  sep = ",",
  dec = "."
)
#> [1] 4177

# Check the imported data
dbListFields(dbcon, "ABALONE")
#>  [1] "Sex"     "Length"  "Diam"    "Height"  "Whole"   "Shucked" "Viscera"
#>  [8] "Shell"   "Rings"   "SEQ"    
head(dbGetQuery(dbcon, "SELECT * FROM ABALONE"))
#>   Sex Length  Diam Height  Whole Shucked Viscera Shell Rings SEQ
#> 1   M  0.455 0.365  0.095 0.5140  0.2245  0.1010 0.150    15   1
#> 2   M  0.350 0.265  0.090 0.2255  0.0995  0.0485 0.070     7   2
#> 3   F  0.530 0.420  0.135 0.6770  0.2565  0.1415 0.210     9   3
#> 4   M  0.440 0.365  0.125 0.5160  0.2155  0.1140 0.155    10   4
#> 5   I  0.330 0.255  0.080 0.2050  0.0895  0.0395 0.055     7   5
#> 6   I  0.425 0.300  0.095 0.3515  0.1410  0.0775 0.120     8   6

# Load data with specific column selection
dbTableFromDSV(
  input_file = file.path(data_path, "abalone.csv"),
  dbcon = dbcon,
  table_name = "ABALONE_SUBSET",
  drop_table = TRUE,
  header = TRUE,
  sep = ",",
  dec = ".",
  col_import = c("Sex", "Length", "Diam", "Whole")
)
#> [1] 4177

head(dbGetQuery(dbcon, "SELECT * FROM ABALONE_SUBSET"))
#>   Sex Length  Diam  Whole
#> 1   M  0.455 0.365 0.5140
#> 2   M  0.350 0.265 0.2255
#> 3   F  0.530 0.420 0.6770
#> 4   M  0.440 0.365 0.5160
#> 5   I  0.330 0.255 0.2050
#> 6   I  0.425 0.300 0.3515

# Check available tables
dbListTables(dbcon)
#> [1] "ABALONE"         "ABALONE_SUBSET"  "ABALONE_SUMMARY" "PENGUINS"       

# Clean up
dbDisconnect(dbcon)
```
