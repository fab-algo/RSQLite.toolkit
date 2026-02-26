# Create a table from a Feather (Arrow IPC) file

The `dbTableFromFeather()` function reads the data from a Feather (Arrow
IPC) file and copies it to a table in a SQLite database. If table does
not exist, it will create it.

The `dbTableFromFeather()` function reads the data from an Apache Arrow
table serialized in a Feather (Arrow IPC) file and copies it to a table
in a SQLite database. If table does not exist, it will create it.

## Usage

``` r
dbTableFromFeather(
  input_file,
  dbcon,
  table_name,
  id_quote_method = "DB_NAMES",
  col_names = NULL,
  col_types = NULL,
  col_import = NULL,
  drop_table = FALSE,
  auto_pk = FALSE,
  build_pk = FALSE,
  pk_fields = NULL,
  constant_values = NULL
)
```

## Arguments

- input_file:

  character, the file name (including path) to be read.

- dbcon:

  database connection, as created by the dbConnect function.

- table_name:

  character, the name of the table.

- id_quote_method:

  character, used to specify how to build the SQLite columns' names
  using the fields' identifiers read from the input file. For details
  see the description of the `quote_method` parameter of the
  [`format_column_names()`](https://fab-algo.github.io/RSQLite.toolkit/reference/format_column_names.md)
  function. Defaults to `DB_NAMES`.

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
  recreated before importing the data. If `FALSE`, data from the input
  file will be appended to an existing table. Defaults to `FALSE`.

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
  columns, and the corresponding values will be associated to each
  record imported from the input file. It is useful to keep track of
  additional information (e.g., the input file name, additional context
  data not available in the data set, ...) when loading the content of
  multiple input files in the same table. Defults to `NULL`.

## Value

integer, the number of records in `table_name` after reading data from
`input_file`.

## Examples

``` r
# Create a temporary database and load Feather data
library(RSQLite.toolkit)

# Set up database connection
dbcon <- dbConnect(RSQLite::SQLite(), file.path(tempdir(), "example.sqlite"))

# Get path to example data
data_path <- system.file("extdata", package = "RSQLite.toolkit")

# Load penguins Feather data
dbTableFromFeather(
  input_file = file.path(data_path, "penguins.feather"),
  dbcon = dbcon,
  table_name = "PENGUINS",
  drop_table = TRUE
)
#> [1] 333

# Check the imported data
dbListFields(dbcon, "PENGUINS")
#> [1] "species"           "culmen_length_mm"  "culmen_depth_mm"  
#> [4] "flipper_length_mm" "body_mass_g"       "sex"              
head(dbGetQuery(dbcon, "SELECT * FROM PENGUINS"))
#>   species culmen_length_mm culmen_depth_mm flipper_length_mm body_mass_g    sex
#> 1  Adelie             39.1            18.7               181        3750   MALE
#> 2  Adelie             39.5            17.4               186        3800 FEMALE
#> 3  Adelie             40.3            18.0               195        3250 FEMALE
#> 4  Adelie             36.7            19.3               193        3450 FEMALE
#> 5  Adelie             39.3            20.6               190        3650   MALE
#> 6  Adelie             38.9            17.8               181        3625 FEMALE

# Load with custom column selection and types
dbTableFromFeather(
  input_file = file.path(data_path, "penguins.feather"),
  dbcon = dbcon,
  table_name = "PENGUINS_SUBSET",
  drop_table = TRUE,
  col_import = c("species", "flipper_length_mm", "body_mass_g", "sex")
)
#> [1] 333

# Check the imported data
dbListFields(dbcon, "PENGUINS_SUBSET")
#> [1] "species"           "flipper_length_mm" "body_mass_g"      
#> [4] "sex"              
head(dbGetQuery(dbcon, "SELECT * FROM PENGUINS_SUBSET"))
#>   species flipper_length_mm body_mass_g    sex
#> 1  Adelie               181        3750   MALE
#> 2  Adelie               186        3800 FEMALE
#> 3  Adelie               195        3250 FEMALE
#> 4  Adelie               193        3450 FEMALE
#> 5  Adelie               190        3650   MALE
#> 6  Adelie               181        3625 FEMALE

# Check available tables
dbListTables(dbcon)
#> [1] "ABALONE"         "ABALONE_SUBSET"  "ABALONE_SUMMARY" "PENGUINS"       
#> [5] "PENGUINS_SUBSET" "SAMPLE_DATA"     "SAMPLE_SUBSET"  

# Clean up
dbDisconnect(dbcon)
```
