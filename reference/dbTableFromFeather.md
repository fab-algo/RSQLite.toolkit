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
  columns, and the corresponding values will be associeted to each
  record imported from the input file. It is useful to keep track of
  additional information (e.g., the input file name, additional context
  data not available in the data set, ...) when loading the content of
  multiple input files in the same table. Defults to `NULL`.

## Value

integer, the number of records in `table_name` after reading data from
`input_file`.
