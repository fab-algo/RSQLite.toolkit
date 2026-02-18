# Create a table in a SQLite database from a view

The `dbTableFromView()` function creates a table in a SQLite database
from a view already present in the same database.

## Usage

``` r
dbTableFromView(
  view_name,
  dbcon,
  table_name,
  drop_table = FALSE,
  build_pk = FALSE,
  pk_fields = NULL
)
```

## Arguments

- view_name:

  character, name of the view.

- dbcon:

  database connection, as created by the dbConnect function.

- table_name:

  character, the name of the table.

- drop_table:

  logical, if `TRUE` the target table will be dropped (if exists) and
  recreated when importing the data. if `FALSE`, data from input file
  will be appended to an existing table. Defaults to `FALSE`.

- build_pk:

  logical, if `TRUE` creates a `UNIQUE INDEX` named `<table_name>_PK`
  defined by the combination of fields specified in the `pk_fields`
  parameter. It will be effective only if `pk_fields` is not null.
  Defaults to `FALSE`.

- pk_fields:

  character vector, the list of the fields' names that define the
  `UNIQUE INDEX`. Defaults to `NULL`.

## Value

integer, the number of records in `table_name` after writing data from
the input view.
