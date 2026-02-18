# Creates a unique index on a table in a SQLite database

The `dbCreatePK()` function creates a `UNIQUE INDEX` named
`<table_name>_PK` on the table specified by `table_name` in the database
connected by `dbcon`. The index is created on the fields specified in
the `pk_fields` argument.

## Usage

``` r
dbCreatePK(dbcon, table_name, pk_fields, drop_index = FALSE)
```

## Arguments

- dbcon:

  database connection, as created by the dbConnect function.

- table_name:

  character, the name of the table where the index will be created.

- pk_fields:

  character vector, the list of the fields' names that define the
  `UNIQUE INDEX`.

- drop_index:

  logical, if `TRUE` the index named `<table_name>_PK` will be dropped
  (if exists) before recreating it. If `FALSE`, it will check if an
  index with that name exists and eventually stops. Default to `FALSE`.

## Value

nothing
