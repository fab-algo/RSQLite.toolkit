# Copy a table from one SQLite database to another

The `dbCopyTable()` function can be used to create a copy of the data in
a table of a SQLite database in another database. The data can be
appended to an already existing table (with the same name of the source
one), or a new table can be created. It is possible to move also the
indexes from source to target.

## Usage

``` r
dbCopyTable(
  db_file_src,
  db_file_tgt,
  table_name,
  drop_table = FALSE,
  copy_indexes = FALSE
)
```

## Arguments

- db_file_src:

  character, the file name (including path) of the source database
  containing the table to be copied.

- db_file_tgt:

  character, the file name (including path) of the target database where
  the table will be copied.

- table_name:

  character, the table name.

- drop_table:

  logical, if `TRUE` the table in the target database will be dropped
  (if exists) before copying the data. If `FALSE`, the data will be
  appended to an existing table in the target database. Defaults to
  `FALSE`.

- copy_indexes:

  logical, if `TRUE` and also `drop_table` is `TRUE`, all indexes
  defined on the source table will be created on the target table.
  Defaults to `FALSE`.

## Value

nothing
