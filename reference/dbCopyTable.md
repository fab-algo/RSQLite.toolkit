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

## Examples

``` r
db_source <- tempfile(fileext = ".sqlite")
db_target <- tempfile(fileext = ".sqlite")

# Load some sample data
dbcon <- dbConnect(RSQLite::SQLite(), db_source)

data_path <- system.file("extdata", package = "RSQLite.toolkit")
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

dbDisconnect(dbcon)

# Copy the table to a new database, recreating it 
# if it already exists and copying indexes
dbCopyTable(
  db_file_src = db_source, 
  db_file_tgt = db_target,
  table_name = "ABALONE",
  drop_table = TRUE,        # Recreate table if it exists
  copy_indexes = TRUE       # Copy indexes too
)

# Check that the table was copied correctly
dbcon_tgt <- dbConnect(RSQLite::SQLite(), db_target)
print(dbListTables(dbcon_tgt))
#> [1] "ABALONE"
print(dbListFields(dbcon_tgt, "ABALONE"))
#>  [1] "Sex"     "Length"  "Diam"    "Height"  "Whole"   "Shucked" "Viscera"
#>  [8] "Shell"   "Rings"   "SEQ"    
print(dbGetQuery(dbcon_tgt, "SELECT COUNT(*) AS TOTAL_ROWS FROM ABALONE;"))
#>   TOTAL_ROWS
#> 1       4177
dbDisconnect(dbcon_tgt)

# Clean up temporary database files
unlink(c(db_source, db_target))
```
