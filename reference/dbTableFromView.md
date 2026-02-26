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

## Examples

``` r
# Create a temporary database and demonstrate view to table conversion
library(RSQLite.toolkit)

# Set up database connection
dbcon <- dbConnect(RSQLite::SQLite(), file.path(tempdir(), "example.sqlite"))

# Load some sample data first
data_path <- system.file("extdata", package = "RSQLite.toolkit")
dbTableFromDSV(
  input_file = file.path(data_path, "abalone.csv"),
  dbcon = dbcon,
  table_name = "ABALONE",
  drop_table = TRUE,
  header = TRUE,
  sep = ",",
  dec = "."
)
#> [1] 4177

# Create a view with aggregated data
dbExecute(dbcon, "DROP VIEW IF EXISTS VW_ABALONE_SUMMARY;")
#> [1] 0

dbExecute(dbcon, 
  "CREATE VIEW VW_ABALONE_SUMMARY AS 
   SELECT SEX, 
          COUNT(*) as COUNT, 
          AVG(LENGTH) as AVG_LENGTH,
          AVG(WHOLE) as AVG_WEIGHT
   FROM ABALONE 
   GROUP BY SEX"
)
#> [1] 0

# Convert the view to a permanent table
dbTableFromView(
  view_name = "VW_ABALONE_SUMMARY",
  dbcon = dbcon,
  table_name = "ABALONE_STATS",
  drop_table = TRUE
)
#> [1] 3

# Check the result
dbListTables(dbcon)
#> [1] "ABALONE"            "ABALONE_STATS"      "ABALONE_SUBSET"    
#> [4] "ABALONE_SUMMARY"    "PENGUINS"           "PENGUINS_SUBSET"   
#> [7] "SAMPLE_DATA"        "SAMPLE_SUBSET"      "VW_ABALONE_SUMMARY"
dbGetQuery(dbcon, "SELECT * FROM ABALONE_STATS")
#>   Sex COUNT AVG_LENGTH AVG_WEIGHT
#> 1   F  1307  0.5790933  1.0465321
#> 2   I  1342  0.4277459  0.4313625
#> 3   M  1528  0.5613907  0.9914594

# Clean up
dbDisconnect(dbcon)
```
