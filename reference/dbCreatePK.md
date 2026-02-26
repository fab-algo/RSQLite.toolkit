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

## Examples

``` r
# Create a database and table, then add a primary key
library(RSQLite.toolkit)

# Set up database connection
dbcon <- dbConnect(RSQLite::SQLite(), file.path(tempdir(), "example.sqlite"))

# Load sample data
data_path <- system.file("extdata", package = "RSQLite.toolkit")

dbTableFromFeather(
  input_file = file.path(data_path, "penguins.feather"),
  dbcon = dbcon, table_name = "PENGUINS",
  drop_table = TRUE
)
#> [1] 333

dbGetQuery(dbcon, "select species, sex, body_mass_g,
                   culmen_length_mm, culmen_depth_mm 
                   from PENGUINS 
                   group by species, sex, body_mass_g,
                   culmen_length_mm, culmen_depth_mm 
                   having count(*) > 1")
#> [1] species          sex              body_mass_g      culmen_length_mm
#> [5] culmen_depth_mm 
#> <0 rows> (or 0-length row.names)

# Create a primary key on multiple fields
dbCreatePK(dbcon, "PENGUINS", 
           c("species", "sex", "body_mass_g",
             "culmen_length_mm", "culmen_depth_mm"))
#> [1] 0

# Check that the index was created
dbGetQuery(dbcon, 
  "SELECT name, sql FROM sqlite_master WHERE type='index' AND tbl_name='PENGUINS'")
#>          name
#> 1 PENGUINS_PK
#>                                                                                                              sql
#> 1 CREATE UNIQUE INDEX PENGUINS_PK ON  PENGUINS  ( species, sex, body_mass_g, culmen_length_mm, culmen_depth_mm )

# Clean up
dbDisconnect(dbcon)
```
