
<!-- README.md is generated from README.Rmd. Please edit that file -->

# RSQLite.toolkit <a href="https://github.com/fab-algo/RSQLite.toolkit">

<img src="man/figures/RSQLite.toolkit_logo.png" align="right" height="138" />
</a>

<!-- badges: start -->
<!-- badges: end -->

RSQLite.toolkit is lightweight wrapper around the RSQLite package for
streamlined loading of data from **tabular files** (i.e. text delimited
files like CSV and TSV, Microsoft Excel, and Arrow IPC files) in SQLite
databases.

It includes also helper functions for inspecting the structure of the
input files, and some functions to simplify activities on the SQLite
tables.

## Installation

You can install the development version of RSQLite.toolkit from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("fab-algo/RSQLite.toolkit")
```

## Examples

These basic examples show how to use the core functions of the package
to load example data in different tables in a test database:

``` r
library(RSQLite.toolkit)
#> Loading required package: RSQLite

dbcon <- dbConnect(RSQLite::SQLite(), file.path(tempdir(), "tests.sqlite"))

data_path <- system.file("extdata", package = "RSQLite.toolkit")

## creates the table ABALONE from a CSV file, adding the field 'SEQ' with the
## ROWID of each record through the use of the parameter 'auto_pk=TRUE'
dbTableFromDSV(input_file = file.path(data_path, "abalone.csv"),
               dbcon = dbcon, table_name = "ABALONE",
               drop_table = TRUE, auto_pk = TRUE,
               header = TRUE, sep = ",", dec = ".")
#> [1] 4177

## creates table PORTFOLIO_PERF from Excel file, using the "all period" sheet
dbTableFromXlsx(input_file = file.path(data_path,
                                       "stock_portfolio.xlsx"),
                dbcon = dbcon, table_name = "PORTFOLIO_PERF",
                drop_table = TRUE,
                sheet_name = "all period", first_row = 2, cols_range = "A:S")
#> [1] 63

## creates table PENGUINS from Feather file
dbTableFromFeather(input_file = file.path(data_path, "penguins.feather"),
                   dbcon = dbcon, table_name = "PENGUINS",
                   drop_table = TRUE)
#> [1] 333

dbListTables(dbcon)
#> [1] "ABALONE"        "PENGUINS"       "PORTFOLIO_PERF"

dbListFields(dbcon, "ABALONE")
#>  [1] "Sex"     "Length"  "Diam"    "Height"  "Whole"   "Shucked" "Viscera"
#>  [8] "Shell"   "Rings"   "SEQ"
dbListFields(dbcon, "PENGUINS")
#> [1] "species"           "culmen_length_mm"  "culmen_depth_mm"  
#> [4] "flipper_length_mm" "body_mass_g"       "sex"
dbListFields(dbcon, "PORTFOLIO_PERF")[1:5]
#> [1] "ID"                                   
#> [2] "Large_B_P"                            
#> [3] "Large_ROE"                            
#> [4] "Large_S_P"                            
#> [5] "Large_Return_Rate_in_the_last_quarter"

dbDisconnect(dbcon)
```

To inspect the structure of the input files, you can use the following
functions:

``` r
library(RSQLite.toolkit)

data_path <- system.file("extdata", package = "RSQLite.toolkit")

file_schema_dsv(input_file = file.path(data_path, "abalone.csv"),
                header = TRUE, sep = ",", dec = ".")$schema[, c(1, 3:7)]
#>   col_names col_types sql_types src_names src_types src_is_quoted
#> 1       Sex character      TEXT       Sex      text         FALSE
#> 2    Length   numeric      REAL    Length      text         FALSE
#> 3      Diam   numeric      REAL      Diam      text         FALSE
#> 4    Height   numeric      REAL    Height      text         FALSE
#> 5     Whole   numeric      REAL     Whole      text         FALSE
#> 6   Shucked   numeric      REAL   Shucked      text         FALSE
#> 7   Viscera   numeric      REAL   Viscera      text         FALSE
#> 8     Shell   numeric      REAL     Shell      text         FALSE
#> 9     Rings   integer   INTEGER     Rings      text         FALSE

file_schema_feather(input_file = file.path(data_path,
                                           "penguins.feather"))[, c(1, 3:6)]
#>           col_names col_types sql_types         src_names src_types
#> 1           species character      TEXT           species      utf8
#> 2  culmen_length_mm    double      REAL  culmen_length_mm    double
#> 3   culmen_depth_mm    double      REAL   culmen_depth_mm    double
#> 4 flipper_length_mm    double      REAL flipper_length_mm    double
#> 5       body_mass_g    double      REAL       body_mass_g    double
#> 6               sex character      TEXT               sex      utf8
```

## How to use the package

The basic idea behind this package is that storing all the data used
throughout a data‑analysis workflow in a database is a highly efficient
and effective way to manage information. The advantages of keeping raw
data and their metadata together in the same database—ideally under a
shared data model—far outweigh not only the additional overhead of
maintaining a structured and centralized system, but also the costs and
complexity involved in importing information from external files with
disparate formats and conventions.

The purpose of this package is to reduce the burden of the last point
(i.e., importing the data) as much as possible, through a set of
function that

The core functions of the package are those that can be used to move
data from a file to a database table:

- `dbTableFromDSV()`
- `dbTableFromXlsx()`
- `dbTableFromFeather()`

All these functions share a common calling template that is outlined in
the following picture:

<img src="man/figures/README-dbTableFrom_template.png" width="100%" style="display: block; margin: auto;" />

Be careful with default values, especially when importing form
*delimiter separated values* text files: the default values for input
data interpretation parameters are seldom the right ones.

## Data sources

The example datasets, included in the `extdata` package directory, have
been retrieved from the following sources:

- `"abalone.csv"`: Predicting the age of abalone from physical
  measurements. Source link:
  <https://archive.ics.uci.edu/dataset/1/abalone>
- `"stock_portfolio.xlsx"`: Stock portfolio performance under a new
  weighted scoring stock selection model. Source link:
  <https://archive.ics.uci.edu/dataset/390/stock+portfolio+performance>
- `"penguins.feather"`: The Palmer Arcipelago’s penguins data set.
  Source link: <https://allisonhorst.github.io/palmerpenguins/>. The
  “feather” file format of the dataset was downloaded from
  <https://github.com/lmassaron/datasets>.
