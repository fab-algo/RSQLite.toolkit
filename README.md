
<!-- README.md is generated from README.Rmd. Please edit that file -->

# RSQLite.toolkit <a href="https://github.com/fab-algo/RSQLite.toolkit"><img src="man/figures/logo.png" align="right" height="138" alt="RSQLite.toolkit website"/></a>

<!-- badges: start -->
<!-- badges: end -->

RSQLite.toolkit is a lightweight wrapper around the RSQLite package for
streamlined loading of data from **tabular files** (i.e. text delimited
files like CSV and TSV, Microsoft Excel, and Arrow IPC files) in SQLite
databases.

It also includes helper functions for inspecting the structure of the
input files, and some functions to simplify activities on the SQLite
tables.

## Installation

You can install the development version of RSQLite.toolkit from
[GitHub](https://github.com/) with:

<div class=".pkgdown-devel">

``` r
# Install development version from GitHub
# install.packages("pak")
pak::pak("fab-algo/RSQLite.toolkit")
```

</div>

## Usage

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

## creates the table PORTFOLIO_PERF from Excel file, using the "all period" sheet
dbTableFromXlsx(input_file = file.path(data_path,
                                       "stock_portfolio.xlsx"),
                dbcon = dbcon, table_name = "PORTFOLIO_PERF",
                drop_table = TRUE,
                sheet_name = "all period", first_row = 2, cols_range = "A:S")
#> [1] 63

## creates the table PENGUINS from Feather file
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

## Description

The basic idea behind this package is that storing all the data used
throughout a data‑analysis workflow in a local database is a highly
efficient and effective way to manage information. The advantages of
keeping raw data and their metadata together in the same
database—ideally under a shared data model—far outweigh not only the
additional overhead of maintaining a structured system, but also the
costs and complexity involved in importing information from external
files with disparate formats and conventions.

The purpose of this package is to reduce the burden of the last point
(i.e., importing the data) as much as possible, through a set of
functions that read the data from the source files, create the
destination table (if it does not exist), store the data there and index
it, all in one step.

The core functions of the package are therefore those that can be used
to move data from a file to a database table:

- `dbTableFromDSV()`
- `dbTableFromXlsx()`
- `dbTableFromFeather()`

Together with these, there are a couple of additional functions:

- `dbTableFromDataFrame()`
- `dbTableFromView()`

that could be handy to move data from intermediate data structures to
database tables, using the same logic of the core ones.

All these functions share a common calling template that is outlined in
the following picture:

<img src="man/figures/README-dbTableFrom_template.png" alt="" width="100%" style="display: block; margin: auto;" />

All core functions require three mandatory arguments:

- `input_file`
- `dbcon`
- `table_name`

that have no default values. The `dbTableFromXlsx()` requires three
additional arguments (again with no defaults): `sheet_name`,
`first_row`, `cols_range` that are needed to correctly locate the
dataset to be imported inside the Excel file.

More details on the parameters and functions’ behaviour are available in
the standard function documentation accessible through R’s help system.

Anyway, be careful with default values of all other arguments,
especially when importing from *delimiter separated values* text files:
the default values for input data interpretation arguments are seldom
the right ones. This is also true for the default values of the
functions actually used by this package to read data from files, that
is:

- `base::scan()` for reading from DSV files;
- `openxlsx2::wb_to_df()` for reading from Xlsx files;
- `arrow::FeatherReader()` for reading from Feather files.

Given the high variability in how data can be stored in DSV and Excel
files, there is always the possibility of passing specific parameters of
the above listed functions through the special argument “`...`”. To help
with using the additional arguments of `dbTableFromDSV()`, along with
those of `base::scan()`, you can read the **vignette “Dealing with DSV
files”** that is part of this package. The first section of the document
describes how to use `base::scan()` arguments to read complex DSV files.
In the second section there is a detailed description of the issues
related to the column names used in the data file and their
corresponding identifiers in the SQLite table’s fields. The second
section of this vignette can be helpful also when importing data from
Xlsx and Feather files.

## Data sources

The example datasets, included in the `extdata` package directory, have
been retrieved from the following sources:

- `"abalone.csv"`: Predicting the age of abalone from physical
  measurements. Source link:
  <https://archive.ics.uci.edu/dataset/1/abalone>
- `"stock_portfolio.xlsx"`: Stock portfolio performance under a new
  weighted scoring stock selection model. Source link:
  <https://archive.ics.uci.edu/dataset/390/stock+portfolio+performance>
- `"penguins.feather"`: The Palmer Archipelago’s penguins data set.
  Source link: <https://allisonhorst.github.io/palmerpenguins/>. The
  “feather” file format of the dataset was downloaded from
  <https://github.com/lmassaron/datasets>.
