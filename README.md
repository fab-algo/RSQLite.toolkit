
<!-- README.md is generated from README.Rmd. Please edit that file -->

# RSQLite.toolkit <a href="https://github.com/fab-algo/RSQLite.toolkit"><img src="man/figures/RSQLite.toolkit_logo.png" align="right" height="138" /></a>

<!-- badges: start -->
<!-- badges: end -->

RSQLite.toolkit is lightweight wrapper around the RSQLite package for
streamlined loading of data from tabular files (i.e. text delimited
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

## Example

This basic example shows you how to use the core functions of the
package to load some data in different tables in a test database:

``` r
library(RSQLite.toolkit)

dbcon <- dbConnect(dbDriver("SQLite"), "./data/test.sqlite")

## creates the table IRIS from a CSV file, adding
## the field 'SEQ' with the ROWID of each record
## through the use of the parameter 'auto_pk=TRUE'
dbTableFromDSV(input_file = "./data/src/iris.csv",
               dbcon=dbcon, table_name="IRIS",
               header=T, sep=",", dec=".",
               drop_table=TRUE, auto_pk=TRUE)

## creates table PASSENGERS from Feather file
dbTableFromFeather(input_file="./data/src/passengers.feather",
                   dbcon=dbcon, table_name="PASSENGERS")

dbListTables(dbcon)

dbDisconnect(dbcon)
```
