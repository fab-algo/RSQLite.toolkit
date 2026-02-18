# error_handler manage error messages for package

error_handler manage error messages for package

## Usage

``` r
error_handler(err, fun, step)
```

## Arguments

- err:

  character, error message

- fun:

  character, function name where error happened

- step:

  integer, code identifying the step in the function where error
  happened. For dbTableFrom... functions steps are:

  - 101,121: read file schema (DSV, Xlsx)

  - 102: handle col_names and col_types

  - 103: create empty table

  - 104: read data

  - 105: write data

  - 106: indexing

## Value

nothing
