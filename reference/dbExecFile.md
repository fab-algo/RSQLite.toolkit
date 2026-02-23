# Execute SQL statements from a text file

The `dbExecFile()` function executes the SQL statements contained in a
text file.

This function reads the text in `input_file`, strips all comment lines
(i.e. all lines beginning with `--` characters) and splits the SQL
statements assuming that they are separated by the `;` character. The
list of SQL statements is then executed, one at a time; the results of
each statement are stored in a list with length equal to the number of
statements.

## Usage

``` r
dbExecFile(input_file, dbcon, plist = NULL)
```

## Arguments

- input_file:

  the file name (including path) containing the SQL statements to be
  executed

- dbcon:

  database connection, as created by the dbConnect function.

- plist:

  a list with values to be binded to the parameters of SQL statements.
  It should have the same length as the number of SQL statements. If any
  of the statements do not require parameters, the corresponding element
  of the list should be set to `NULL`. If no statements require
  parameters, `plist` can be set to `NULL`.

## Value

a list with the results returned by each statement executed.
