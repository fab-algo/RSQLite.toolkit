# Execute SQL statements from a text file

The `dbExecFile()` function executes the SQL statements contained in a
text file.

This function reads the text in `input_file`, strips all comment lines
(i.e. all lines beginning with `--` characters) and splits the SQL
statements assuming that they are separeted by the `;` character. The
list of SQL statements is then executed, one at a time; the results of
each statements are stored in a list with length equal to the number of
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
  Defaults to `NULL`

## Value

a list with the results of each statement executed.
