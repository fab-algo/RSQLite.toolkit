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

## Examples

``` r
# Create a database and execute SQL from a file
library(RSQLite.toolkit)

# Set up database connection
dbcon <- dbConnect(RSQLite::SQLite(), file.path(tempdir(), "example.sqlite"))

# Load some sample data
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

# Create a SQL file with multiple statements
sql_content <- "
-- Create a summary table
DROP TABLE IF EXISTS ABALONE_SUMMARY;

CREATE TABLE ABALONE_SUMMARY AS 
SELECT SEX, 
       COUNT(*) as TOTAL_COUNT,
       ROUND(AVG(LENGTH), 3) as AVG_LENGTH,
       ROUND(AVG(WHOLE), 3) as AVG_WEIGHT
FROM ABALONE 
GROUP BY SEX;

-- Query the results  
SELECT * FROM ABALONE_SUMMARY ORDER BY SEX;

-- Parameterized query example
SELECT SEX, COUNT(*) as COUNT 
FROM ABALONE 
WHERE LENGTH > :min_length
GROUP BY SEX;
"

sql_file <- tempfile(fileext = ".sql")
writeLines(sql_content, sql_file)

# Execute SQL statements with parameters
plist <- list(
  NULL,  # DROP TABLE statement (no parameters)
  NULL,  # CREATE TABLE statement (no parameters)
  NULL,  # First SELECT (no parameters) 
  list(min_length = 0.5)  # Parameterized SELECT
)

results <- dbExecFile(
  input_file = sql_file,
  dbcon = dbcon,
  plist = plist
)

# Check results
print(results[[3]])  # Summary data
#>   Sex TOTAL_COUNT AVG_LENGTH AVG_WEIGHT
#> 1   F        1307      0.579      1.047
#> 2   I        1342      0.428      0.431
#> 3   M        1528      0.561      0.991
print(results[[4]])  # Filtered data
#>   Sex COUNT
#> 1   F  1061
#> 2   I   364
#> 3   M  1161

# Clean up
unlink(sql_file)
dbDisconnect(dbcon)
```
