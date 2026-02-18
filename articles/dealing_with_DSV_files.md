# Dealing with DSV files

“Delimiter-separated values” (DSV) files are (unfortunately) still a
very common way to serialize tabular data and are widely used as a data
exchange format. Since there isn’t a strict technical standard that
specifies how different data types should be represented in plain text,
and a lot of possible customizations are left to the final users, it is
very common to encounter issues when data is imported from this kind of
files. The purpose of this article is to show some problems you can
encounter loading data in a SQLite database from DSV files and how to
deal with them.

## Data interpretation issues

When dealing with DSV files, one of the most common sources of issues is
the way data is represented in the file. Since DSV files are plain text
files, there are many conventions that can be used to represent
different data types (e.g., strings, numbers, dates, missing data,
etc.).

``` r
library(RSQLite.toolkit)
#> Loading required package: RSQLite
```

Let’s start with a simple example using real-world data. To retrieve
some example files, we can use the package
[piggyback](https://github.com/ropensci/piggyback) to download public
datasets that have been stored in the
[RSQLite.toolkit-tests](https://github.com/fab-algo/RSQLite.toolkit-tests)
GitHub repository[¹](#fn1).

The first example will deal with a table contained in a “standard” CSV
file (i.e. a text file with the first line containing column names,
using the comma to separate fields’ values and the dot as a decimal
separator for numbers); we will load the data in a new table inside a
SQLite database using the
[`dbTableFromDSV()`](https://fab-algo.github.io/RSQLite.toolkit/reference/dbTableFromDSV.md)
function.

First, we download the data from the repo:

``` r
library("piggyback", quietly = TRUE)
pb_download(file = "DOSE_V2.10.zip", dest = tempdir(),
            repo = "fab-algo/RSQLite.toolkit-tests", tag = "latest")

unzip(zipfile = file.path(tempdir(), "DOSE_V2.10.zip"), exdir = tempdir())
dir(file.path(tempdir(), "DOSE_V2.10"))
#> [1] "DOSE_V2.10.csv"        "DoseV2p10_changes.pdf"

data_file <- file.path(tempdir(), "DOSE_V2.10/DOSE_V2.10.csv")
```

After unzipping the downloaded file, we get the `DOSE_V2.10.csv` file
that contains the dataset we are interested in. We can now check the
number of rows in the input file:

``` r
n_rows <- length(count.fields(data_file))
n_rows
#> [1] 46852
```

So we expect to have 46851 records in the SQLite table at the end of the
import process. Now we connect to a sample database
(i.e. `tests.sqlite`):

``` r
dbcon <- dbConnect(RSQLite::SQLite(), file.path(tempdir(), "tests.sqlite"))
```

and move the dataset in a table there, calling the
[`dbTableFromDSV()`](https://fab-algo.github.io/RSQLite.toolkit/reference/dbTableFromDSV.md)
function specifying the connection to the database (i.e., `dbcon`
parameter) and the table name (i.e., `table_name` parameter). We don’t
need to specify the parameters for the CSV file structure (i.e.,
`header = TRUE`, `sep = ","`, `dec = "."`, `grp = ""`) since the default
values will be fine.

``` r
## do not run: error
dbTableFromDSV(input_file = data_file, dbcon = dbcon, table_name = "DOSE")
#> Error:
#> ! Blocking error in dbTableFromDSV while reading data from input file.
#> Original error msg: scan() expected 'an integer', got 'GRC.7_1'
```

To understand what happened we need to keep in mind that the
[`dbTableFromDSV()`](https://fab-algo.github.io/RSQLite.toolkit/reference/dbTableFromDSV.md)
function calls *`file_schema_dsv`* function, to guess the table
structure stored in the DSV file, and then it calls the *`scan`*
function to actually read the file content in a data frame and write it
to the SQLite table.

Both these functions make some assumptions about how the data is stored
in the DSV file as “plain text”. To that end, the `scan` function uses
the parameters listed in the following table along with their default
values (we inclued also those of the `read.table` function for
comparison):

| parameters   | *scan*  | read.table |
|--------------|---------|------------|
| quote        | `"'\""` | `"\"'"`    |
| allowEscapes | `FALSE` | `FALSE`    |
| skipNul      | `FALSE` | `FALSE`    |
| strip.white  | `FALSE` | `FALSE`    |
| comment.char | `""`    | `"#"`      |
| na.strings   | `"NA"`  | `"NA"`     |
| fill         | `FALSE` | `FALSE`    |
| fileEncoding | `""`    | `""`       |

`scan` and `read.table` parameters to handle data interpretation.

These parameters control the conventions used to interpret the text read
from the file:

- *quoting*: convention used to include in strings “special” characters
  or to preserve the exact string without interpretation. Parameter:
  `quote`
- *escaping*: rule used to represent characters that would otherwise be
  interpreted as delimiters or special commands. Parameter:
  `allowEscapes`
- *NUL characters*: how to manage ASCII NUL characters that could be
  found inside strings. Parameter: `skipNul`
- *whitespace stripping*: whether unwanted spaces from the beginning,
  end, or middle of a string should be removed. Parameter: `strip.white`
- *commenting*: a character used to identify that one line (or the rest
  of a line after the “comment” identifier) does not contain data and
  should be ignored. Parameter: `comment.char`
- *missing data*: how missing data is represented in the file.
  Parameter: `na.strings`
- *missing fields*: how to manage rows with less fields then expected.
  Parameter: `fill`
- *encoding*: the technical standard used to represent characters in
  binary form, such as ASCII, UTF-8, EBCDIC. Parameter: `fileEncoding`

To understand how to use each of these parameters you are strongly
encouraged to read the help pages of the `scan` function.

It should be noted that the default values are seldom—almost never—the
right ones and we should invest some time in better understanding the
strategies used by the data source. If we have a technical description
of the assumptions used in creating the file, we can follow those
guidelines, otherwise we have to inspect the file and determine them
through direct examination.

To look into a DSV file (especially big files) it is better to use log
file explorers and avoid general purpose text editors. As an example, we
can use [klogg](https://klogg.filimonov.dev/) that is available for all
major operating systems.

If we open the file and inspect some records, we understand that:

- there are strings with comma and single quote inside, surrounded by
  double quotes, so the file is using a specific quoting convention;
- missing data is represented by a “zero-lenght” string;
- many strings use special characters that need a specific encoding
  system; in this file the UTF-8 encoding is used.

For the input file used in this example we need:

- to explicitly force the `quote` parameter to `"\""` (i.e., only the
  double quote character) and the `na.strings` parameter to `""`;
- to turn off the commenting option: `comment.char = ""`;
- to tell the [`base::scan()`](https://rdrr.io/r/base/scan.html)
  function that the input file is encoded in UTF-8:
  `fileEncoding = "UTF-8"`.

``` r
f_schema1 <- file_schema_dsv(input_file = data_file,
                             quote = "\"", na.strings = "",
                             comment.char = "", fileEncoding = "UTF-8")
```

Now we can see that the guessed schema is correct. Let’s look at the
first few rows of the file schema created using these parameters:

``` r
f_schema1$schema[1:8, ]
#>    col_names col_names_unquoted col_types sql_types  src_names src_types
#> 1    country            country character      TEXT    country      text
#> 2     region             region character      TEXT     region      text
#> 3      GID_0              GID_0 character      TEXT      GID_0      text
#> 4      GID_1              GID_1 character      TEXT      GID_1      text
#> 5     F_year             F_year   integer   INTEGER       year      text
#> 6    grp_lcu            grp_lcu   numeric      REAL    grp_lcu      text
#> 7        pop                pop   numeric      REAL        pop      text
#> 8 grp_pc_lcu         grp_pc_lcu   numeric      REAL grp_pc_lcu      text
#>   src_is_quoted
#> 1         FALSE
#> 2         FALSE
#> 3         FALSE
#> 4         FALSE
#> 5         FALSE
#> 6         FALSE
#> 7         FALSE
#> 8         FALSE
```

The
[`file_schema_dsv()`](https://fab-algo.github.io/RSQLite.toolkit/reference/file_schema_dsv.md)
function returns a list with many useful information about the input
file, including the `schema` data frame that describes the columns found
in the file (i.e. name, type, size, etc.). For more details about it,
please refer to its documentation. One point to note is that the `year`
column has been modified to `F_year` since `year` is a reserved keyword
in SQLite. That’s the default behavior of the
[`dbTableFromDSV()`](https://fab-algo.github.io/RSQLite.toolkit/reference/dbTableFromDSV.md)
function when dealing with column names (see next section for more
details).

Now we can re-run the
[`dbTableFromDSV()`](https://fab-algo.github.io/RSQLite.toolkit/reference/dbTableFromDSV.md)
function specifying the correct parameters to interpret the data in the
input file:

``` r
dbTableFromDSV(input_file = data_file, dbcon = dbcon, table_name = "DOSE",
               drop_table = TRUE, quote = "\"", na.strings = "",
               comment.char = "", fileEncoding = "UTF-8")
#> [1] 46851
```

As we can see now we got all the expected records in the SQLite table.
We can check that by listing the tables in the database, the fields in
the `DOSE` table and counting the number of records in it:

``` r
dbListTables(dbcon)
#> [1] "DOSE"

dbListFields(dbcon, "DOSE")[1:8]
#> [1] "country"    "region"     "GID_0"      "GID_1"      "F_year"    
#> [6] "grp_lcu"    "pop"        "grp_pc_lcu"

dbGetQuery(dbcon, "SELECT COUNT(*) AS n_records FROM DOSE")
#>   n_records
#> 1     46851
```

## Issues with column names

Unfortunately, when dealing with DSV files, problems are not limited to
data interpretation only. Another common source of issues are the column
names used in the input file.

While DSV files may contain column headers that are valid in their
original context (such as Excel or other applications), these names
might not conform to SQLite’s naming conventions or best practices.

SQLite has specific rules and restrictions regarding column names,
including:

- Must begin with a letter (A-Z, a-z) or an underscore (\_)
- Can contain letters, digits (0-9), and underscores (\_)
- Cannot be a reserved keyword in SQLite
- Should avoid special characters and spaces for better compatibility

You can find more details about SQLite’s naming conventions in the
[official documentation](https://www.sqlite.org/lang_keywords.html) and
[CREATE TABLE](https://www.sqlite.org/lang_createtable.html) page. There
are also some interesting discussions on Stack Overflow:

- [What are valid table names in
  SQLite?](https://stackoverflow.com/questions/3694276/what-are-valid-table-names-in-sqlite)
- [What SQLite column name can be/cannot
  be?](https://stackoverflow.com/questions/3373234/what-sqlite-column-name-can-be-cannot-be)
- [SQLite table and column name
  requirements](https://stackoverflow.com/questions/23770480/sqlite-table-and-column-name-requirements)

Some typical problems you might encounter include:

- *Special characters*: Column names containing spaces, hyphens, or
  other special characters (e.g., `"Patient-ID"`, `"Test Result"`,
  `"Cost($)"`);
- *Reserved keywords*: Column names that match SQLite reserved words
  (e.g., `"SELECT"`, `"TABLE"`, `"WHERE"`);
- *Starting with numbers*: Column names beginning with digits (e.g.,
  `"2025_Sales"`);
- *Case sensitivity*: While SQLite is case-insensitive for column names,
  mixing cases can lead to confusion;
- *Empty or duplicate names*: Missing headers or columns with identical
  names.

You should always inspect the column names in your DSV files before
importing them into a SQLite database to ensure they meet these
criteria. Failing to do so can lead to errors during the import process
or unexpected behavior when querying the database.

### How RSQLite.toolkit handles column names

The
[`dbTableFromDSV()`](https://fab-algo.github.io/RSQLite.toolkit/reference/dbTableFromDSV.md)
function[²](#fn2) offers a built-in mechanism to handle problematic
column names when importing data from DSV files. It does so through the
`id_quote_method` parameter that can be set to one of the following
values:

- `DB_NAMES` tries to build a valid SQLite column name:
  1.  substituting all characters, that are not letters or digits or the
      `_` character, with the `_` character;
  2.  prefixing `N_` to all strings starting with a digit;
  3.  prefixing `F_` to all strings equal to any SQL92 keyword.
- `SINGLE_QUOTES` encloses each string in single quotes.
- `SQL_SERVER` encloses each string in square brackets.
- `MYSQL` encloses each string in back ticks.

The default value is `DB_NAMES`.

What actually happens is that to ensure valid and unique column names,
the
[`dbTableFromDSV()`](https://fab-algo.github.io/RSQLite.toolkit/reference/dbTableFromDSV.md)
function calls the
[`file_schema_dsv()`](https://fab-algo.github.io/RSQLite.toolkit/reference/file_schema_dsv.md)
function that in turn calls the
[`format_column_names()`](https://fab-algo.github.io/RSQLite.toolkit/reference/format_column_names.md)
function, passing to it the column names read from the DSV file, the
selected `id_quote_method` and the `unique_names = TRUE` parameter, to
ensure that all column names are unique.

If you are not satisfied with the way column names are handled, you can
always manually specify the column names to be used in the SQLite table
using the `col_names` parameter of the
[`dbTableFromDSV()`](https://fab-algo.github.io/RSQLite.toolkit/reference/dbTableFromDSV.md)
function.

### Example

Let’s create a simple example to demonstrate this behavior; we will use
a DSV file with problematic column names that we will download from the
[RSQLite.toolkit-tests](https://github.com/fab-algo/RSQLite.toolkit-tests)
repository.

``` r
library("piggyback", quietly = TRUE)
pb_download(file = "Blockchain_Banking_Scopus_Dataset_2015_2025.zip",
            dest = tempdir(), repo = "fab-algo/RSQLite.toolkit-tests",
            tag = "latest")

unzip(zipfile = file.path(tempdir(),
                          "Blockchain_Banking_Scopus_Dataset_2015_2025.zip"),
      exdir = file.path(tempdir(), "Blockchain"))
dir(file.path(tempdir(), "Blockchain"))
#> [1] "Blockchain_Banking_Scopus_Dataset_2015_2025.csv"

data_file <- file.path(tempdir(),
                       "Blockchain/Blockchain_Banking_Scopus_Dataset_2015_2025.csv") # nolint
```

Now we can inspect the column names used in the input file:

``` r
f_schema2 <- file_schema_dsv(input_file = data_file,
                             quote = "\"", na.strings = "",
                             comment.char = "", fileEncoding = "UTF-8")

f_schema2$schema[1:10, ]
#>            col_names col_names_unquoted col_types sql_types         src_names
#> 1            Authors            Authors character      TEXT           Authors
#> 2  Author_full_names  Author_full_names character      TEXT Author full names
#> 3        Author_s_ID        Author_s_ID character      TEXT      Author(s) ID
#> 4              Title              Title character      TEXT             Title
#> 5             F_Year             F_Year character      TEXT              Year
#> 6       Source_title       Source_title character      TEXT      Source title
#> 7             Volume             Volume character      TEXT            Volume
#> 8              Issue              Issue character      TEXT             Issue
#> 9            Art_No_            Art_No_ character      TEXT          Art. No.
#> 10        Page_start         Page_start character      TEXT        Page start
#>    src_types src_is_quoted
#> 1       text          TRUE
#> 2       text          TRUE
#> 3       text          TRUE
#> 4       text          TRUE
#> 5       text          TRUE
#> 6       text          TRUE
#> 7       text          TRUE
#> 8       text          TRUE
#> 9       text          TRUE
#> 10      text          TRUE
```

The `src_names` column contains the original column names found in the
input file. As we can see, there are some problematic column names, such
as `Author(s) ID`, `Document Title`, `Funding Text`, `Art. No.`, etc.

The `col_names` column contains the transformed column names using the
`DB_NAMES` method (i.e., the default value for the `id_quote_method`
parameter). Comparing the two columns, we can see how some characters
have been replaced with the `_` character, spaces have been replaced
with `_`, and some names have been prefixed with `F_` to avoid conflicts
with SQLite reserved keywords (e.g., `Year`).

Now let’s see how the column names are handled when using the
`SQL_SERVER` method:

``` r
f_schema2 <- file_schema_dsv(input_file = data_file,
                             quote = "\"", na.strings = "",
                             id_quote_method = "SQL_SERVER",
                             comment.char = "", fileEncoding = "UTF-8")

f_schema2$schema[1:10, ]
#>              col_names col_names_unquoted col_types sql_types         src_names
#> 1            [Authors]            Authors character      TEXT           Authors
#> 2  [Author full names]  Author full names character      TEXT Author full names
#> 3       [Author(s) ID]       Author(s) ID character      TEXT      Author(s) ID
#> 4              [Title]              Title character      TEXT             Title
#> 5               [Year]               Year character      TEXT              Year
#> 6       [Source title]       Source title character      TEXT      Source title
#> 7             [Volume]             Volume character      TEXT            Volume
#> 8              [Issue]              Issue character      TEXT             Issue
#> 9           [Art. No.]           Art. No. character      TEXT          Art. No.
#> 10        [Page start]         Page start character      TEXT        Page start
#>    src_types src_is_quoted
#> 1       text          TRUE
#> 2       text          TRUE
#> 3       text          TRUE
#> 4       text          TRUE
#> 5       text          TRUE
#> 6       text          TRUE
#> 7       text          TRUE
#> 8       text          TRUE
#> 9       text          TRUE
#> 10      text          TRUE
```

As we can see, now the column names have been enclosed in square
brackets (`[` and `]`) to make them valid SQLite identifiers. This
quoting convention allows the use of special characters and spaces in
column names without causing issues during database operations. We can
test that by creating a new table in the database using this quoting
method:

``` r
dbTableFromDSV(input_file = data_file,
               dbcon = dbcon, table_name = "BLOCKCHAIN_BANKING",
               quote = "\"", na.strings = "",
               comment.char = "", fileEncoding = "UTF-8",
               id_quote_method = "SQL_SERVER",
               drop_table = TRUE)
#> [1] 389
```

``` r
dbListTables(dbcon)
#> [1] "BLOCKCHAIN_BANKING" "DOSE"

dbListFields(dbcon, "BLOCKCHAIN_BANKING")[1:8]
#> [1] "Authors"           "Author full names" "Author(s) ID"     
#> [4] "Title"             "Year"              "Source title"     
#> [7] "Volume"            "Issue"

dbGetQuery(dbcon, "SELECT COUNT(*) AS n_records FROM BLOCKCHAIN_BANKING")
#>   n_records
#> 1       389
```

``` r

dbDisconnect(dbcon)
```

## Conclusions

To more safely handle DSV files (and avoid unexpected results) when
importing data in a SQLite database using the
[`dbTableFromDSV()`](https://fab-algo.github.io/RSQLite.toolkit/reference/dbTableFromDSV.md)
function, you should always:

- inspect the input file to understand how data is represented; to that
  end, you can use the
  [`file_schema_dsv()`](https://fab-algo.github.io/RSQLite.toolkit/reference/file_schema_dsv.md)
  function to help you in this task and test different parameter
  settings before importing the data;
- specify the correct parameters to interpret data in the input file
  (i.e. `quote`, `na.strings`, `comment.char`, `fileEncoding`, etc.);
- check the column names used in the input file and choose the right
  strategy to handle them, here the `id_quote_method` parameter of the
  [`dbTableFromDSV()`](https://fab-algo.github.io/RSQLite.toolkit/reference/dbTableFromDSV.md)
  function can be very useful.

------------------------------------------------------------------------

1.  The original source of each dataset is described in the README page
    of the GitHub repo.

2.  The same mechanism to handle column names is used also by the
    [`dbTableFromXlsx()`](https://fab-algo.github.io/RSQLite.toolkit/reference/dbTableFromXlsx.md),
    [`dbTableFromFeather()`](https://fab-algo.github.io/RSQLite.toolkit/reference/dbTableFromFeather.md)
    and
    [`dbTableFromDataFrame()`](https://fab-algo.github.io/RSQLite.toolkit/reference/dbTableFromDataFrame.md)
    functions.
