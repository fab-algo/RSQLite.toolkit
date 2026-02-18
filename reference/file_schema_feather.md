# Preview the table structure contained in a Feather file.

The `file_schema_feather()` function returns a data frame with the
schema of a Feather file. This function is used to preview the table
structure contained in a Feather file, by reading only the metadata of
the file. It inspects the input file metadata to read the field
identifiers' names and data types, then converts them to the candidate
data frame columns' names and data types. The dataset contained in the
input file is not read in to memory, only meta-data are accessed.

## Usage

``` r
file_schema_feather(input_file, id_quote_method = "DB_NAMES")
```

## Arguments

- input_file:

  File name (including path) to be read

- id_quote_method:

  character, used to specify how to build the SQLite columns' names
  using the fields' identifiers read from the input file. For details
  see the description of the `quote_method` parameter of the
  [`format_column_names()`](https://fab-algo.github.io/RSQLite.toolkit/reference/format_column_names.md)
  function. Defaults to `DB_NAMES`.

## Value

a data frame with these columns:

- `col_names`: columns' names, after applying the selected quote method;

- `col_names_unquoted`: columns' names, unquoted; if `id_quote_method`
  is set to `DB_NAMES` they will be the same as `col_names`; for other
  quote methods they will be the unquoted versions of `col_names`, that
  is generally the same as `src_names` unless `src_names` contain the
  quoting characters;

- `col_types`: columns' R data types;

- `sql_types`: columns' SQLite data types;

- `src_names`: columns' names as they appear in the input file;

- `src_types`: the Arrow's data type of each column.

## References

The implementation is based on this question on
[Stackoverflow](https://stackoverflow.com/questions/66529055/how-to-read-column-names-and-metadata-from-feather-files-in-r-arrow).
\# nolint: line_length_linter.
