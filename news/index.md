# Changelog

## RSQLite.toolkit 0.1.3

- Fixed bug in
  [`file_schema_dsv()`](https://fab-algo.github.io/RSQLite.toolkit/reference/file_schema_dsv.md)
  and
  [`dbTableFromDSV()`](https://fab-algo.github.io/RSQLite.toolkit/reference/dbTableFromDSV.md)
  when 1A character is present in the DSV file.
- Fixed bug when
  [`dbTableFromDSV()`](https://fab-algo.github.io/RSQLite.toolkit/reference/dbTableFromDSV.md)
  is called with `skip > 0` among the parameters passed to `scan`.
- Added support for reading a group of lines of length `nlines` from the
  DSV file by `chunk_size` blocks.
- It is now possible to combine the `skip`, `nlines` and `chunk_size`
  parameters to read a specific portion of the DSV file.
- Added support for automatic conversion of percentage numbers
  (i.e. columns ending with `%`) in DSV files.
- In function
  [`file_schema_dsv()`](https://fab-algo.github.io/RSQLite.toolkit/reference/file_schema_dsv.md)
  added the `all_na` column to the schema of the DSV file, which
  indicates whether all values in a column are NA.
- Minor improvements to documentation.

## RSQLite.toolkit 0.1.2

CRAN release: 2026-04-04

- Fixed bug in
  [`dbExecFile()`](https://fab-algo.github.io/RSQLite.toolkit/reference/dbExecFile.md):
  it now supports quoting marks inside quoted strings.

## RSQLite.toolkit 0.1.1

CRAN release: 2026-03-05

- Initial release.
