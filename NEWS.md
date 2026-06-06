# RSQLite.toolkit 0.1.3

* Fixed bug in `file_schema_dsv()` and `dbTableFromDSV()` when \x1A character is present in the DSV file.
* Fixed bug when `dbTableFromDSV()` is called with `skip > 0` among the parameters passed to `scan`.
* Added support for reading a group of lines of length `nlines` from the DSV file by `chunk_size` blocks.
* It is now possible to combine the `skip`, `nlines` and `chunk_size` parameters to read a specific portion of the DSV file.
* Added support for automatic conversion of percentage numbers (i.e. columns ending with `%`) in DSV files.
* Minor improvements to documentation.

# RSQLite.toolkit 0.1.2

* Fixed bug in `dbExecFile()`: it now supports quoting marks inside quoted strings.

# RSQLite.toolkit 0.1.1

* Initial release.
