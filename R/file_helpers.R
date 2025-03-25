#' feather_file_schema returns a data frame with schema of the Feather file
#'
#' Preview the table structure contained in a Feather file. It can be used
#' to select a subset of the columns to be passed to the `dbTableFromFeather`
#' through the `columns` parameter.
#'
#' @section References:
#' The implementation is based on this question on
#' [Stackoverflow](https://stackoverflow.com/questions/66529055/how-to-read-column-names-and-metadata-from-feather-files-in-r-arrow). 
#' 
#' @param input_file File name (including path) to be read
#' @returns a data frame with the column names (`col_names`)
#'   and the Arrow data type of each column (`type_names`).
#' 
#' @import arrow
#' @export
feather_file_schema <- function(input_file) {

    rf <- arrow::ReadableFile$create(input_file)
    ft <- FeatherReader$create(rf)

    col_names <- ft$column_names
    
    ss <- lapply(ft$schema$fields, FUN=function(x) `$`(x, "type"))
    type_names <- sapply(ss, FUN=function(x) `$`(x, "name"))

    data.frame(col_names, type_names)
}


#' DSV_file_schema returns a data frame with schema of a DSV file
#'
#' Reads only the first `max_lines` of a delimiter separated values (DSV)
#' file to infer column names and data types, without reading the full
#' dataset into memory.
#'
#' @param input_file File name (including path) to be read.
#' @param max_lines Number of lines (excluding the header) to read.
#' @param sep Field delimiter (e.g., "," for CSV, "\\t" for TSV).
#' @param dec Decimal separator (e.g., "." or "," depending on locale).
#' @param ... Additional arguments passed to `read.table()`.
#'
#' @return A data frame with columns `col_names` and `data_type`.
#' @export
#'
#' @examples
#' \dontrun{
#' DSV_file_schema("data.csv", max_lines = 50, sep = ",", dec = ".")
#' DSV_file_schema("euro.csv", max_lines = 50, sep = ";", dec = ",")
#' }
DSV_file_schema <- function(input_file, max_lines = 100,
                            sep = ",", dec = ".", ...) {
  if (!file.exists(input_file)) {
    stop("DSV_file_schema: File does not exist: ", input_file)
  }

  df <- utils::read.table(
    file = input_file,
    header = TRUE,
    sep = sep,
    dec = dec,
    nrows = max_lines,
    stringsAsFactors = FALSE,
    ... # Allow custom options like quote, colClasses, etc.
  )

  data.frame(
    col_names = names(df),
    data_type = vapply(df, function(col) class(col)[1], character(1)),
    stringsAsFactors = FALSE
  )
}
