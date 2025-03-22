#' feather_schema returns a data frame with schema of the Feather file
#'
#' This helper function can be used to preview the table structure contained
#' in a Feather file in order to select a subset of the columns to be
#' passed to the `dbTableFromFeather` through the `columns` parameter.
#'
#' @section References:
#' The implementation is based on this questioon on [Stackoverflow](https://stackoverflow.com/questions/66529055/how-to-read-column-names-and-metadata-from-feather-files-in-r-arrow). 
#' 
#' @param input.file the file name (including path) to be read
#' @returns a data frame with the column names (`col_names`)
#'   and the Arrow data type of each column (`type_names`).
#' 
#' @import arrow
#' @export
feather_schema <- function(input.file) {

    rf <- arrow::ReadableFile$create(input.file)
    ft <- FeatherReader$create(rf)

    col_names <- ft$column_names
    
    ss <- lapply(ft$schema$fields, FUN=function(x) `$`(x, "type"))
    type_names <- sapply(ss, FUN=function(x) `$`(x, "name"))

    data.frame(col_names, type_names)
}

