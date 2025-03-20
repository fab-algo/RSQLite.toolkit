#' feather_schema returns a data frame with schema of the file
#'
#' @section References:
#' [Stackoverflow](https://stackoverflow.com/questions/66529055/how-to-read-column-names-and-metadata-from-feather-files-in-r-arrow)
#' 
#' @param input.file the file name (including path) to be read
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

