#' Feather_file_schema returns a data frame with the schema of a Feather file
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
#' @param id_quote_method Character, the quote method to be use to
#'   form the column names from the field names read from the input
#'   file. 
#' 
#' @returns a data frame with the column names (`col_names`)
#'   and the Arrow's data type of each column (`src_types`).
#' 
#' @import arrow
#' @export
Feather_file_schema <- function(input_file, id_quote_method="DB_NAMES") {

    if (!file.exists(input_file)) {
        stop("Feather_file_schema: File does not exist: ", input_file)
    }
    
    rf <- arrow::ReadableFile$create(input_file)
    ft <- FeatherReader$create(rf)

    src_names <- ft$column_names

    col_names <- format_field_names(x=src_names, quote_method=id_quote_method,
                                    unique_names=TRUE)
    
    ss <- lapply(ft$schema$fields, FUN=function(x) `$`(x, "type"))
    src_types <- sapply(ss, FUN=function(x) `$`(x, "name"))

    rf$close()

    data.frame(col_names,
               col_types=Arrow2R_types(src_types),
               sql_types=R2SQL_types(Arrow2R_types(src_types)),
               src_names, src_types,
               row.names=NULL)    
}


#' DSV_file_schema returns a data frame with the schema of a DSV file
#'
#' Reads only the first `max_lines` of a delimiter separated values (DSV)
#' file to infer column names and data types, without reading the full
#' dataset into memory.
#'
#' The returned data frame consists of these columns:
#' - `col_names`: input file's field names that have been 
#' - `col_types`: R data type
#' - `src_names`:
#'
#' @param input_file File name (including path) to be read.
#' @param id_quote_method Character, the quote method to be use to
#'   form the column names from the field names read from the input
#'   file. 
#' @param header Logical, if `TRUE` the first line contains the fields'
#'    names. If `FALSE`, the column names will be formed  using a "V"
#'    followed by the column number (as specified in `read.table`
#' @param sep ...
#'    function).#' @param sep Field delimiter (e.g., "," for CSV, "\\t" for TSV).
#' @param dec Decimal separator (e.g., "." or "," depending on locale).
#' @param max_lines Number of lines (excluding the header) to read.
#'    Defaults to 100.
#' @param ... Additional arguments passed to `read.table()`.
#'
#' @return A data frame with columns `col_names`, `col_types` and
#'    `src_names`.
#' 
#' @importFrom RSQLite dbQuoteIdentifier
#' @export
#'
#' @examples
#' \dontrun{
#' DSV_file_schema("data.csv", sep=",", dec=".", max_lines=50)
#' DSV_file_schema("euro.csv", sep=";", dec=",", header=FALSE)
#' }
DSV_file_schema <- function(input_file, id_quote_method="DB_NAMES",
                            header=TRUE, sep=",", dec=".",
                            max_lines = 100, ...) {
    if (!file.exists(input_file)) {
        stop("DSV_file_schema: File does not exist: ", input_file)
    }

    df <- utils::read.table(
                     file = input_file,
                     header = header,
                     sep = sep,
                     dec = dec,
                     nrows = max_lines,
                     stringsAsFactors = FALSE,
                     ... # Allow custom options like quote, colClasses, etc.
                 )
    
    src_names <- names(df)
    col_types <- vapply(df, function(col) class(col)[1], character(1))
    
    col_names <- format_field_names(x=src_names, quote_method=id_quote_method,
                                    unique_names=TRUE)

    data.frame(
        col_names, col_types,
        sql_types=R2SQL_types(col_types),
        src_names,
        stringsAsFactors = FALSE,
        row.names=NULL
    )
}



#' Xlsx_file_schema returns a data frame with the schema of an Excel data table
#'
#' Preview the table structure contained in a rectangula range of worksheet
#' of an Excel file. It can be used to select a subset of the columns to be
#' passed to the `dbTableFromXlsx` through the `col_names` parameter.
#'
#' 
#' @param input_file File name (including path) to be read
#' @param id_quote_method Character, the quote method to be use to
#'   form the column names from the field names read from the input
#'   file.
#' @param header ...
#' @param sheet_name ...
#' @param first_row ...
#' @param cols_range ...
#' @param max_lines ...
#' @param ...  parameters passed to openxlsx2::wb_to_df
#' 
#' @returns a data frame with the column names (`col_names`)
#'   and the Arrow's data type of each column (`src_types`).
#' 
#' @importFrom openxlsx2 wb_to_df
#' @export
Xlsx_file_schema <- function(input_file, id_quote_method="DB_NAMES",
                             header = TRUE, sheet_name, first_row,
                             cols_range, max_lines = 100, ...) {

    if (!file.exists(input_file)) {
        stop("Xlsx_file_schema: File does not exist: ", input_file)
    }
    
    df <- openxlsx2::wb_to_df(
        file = input_file,
        sheet = sheet_name,
        start_row = first_row,
        col_names = header,
        cols = cols_range,
        rows = c(first_row:(first_row+max_lines)),
        skip_empty_rows = FALSE,
        skip_empty_cols = FALSE,
        check_names = TRUE,
        keep_attributes = TRUE,
        ...
        )
    
    src_names <- names(df)
    col_types <- vapply(df, function(col) class(col)[1], character(1))
    
    col_names <- format_field_names(x=src_names, quote_method=id_quote_method,
                                    unique_names=TRUE)
    
    data.frame(
        col_names, col_types,
        sql_types=R2SQL_types(col_types),
        src_names,
        src_types=attr(df, "types"),
        stringsAsFactors = FALSE,
        row.names=NULL
    )
    
}
