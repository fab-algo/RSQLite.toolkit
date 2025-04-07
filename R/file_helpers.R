#' Feather_file_schema returns a data frame with the schema of a Feather file
#'
#' Preview the table structure contained in a Feather file. This function
#' inspects the input file metadata to read the field identifiers' names and
#' data types, then converts them to the candidate data frame columns' names
#' and data types. The dataset contained in the input file is not read in to
#' memory, only meta-data are accessed.
#'
#' @param input_file File name (including path) to be read
#' @param id_quote_method character, used to specify how to build the SQLite
#'    columns' names using the fields' identifiers read from the input file.
#'    For details see the description of the `quote_method` parameter of
#'    the [format_field_names()] function. Defautls to `DB_NAMES`.
#' 
#' @returns a data frame with these columns:
#'    - `col_names`: columns' names, after applying the selected quote method;
#'    - `col_types`: columns' R data types;
#'    - `sql_types`: columns' SQLite data types;
#'    - `src_names`: columns' names as they appear in the input file;
#'    - `src_types`: the Arrow's data type of each column.
#' 
#' @section References:
#' The implementation is based on this question on
#' [Stackoverflow](https://stackoverflow.com/questions/66529055/how-to-read-column-names-and-metadata-from-feather-files-in-r-arrow). 
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
               src_names,
               src_types,
               stringsAsFactors = FALSE,
               row.names=NULL)    
}


#' DSV_file_schema returns a data frame with the schema of a DSV file
#'
#' Reads only the first `max_lines` of a delimiter separated values (DSV)
#' text file to infer column names and data types, without reading the full
#' dataset into memory. Then it converts them to the candidate data
#' frame columns' names and data types.
#'
#' @param input_file character, file name (including path) to be read.
#' @param header logical, if `TRUE` the first line contains the fields'
#'    names. If `FALSE`, the column names will be formed sing a "V"
#'    followed by the column number (as specified in [utils::read.table()]).
#' @param sep character, field delimiter (e.g., "," for CSV, "\\t" for TSV)
#'    in the input file. Defaults to ",".
#' @param dec character, decimal separator (e.g., "." or "," depending on locale)
#'    in the input file. Defaults to ".".
#' @param id_quote_method character, used to specify how to build the SQLite
#'    columns' names using the fields' identifiers read from the input file.
#'    For details see the description of the `quote_method` parameter of
#'    the [format_field_names()] function. Defautls to `DB_NAMES`.
#' @param max_lines integer, number of lines (excluding the header) to be
#'    read to infer columns' data types. Defaults to 100.
#' @param ... Additional arguments passed to [utils::read.table()].
#'
#' @returns a data frame with these columns:
#'    - `col_names`: columns' names, after applying the selected quote method;
#'    - `col_types`: columns' R data types;
#'    - `sql_types`: columns' SQLite data types;
#'    - `src_names`: columns' names as they appear in the input file.
#'
#' @importFrom utils read.table
#' @export
#'
#' @examples
#' \dontrun{
#' DSV_file_schema("data.csv", sep=",", dec=".", max_lines=50)
#' DSV_file_schema("euro.csv", sep=";", dec=",", header=FALSE)
#' }
DSV_file_schema <- function(input_file,
                            header=TRUE, sep=",", dec=".",
                            id_quote_method="DB_NAMES",
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
                     ... # allow custom options like quote, colClasses, etc.
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
#' Preview the table structure contained in a rectangular range of worksheet
#' of an Excel file. 
#'
#' @param input_file character, file name (including path) to be read.
#' @param sheet_name character, the name of the worksheet containing the data
#'    table.
#' @param first_row integer, the row number where the data table starts. 
#'    If present, it is the row number of the header row, otherwise it is
#'    the row number of the first row of data. 
#' @param cols_range integer, a numeric vector specifying which columns in
#'    the worksheet to be read.
#' @param header logical, if `TRUE` the first row contains the fields'
#'    names. If `FALSE`, the column names will be the column names of the
#'    Excel worksheet (i.e. letters).
#' @param id_quote_method character, used to specify how to build the SQLite
#'    columns' names using the fields' identifiers read from the input file.
#'    For details see the description of the `quote_method` parameter of
#'    the [format_field_names()] function. Defautls to `DB_NAMES`.
#' @param max_lines integer, number of lines (excluding the header) to be
#'    read to infer columns' data types. Defaults to 100.
#' @param ...  parameters passed to [openxlsx2::wb_to_df()] function.
#' 
#' @returns a data frame with these columns:
#'    - `col_names`: columns' names, after applying the selected quote method;
#'    - `col_types`: columns' R data types;
#'    - `sql_types`: columns' SQLite data types;
#'    - `src_names`: columns' names as they appear in the input file;
#'    - `src_types`: data type attribute of each column, as determined by the
#'       [openxlsx2::wb_to_df()] function.
#' 
#' @importFrom openxlsx2 wb_to_df
#' @export
Xlsx_file_schema <- function(input_file,
                             sheet_name, first_row, cols_range, header = TRUE,
                             id_quote_method="DB_NAMES",
                             max_lines = 100, ...) {

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
        col_names,
        col_types,
        sql_types=R2SQL_types(col_types),
        src_names,
        src_types=attr(df, "types"),
        stringsAsFactors = FALSE,
        row.names=NULL
    )
    
}
