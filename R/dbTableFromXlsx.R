#' dbTableFromXlsx create a table in a SQLite database from a
#'   range of an Excel worksheet
#'
#' The dbTableFromXlsx function reads the data from a range of
#' an Excel worksheet. If table does not exist, it will
#' create it.
#'
#' @param input_file character, the file name (including path) to be read.
#' @param dbcon database connection, as created by the dbConnect function.
#' @param table_name character, the name of the table.
#' 
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
#' 
#' @param id_quote_method character, used to specify how to build the SQLite
#'    columns' names using the fields' identifiers read from the input file.
#'    For details see the description of the `quote_method` parameter of
#'    the [format_field_names()] function. Defautls to `DB_NAMES`.
#' @param col_names character vector, names of the columuns to be imported.
#'    Used to override the field names derived from the input file (using the
#'    quote method selected by `id_quote_method`). Must be of the same length
#'    of the number of columns in the input file. If `NULL` the column names
#'    coming from the input file (after quoting) will be used. Defaults to `NULL`.
#' @param col_types character vector of classes to be assumed for the columns.
#'    If not null, it will override the data types inferred from the input file.
#'    Must be of the same length of the number of columns in the input file.
#'    If `NULL` the data type inferred from the input files will be used.
#'    Defaults to `NULL`.
#' 
#' @param drop_table logical, if `TRUE` the target table will be dropped (if exists)
#'    and recreated before importing the data.  if `FALSE`, data from input
#'    file will be appended to an existing table. Defaults to `FALSE`.
#' @param auto_pk logical, if `TRUE`, and `pk_fields` parameter is `NULL`, an
#'    additional column named `SEQ` will be added to the table and it will be
#'    defined to be `INTEGER PRIMARY KEY` (i.e. in effect an alias for
#'    `ROWID`). Defaults to `FALSE`.
#' @param build_pk logical, if `TRUE` creates a `UNIQUE INDEX` named
#'    `<table_name>_PK` defined by the combination of fields specified
#'    in the `pk_fields` parameter. It will be effective only if
#'    `pk_fields` is not null. Defaults to `FALSE`.
#' @param pk_fields character vector, the list of the fields' names that
#'    define the `UNIQUE INDEX`. Defults to `NULL`.
#'
#' @param constant_values a one row data frame whose columns will be added to
#'    the table in the database. The additional table columns will be named
#'    as the data frame columns, and the corresponding values will be associeted
#'    to each record imported from the input file. It is useful to keep
#'    track of additional information (e.g., the input file name, additional
#'    context data not available in the data set, ...) when loading
#'    the content of multiple input files in the same table. Defults to `NULL`.
#' 
#' @param ... additional arguments passed to [openxlsx2::wb_to_df()] function used
#'    to read input data.
#' 
#' @returns nothing
#'
#' @import RSQLite
#' @importFrom openxlsx2 wb_to_df
#' @export
dbTableFromXlsx <- function(input_file, dbcon, table_name,
                            sheet_name, first_row, cols_range, header=TRUE,
                            id_quote_method="DB_NAMES",
                            col_names=NULL, col_types=NULL,
                            drop_table=FALSE,
                            auto_pk=FALSE, build_pk=FALSE, pk_fields=NULL,
                            constant_values=NULL, ...) {

    ## formal checks on parameters ................
    if (!("data.frame" %in% class(constant_values)) && !is.null(constant_values)) {
        stop("dbTableFromXlsx: 'constant_values' must be a data frame.")
    }

    if (("data.frame" %in% class(constant_values)) && length(constant_values) == 0) {
        stop("dbTableFromXlsx: 'constant_values' must not be of zero length.")
    }

    if (("data.frame" %in% class(constant_values)) && dim(constant_values)[1]>1) {
        stop("dbTableFromXlsx: 'constant_values' must have one row.")
    }

    ## read schema ................................
    df.scm <- Xlsx_file_schema(input_file, sheet_name=sheet_name,
                               first_row=first_row, cols_range=cols_range,
                               header=header, 
                               id_quote_method=id_quote_method)

    cnames <- df.scm$col_names
    cclass <- df.scm$col_types
    fields <- df.scm$sql_types

    if (!is.null(col_names)) {
        if (length(col_names)!=length(cnames)) {
            stop("dbTableFromXlsx: wrong 'col_names' length, must be ",
                 length(cnames), " elements but found ", length(col_names))
        }
        cnames <- col_names
    }

    if (!is.null(col_types)) {
        if (length(col_types)!=length(cclass)) {
            stop("dbTableFromXlsx: wrong 'col_types' length, must be ",
                 length(cclass), " elements but found ", length(col_types))
        }
        cclass <- col_types
        fields <- R2SQL_types(col_types)
    }
    
	
    ## create empty table .........................
	if (drop_table) {
        sql.def <- paste("DROP TABLE IF EXISTS ", table_name, ";", sep = "")
        dbExecute(dbcon, sql.def)
    }

    sql.head <-
        paste("CREATE TABLE IF NOT EXISTS ", table_name, " (", sep = "")
    sql.body <- paste(cnames, fields, sep = " ", collapse = ", ")

    cv_names <- c()
    cv_types <- c()

    if (!is.null(constant_values)) {
        src_names <- names(constant_values)
        cv_names <- format_field_names(x=src_names, quote_method=id_quote_method,
                                       unique_names=TRUE)
        cv_types <- vapply(constant_values, function(col) class(col)[1], character(1))
        
        sql.ext <- paste(cv_names, cv_types, sep = " ", collapse = ", ")
        sql.body <- paste(sql.body, ", ", sql.ext, sep = "")

        names(constant_values) <- cv_names
        
        cnames1 <- c(cnames, cv_names)
    } else {
        cnames1 <- cnames
    }

    autoPK <- FALSE
    if (length(pk_fields) == 0 && auto_pk) autoPK <- TRUE

    if (autoPK) {
        sql.body <- paste(sql.body, ", SEQ INTEGER PRIMARY KEY", sep = "")
        cnames2 <- c(cnames1, "SEQ")
    } else {
        cnames2 <- cnames1
    }

    sql.tail <- ");"
    sql.def <- paste(sql.head, sql.body, sql.tail, sep = " ")

    dbExecute(dbcon, sql.def)

    
    ## read data ..................................
    df <- openxlsx2::wb_to_df(
        file = input_file,
        sheet = sheet_name,
        start_row = first_row,
        col_names = header,
        cols = cols_range,
        skip_empty_rows = FALSE,
        skip_empty_cols = FALSE,
        check_names = TRUE,
        ...
        )

    
    # Write data ................................
    if (!is.null(constant_values)) {
        df <- cbind(df, constant_values)
        names(df) <- cnames1
    } else {
        names(df) <- cnames
    }

    df[, which(cclass == "Date")] <-
        as.data.frame(apply(df[, which(cclass == "Date")], 2,
                            function(x) format(x,format = "%Y-%m-%d"))
                      )

    if (autoPK) {
        df <- cbind(df, NA)
        names(df) <- cnames2
    }

    dbWriteTable(dbcon, table_name, as.data.frame(df),
                 row.names = FALSE, append = TRUE)


    ## Indexing -------------------------------
    if (!is.null(pk_fields) && build_pk) {
        
        if (!is.character(pk_fields)) {
            stop("dbTableFromXlsx: 'pk_fields' must be a character vector.")
        }

        check_fields <- setdiff(pk_fields, cnames1)
        if (length(check_fields) > 0) {
            stop("dbTableFromXlsx: 'pk_fields' contains unknown field names: ",
                 check_fields)
        }

        dbExecute(dbcon, paste(
            "CREATE UNIQUE INDEX ", paste(table_name, "_PK", sep = ""),
            "ON ", table_name, " (", paste(pk_fields, collapse = ", "),
            ");",
            sep = " "
        ))
    }
}
