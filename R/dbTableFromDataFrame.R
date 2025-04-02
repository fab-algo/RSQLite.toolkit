#' The dbTableFromDataFrame function reads the data from a rectangula region
#' of a sheet in an Excel file and copies it to a table in a SQLite
#' database. If table does not exist, it will create it.
#'
#' @param df the data frame to be saved in SQLite table
#' @param dbcon ...
#' @param table_name ...
#'
#' @param id_quote_method character vector,  used to specify how to build the SQLite
#'    table's field names from the column identifiers read from `input_file`.
#'    It can assume the following values:
#'    - `DB_NAMES` tries to build a valid field name:
#'      a. substituting all characters, that are not letters or digits or
#'         the `_` character, with the `_` character;
#'      b. prefixing `N_` to all strings starting with a digit;
#'      c. prefixing `F_` to all strings equal to a SQL92 keyword.
#'    - `SINGLE_QUOTES` encloses each string in single quotes.
#'    - `DOUBLE_QUOTES` encloses each string in double quotes.
#'    - `SQL_SERVER` encloses each string in square brackets.
#'    - `MYSQL` encloses each string in back ticks.
#'    Defaults to `DB_NAMES`.
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
#' @param drop_table ...
#' @param auto_pk ...
#' @param pk_fields ...
#' @param build_pk ...
#' @param constant_values ...
#'
#' @returns a data frame with the structure of the table created in the
#'          database
#'
#' @import RSQLite
#' @export
dbTableFromDataFrame <- function(df, dbcon, table_name,
                                 id_quote_method="DB_NAMES",
                                 col_names=NULL, col_types=NULL,
                                 drop_table=FALSE,
                                 auto_pk=FALSE, pk_fields=NULL, build_pk=FALSE,
                                 constant_values=NULL) {

    ## formal checks on parameters ................
    if (!is.list(constant_values) && !is.null(constant_values)) {
        stop("dbTableFromDataFrame: error in 'constant.values' param.: ",
             "must be a list.")
    }

    if (is.list(constant_values) && length(constant_values) == 0) {
        stop("dbTableFromDataFrame: 'constant.values' list must not ",
             "be of zero length.")
    }

    ## read schema ................................
    src_names <- names(df)

    cnames <- format_field_names(src_names, quote_method=id_quote_method)
    cclass <- vapply(df, function(col) class(col)[1], character(1))
    fields <- R2SQL_types(cclass)

    if (!is.null(col_names)) {
        if (length(col_names)!=length(cnames)) {
            stop("dbTableFromFeather: wrong 'col_names' length, must be ",
                 length(cnames), " elements but found ", length(col_names))
        }
        cnames <- col_names
    }

    if (!is.null(col_types)) {
        if (length(col_types)!=length(cclass)) {
            stop("dbTableFromFeather: wrong 'col_types' length, must be ",
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

    autoPK <- FALSE
    if (length(pk_fields) == 0 && auto_pk) autoPK <- TRUE

    sql.head <-
        paste("CREATE TABLE IF NOT EXISTS ", table_name, " (", sep = "")
    sql.body <- paste(cnames, fields, sep = " ", collapse = ", ")

    cv_class <- c()
    cv_names <- c()
    cv_types <- c()

    if (!is.null(constant_values)) {
        for (ii in seq_along(constant_values)) {
            if (is.null(names(constant_values)[ii])) {
                fld.name <- paste("V", ii, sep = "")
            } else {
                fld.name <- names(constant_values)[ii]
            }

            if (data.class(constant_values[[ii]]) == "character") {
                fld.type <- "TEXT"
            } else if (data.class(constant_values[[ii]]) == "numeric") {
                fld.type <- "REAL"
            } else if (data.class(constant_values[[ii]]) == "Date") {
                fld.type <- "DATE"
            } else if (data.class(constant_values[[ii]]) == "integer") {
                fld.type <- "INTEGER"
            } else if (data.class(constant_values[[ii]]) == "logical") {
                fld.type <- "INTEGER"
            } else {
                fld.type <- "TEXT"
            }

            sql.body <- paste(sql.body, ", ", fld.name, " ", fld.type, sep = "")

            cv_names <- c(cv_names, fld.name)
            cv_types <- c(cv_types, fld.type)
        }
        cnames1 <- c(cnames, cv_names)
    } else {
        cnames1 <- cnames
    }

    if (auto_pk) {
        sql.body <- paste(sql.body, ", SEQ INTEGER PRIMARY KEY", sep = "")
        cnames2 <- c(cnames1, "SEQ")
    } else {
        cnames2 <- cnames1
    }

    sql.tail <- ");"
    sql.def <- paste(sql.head, sql.body, sql.tail, sep = " ")

    dbExecute(dbcon, sql.def)

    
    ## Write data -------------------------------
    if (!is.null(constant_values)) {
        df <- cbind(df, constant_values)
        names(df) <- cnames1
    } else {
        names(df) <- cnames
    }

    if (auto_pk) {
        df <- cbind(df, NA)
    }
    
    dbWriteTable(dbcon, table_name, as.data.frame(df), row.names = FALSE, append = TRUE)


    ## Indexing -------------------------------
    if (drop_table && !auto_pk && !is.null(pk_fields) && build_pk) {
        
        if (!is.character(pk_fields)) {
            stop("dbCreateTableFromDF: 'pk.fields' must be a character vector.")
        }

        check_fields <- setdiff(pk_fields, cnames)
        if (length(check_fields) > 0) {
            stop("dbCreateTableFromDF: 'pk.fields' contains unknown field names: ",
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
