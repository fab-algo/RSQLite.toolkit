#' dbTableFromDSV  create a table in SQLite database from a
#'   delimiter separated values (DSV) file
#'
#' The dbTableFromDSV function reads the data from a DSV file
#' and copies it to a table in a SQLite database. If table does
#' not exist, it will create it.
#'
#' 
#' @param input_file the file name (including path) to be read.
#' @param dbcon database connection, as created by the dbConnect function.
#' @param table_name string, the name of the table.
#'
#' @param header ...
#' @param sep ...
#' @param dec ...
#'
#' @param id_quote_method character vector, used to specify how to build the SQLite
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
#' @param drop_table logical, if `TRUE` the target table will be dropped (if exists)
#'    and recreated before importing the data. Defaults to `FALSE`.
#' @param auto_pk ...
#' @param pk_fields ...
#' @param build_pk ...
#' @param constant_values ...
#' 
#' @param chunk_size ...
#' @param ... ...
#' 
#' @returns nothing
#'
#' @import RSQLite
#' @importFrom utils read.table
#' @export
dbTableFromDSV <- function(input_file, dbcon, table_name,
                           header=TRUE, sep=",", dec=".",
                           id_quote_method="DB_NAMES",
                           col_names=NULL, col_types=NULL,
                           drop_table=FALSE,
                           auto_pk=FALSE, pk_fields=NULL, build_pk=FALSE,
                           constant_values=NULL, chunk_size=10000, ...) {

    ## formal checks on parameters ................
    if (!is.list(constant_values) && !is.null(constant_values)) {
        stop("dbTableFromDSV: error in 'constant.values' param.: must be a list.")
    }

    if (is.list(constant_values) && length(constant_values) == 0) {
        stop("dbTableFromDSV: 'constant.values' list must not be of zero length.")
    }

    ## read schema ................................
    df.scm <- DSV_file_schema(input_file, id_quote_method=id_quote_method,
                              header=header, sep=sep, dec=dec,
                              max_lines = 200)

    cnames <- df.scm$col_names
    cclass <- df.scm$col_types
    fields <- df.scm$sql_types

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

    if (autoPK) {
        sql.body <- paste(sql.body, ", SEQ INTEGER PRIMARY KEY", sep = "")
		cnames2 <- c(cnames1, "SEQ")
    } else {
        cnames2 <- cnames1
    }

    sql.tail <- ");"
    sql.def <- paste(sql.head, sql.body, sql.tail, sep = " ")

    dbExecute(dbcon, sql.def)


    ## read & write data ..................................
    lclass <- list()
    for (ii in seq_along(cclass)) {
        if (cclass[ii] == "Date") {
            lclass[[ii]] <- vector("character", 0)
        } else {
            lclass[[ii]] <- vector(cclass[ii], 0)
        }
    }
	
    fcon <- file(input_file, "r", blocking = FALSE)

    if (header) {
        scan(file = fcon, what = character(), nlines = 1)
    }

    nread <- 0
    repeat {
        dfbuffer <- scan(
            file = fcon, sep=sep, dec=dec,
            what = lclass,
            nlines = chunk_size,
            strip.white = TRUE, flush = TRUE, fill = TRUE,
            multi.line = FALSE, quiet = TRUE, ...
        )

        if (length(dfbuffer[[1]]) == 0) break

        dfbuffer <- as.data.frame(dfbuffer,
                                  row.names = NULL, stringAsFactors = FALSE)

        if (!is.null(constant_values)) {
            dfbuffer <- cbind(dfbuffer, constant_values)
            names(dfbuffer) <- cnames1
        } else {
            names(dfbuffer) <- cnames
        }

        dfbuffer[, which(cclass == "Date")] <-
            format(dfbuffer[, which(cclass == "Date")], format = "%Y-%m-%d")

        if (autoPK) {
            dfbuffer <- cbind(dfbuffer, NA)
        }

        dbWriteTable(dbcon, table_name, dfbuffer, row.names = FALSE, append = TRUE)
        nread <- nread + chunk_size
    }

    close(fcon)

    ## Indexing -------------------------------
    if (drop_table && !auto_pk &&  !is.null(pk_fields) && build_pk) {
        
		if (!is.character(pk_fields)) {
            stop("dbTableFromDSV: 'pk.fields' must be a character vector.")
        }

        check_fields <- setdiff(pk_fields, cnames1)
        if (length(check_fields) > 0) {
            stop("dbTableFromDSV: 'pk.fields' contains unknown field names: ",
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
