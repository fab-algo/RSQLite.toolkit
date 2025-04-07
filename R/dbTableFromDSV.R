#' dbTableFromDSV create a table in a SQLite database from a
#'   delimiter separated values (DSV) text file
#'
#' The dbTableFromDSV function reads the data from a DSV file
#' and copies it to a table in a SQLite database. If table does
#' not exist, it will create it.
#'
#' @param input_file character, the file name (including path) to be read.
#' @param dbcon database connection, as created by the dbConnect function.
#' @param table_name character, the name of the table.
#'
#' @param header logical, if `TRUE` the first line contains the columns'
#'    names. If `FALSE`, the columns' names will be formed sing a "V"
#'    followed by the column number (as specified in [utils::read.table()]).
#' @param sep character, field delimiter (e.g., "," for CSV, "\\t" for TSV)
#'    in the input file. Defaults to ",".
#' @param dec character, decimal separator (e.g., "." or "," depending on locale)
#'    in the input file. Defaults to ".".
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
#'    define the `UNIQUE INDEX`. Defaults to `NULL`.
#'
#' @param constant_values a one row data frame whose columns will be added to
#'    the table in the database. The additional table columns will be named
#'    as the data frame columns, and the corresponding values will be associeted
#'    to each record imported from the input file. It is useful to keep
#'    track of additional information (e.g., the input file name, additional
#'    context data not available in the data set, ...) when loading
#'    the content of multiple input files in the same table.
#' 
#' @param chunk_size integer, the number of lines in each "chunk" (i.e. block
#'    of lines from the input file). Setting its value to a positive integer
#'    number, will process the input file by blocks of `chunck_size` lines,
#'    avoiding to read all the data in memory at once. It can be useful
#'    for very large size files. If set to zero, it will process the whole
#'    text file in one pass. Default to zero.
#' @param ... additional arguments passed to [base::scan()] function used
#'    to read input data.
#'
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
                           auto_pk=FALSE, build_pk=FALSE, pk_fields=NULL,
                           constant_values=NULL, chunk_size=0, ...) {

    ## formal checks on parameters ................
    if (!is.list(constant_values) && !is.null(constant_values)) {
        stop("dbTableFromDSV: error in 'constant_values' param.: must be a list.")
    }

    if (is.list(constant_values) && length(constant_values) == 0) {
        stop("dbTableFromDSV: 'constant_values' list must not be of zero length.")
    }

    ## read schema ................................
    df.scm <- DSV_file_schema(input_file, 
                              header=header, sep=sep, dec=dec,
                              id_quote_method=id_quote_method,
                              max_lines = 200)

    cnames <- df.scm$col_names
    cclass <- df.scm$col_types
    fields <- df.scm$sql_types

    if (!is.null(col_names)) {
        if (length(col_names)!=length(cnames)) {
            stop("dbTableFromDSV: wrong 'col_names' length, must be ",
                 length(cnames), " elements but found ", length(col_names))
        }
        cnames <- col_names
    }

    if (!is.null(col_types)) {
        if (length(col_types)!=length(cclass)) {
            stop("dbTableFromDSV: wrong 'col_types' length, must be ",
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

        ## Write data ...............................
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

        dbWriteTable(dbcon, table_name, as.data.frame(dfbuffer),
                     row.names = FALSE, append = TRUE)
        nread <- nread + chunk_size
    }

    close(fcon)

    ## Indexing -------------------------------
    if (!is.null(pk_fields) && build_pk) {
        
        if (!is.character(pk_fields)) {
            stop("dbTableFromDSV: 'pk_fields' must be a character vector.")
        }

        check_fields <- setdiff(pk_fields, cnames1)
        if (length(check_fields) > 0) {
            stop("dbTableFromDSV: 'pk_fields' contains unknown field names: ",
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
