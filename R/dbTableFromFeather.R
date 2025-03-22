#' dbTableFromFeather create a table in SQLite database from a
#'   Feather file
#'
#' The dbTableFromFeather function reads the data from a Apache
#' Arrow table serialized in a Feather (Arrow IPC) file and copies it
#' to a table in a SQLite database. If table does not exist, it will
#' create it.
#'
#' @param input_file the file name (including path) to be read.
#' @param dbcon database connection, as created by the dbConnect function.
#' @param table_name string, the name of the table.
#' @param columns character vector, names of the columuns to be imported.
#'    If `NULL` all columns will be imported in the db table. Defaults to `NULL`.
#' @param drop_table logical, if `TRUE` the target table will be dropped (if exists)
#'    and recreated before importing the data. Defaults to `FALSE`.
#' @param auto_pk ...
#' @param pk_fields ...
#' @param build_pk ...
#' @param constant_values ...
#' @returns nothing
#'
#' @import RSQLite
#' @importFrom arrow read_feather
#' @export
dbTableFromFeather <- function(input_file, dbcon, table_name,
                            columns=NULL, drop_table = FALSE,
                            auto_pk = FALSE, pk_fields = NULL, build_pk = FALSE,
                            constant_values = NULL) {

    ## formal checks on parameters ................
    if (!is.list(constant_values) && !is.null(constant_values)) {
        stop("dbTableFromFeather: error in 'constant.values' param.: must be a list.")
    }

    if (is.list(constant_values) && length(constant_values) == 0) {
        stop("dbTableFromFeather: 'constant.values' list must not be of zero length.")
    }


    ## read data ..................................
    df <- arrow::read_feather(
        file = input_file,
        col_select  = columns,
        as_data_frame = TRUE,
        mmap = TRUE
    )


    cclass <- sapply(df, typeof)
    fields <- sapply(sapply(df, typeof), SQLtype)
    cnames1 <- names(df)
	cnames <- paste("[",cnames1,"]", sep="")
    
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
            } else {
                fld.type <- "TEXT"
            }

            sql.body <- paste(sql.body, ", ", fld.name, " ", fld.type, sep = "")

            cv_names <- c(cv_names, fld.name)
            cv_types <- c(cv_types, fld.type)
        }
        cnames1 <- c(cnames1, cv_names)
    }

    if (auto_pk) {
        sql.body <- paste(sql.body, ", SEQ INTEGER PRIMARY KEY", sep = "")
        cnames1 <- c(cnames1, "SEQ")
    }

    sql.tail <- ");"
    sql.def <- paste(sql.head, sql.body, sql.tail, sep = " ")

    dbExecute(dbcon, sql.def)

    # Write data ................................
    if (!is.null(constant_values)) {
        df <- cbind(df, constant_values)
    }

    if (auto_pk) {
        df <- cbind(df, NA)
    }

    names(df) <- cnames1
    
    dbWriteTable(dbcon, table_name, as.data.frame(df), row.names = FALSE, append = TRUE)


    ## Indexing -------------------------------
    if (drop_table && !auto_pk && !is.null(pk_fields) && build_pk) {
        
		if (!is.character(pk_fields)) {
            stop("dbTableFromFeather: 'pk.fields' must be a character vector.")
        }

        check_fields <- setdiff(pk_fields, cnames)
        if (length(check_fields) > 0) {
            stop("dbTableFromFeather: 'pk.fields' contains unknown field names: ",
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
