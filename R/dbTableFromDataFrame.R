#' The dbTableFromDataFrame function reads the data from a rectangula region
#' of a sheet in an Excel file and copies it to a table in a SQLite
#' database. If table does not exist, it will create it.
#'
#' @param df the data frame to be saved in SQLite table
#' @param dbcon ...
#' @param table_name ...
#' @param drop_table ...
#' @param auto_pk ...
#' @param pk_fields ...
#' @param build_pk ...
#' @param constant_values ...
#' @returns a data frame with the structure of the table created in the
#'          database
#'
#' @import RSQLite
#' @export
dbTableFromDataFrame <- function(df, dbcon, table_name, drop_table = FALSE,
                                 auto_pk = FALSE, pk_fields = NULL, build_pk = FALSE,
                                 constant_values = NULL) {

    ## formal checks on parameters ................
    if (!is.list(constant_values) && !is.null(constant_values)) {
        stop("dbTableFromDataFrame: error in 'constant.values' param.: ",
             "must be a list.")
    }

    if (is.list(constant_values) && length(constant_values) == 0) {
        stop("dbTableFromDataFrame: 'constant.values' list must not ",
             "be of zero length.")
    }

    cclass <- sapply(df, typeof)
    fields <- R2SQL_types(vapply(df, function(col) class(col)[1], character(1)))
    cnames <- names(df)


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
            } else {
                fld.type <- "TEXT"
            }

            sql.body <- paste(sql.body, ", ", fld.name, " ", fld.type, sep = "")

            cv_class <- data.class(constant_values[[ii]])
            cv_names <- c(cv_names, fld.name)
            cv_types <- c(cv_types, fld.type)
        }
		cclass <- c(cclass, cv_class)
        cnames <- c(cnames, cv_names)
		fields <- c(fields, cv_types)
    }

    if (auto_pk) {
        sql.body <- paste(sql.body, ", SEQ INTEGER PRIMARY KEY", sep = "")
        cnames <- c(cnames, "SEQ")
		cclass <- c(class, "integer")
		fields <- c(fields, "INTEGER")
    }

    sql.tail <- ");"
    sql.def <- paste(sql.head, sql.body, sql.tail, sep = " ")

    dbExecute(dbcon, sql.def)

    ## Save data -------------------------------
    if (!is.null(constant_values)) {
        df <- cbind(df, constant_values)
    }

    if (auto_pk) {
        df <- cbind(df, NA)
    }

    names(df) <- cnames    
    
    dbWriteTable(dbcon, table_name, df, row.names = FALSE, append = TRUE)


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

    return(
	  data.frame(
	    cnames = cnames,
		cclass = cclass,
		sqltype = fields
	  )
	)

}
