#' The dbTableFromSchema function reads the data from a rectangula region
#' of a sheet in an Excel file and copies it to a table in a SQLite
#' database. If table does not exist, it will create it.
#'
#' @param schema_file ...
#' @param dbcon ...
#' @param table_name ...
#' @param drop_table ...
#' @param auto_pk ...
#' @param build_pk ...
#' @param constant_values ...
#' @returns nothing
#'
#' @importFrom utils read.table
#' @import RSQLite
#' @export
dbTableFromSchema <- function(schema_file, dbcon, table_name, drop_table = FALSE,
                              auto_pk = FALSE, build_pk = FALSE,
                              constant_values = NULL) {

    ## formal checks on parameters ................
    if (!is.list(constant_values) && !is.null(constant_values)) {
        stop("dbReadFileTable: error in 'constant.values' param.: must be a list.")
    }

    if (is.list(constant_values) && length(constant_values) == 0) {
        stop("dbReadFileTable: 'constant.values' list must not be of zero length.")
    }

    ## read schema ................................
    df.scm <- read.table(
        file = schema_file, header = TRUE, sep = ",",
        skip = 0, quote = "", comment.char = "", row.names = NULL,
        colClasses = rep("character", times = 4),
        col.names = c("VARNAME", "SQLTYPE", "RTYPE", "PK"),
        strip.white = TRUE, stringsAsFactors = FALSE
    )


    ## create empty table .........................
    autoPK <- FALSE
    if (length(df.scm[which(df.scm$PK == "Y"), "VARNAME"]) == 0 &&
        auto_pk) autoPK <- TRUE

    if (drop_table) {
        sql.def <- paste("DROP TABLE IF EXISTS ", table_name, ";", sep = "")
        dbExecute(dbcon, sql.def)
    }


    sql.head <- paste("CREATE TABLE IF NOT EXISTS ", table_name, " (", sep = "")
    sql.body <- paste(df.scm$VARNAME, df.scm$SQLTYPE, sep = " ", collapse = ", ")

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
        cnames <- c(cnames, cv_names)
    }

    if (autoPK) {
        sql.body <- paste(sql.body, ", SEQ INTEGER PRIMARY KEY", sep = "")
    }

    sql.tail <- ");"
    sql.def <- paste(sql.head, sql.body, sql.tail, sep = " ")

    dbExecute(dbcon, sql.def)

    ## Indexing -------------------------------
    if (drop_table &&
        !auto_pk &&
        length(df.scm[which(df.scm$PK == "Y"), "VARNAME"]) > 0 &&
        build_pk) {
        
		cnames <- df.scm[which(df.scm$PK == "Y"), "VARNAME"]
        
		dbExecute(dbcon, paste(
            "CREATE UNIQUE INDEX ", paste(table_name, "_PK", sep = ""),
            "ON ", table_name, " (", paste(cnames, collapse = ", "),
            ");",
            sep = " "
        ))
    }
}
