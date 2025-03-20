#' The dbTableFromCSV function reads the data from a rectangular region
#' of a sheet in an Excel file and copies it to a table in a SQLite
#' database. If the destination table does not exist, it will create it.
#'
#' @param input_file the file name (including path) to be read
#' @param schema_file ...
#' @param header ...
#' @param dbcon ...
#' @param table_name ...
#' @param drop_table ...
#' @param auto_pk ...
#' @param build_pk ...
#' @param constant_values ...
#' @param chunk_size ...
#' @param ... ...
#' @returns nothing
#'
#' @import RSQLite
#' @importFrom utils read.table
#' @export
dbTableFromCSV <- function(input_file, schema_file, dbcon, table_name,
                           header = FALSE, drop_table = FALSE,
                           auto_pk = FALSE, build_pk = FALSE,
                           constant_values = NULL, chunk_size = 10000, ...) {

    ## formal checks on parameters ................
    if (!is.list(constant_values) && !is.null(constant_values)) {
        stop("dbTableFromCSV: error in 'constant.values' param.: must be a list.")
    }

    if (is.list(constant_values) && length(constant_values) == 0) {
        stop("dbTableFromCSV: 'constant.values' list must not be of zero length.")
    }

    ## read schema ................................
    df.scm <- read.table(
        file = schema_file, 
		header = TRUE, sep = ",",
        skip = 0, 
		quote = "", comment.char = "", 
		row.names = NULL,
        colClasses = rep("character", times = 4),
        col.names = c("VARNAME", "SQLTYPE", "RTYPE", "PK"),
        strip.white = TRUE, stringsAsFactors = FALSE
    )

    cclass <- df.scm$RTYPE
    fields <- sapply(df.scm$RTYPE, SQLtype)
    cnames1 <- df.scm$VARNAME
	cnames <- paste("[",cnames1,"]", sep="")
    
    ## create empty table .........................
    if (drop_table) {
        sql.def <- paste("DROP TABLE IF EXISTS ", table_name, ";", sep = "")
        dbExecute(dbcon, sql.def)
    }

    autoPK <- FALSE
    if (length(df.scm[which(df.scm$PK == "Y"), "VARNAME"]) == 0 && auto_pk)
        autoPK <- TRUE

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

    if (autoPK) {
        sql.body <- paste(sql.body, ", SEQ INTEGER PRIMARY KEY", sep = "")
		cnames1 <- c(cnames1, "SEQ")
    }

    sql.tail <- ");"
    sql.def <- paste(sql.head, sql.body, sql.tail, sep = " ")

    dbExecute(dbcon, sql.def)


    ## read data ..................................
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
            file = fcon,
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
            names(dfbuffer) <- c(df.scm$VARNAME, cv_names)
        } else {
            names(dfbuffer) <- cnames1
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
    if (drop_table && !auto_pk &&
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
