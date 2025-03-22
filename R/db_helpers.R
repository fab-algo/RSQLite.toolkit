#' dbExecFile 
#'
#' @param input_file the file name (including path) to be read.
#' @param dbcon database connection, as created by the dbConnect function.
#' @param plist a list with value to be binded to parameters of
#'    SQL statements
#' 
#' @import RSQLite
#' @export
dbExecFile <- function(input_file, dbcon, plist=NULL) {
    sql <- readLines(input_file)
    sql <- sql[grep("^--", sql, invert = T)]

    idx <- grep("--", sql)
    sql[idx] <- sub("(--).+", "", sql[idx])

    sql <- unlist(strsplit(paste(sql, collapse = " "), ";", fixed=TRUE))
    sql <- sql[grep("^ *$", sql, invert = T)]

    res<-list()
    if (length(sql) > 0) {
        if (is.null(plist)) {
            
            for (ii in seq_along(sql)) {
                res[[ii]] <- dbExecute(dbcon, sql[ii])
            }

        } else {

            if (length(sql) != length(plist)) {
                stop("RSQLite.toolkit: dbExecFile: ",
                     "number of statements should match length of parameters list.")
            }
            
            for (ii in seq_along(sql)) {
                rs <- dbSendQuery(dbcon, sql[ii])
                dbBind(rs, params = plist[[ii]])
                while (!dbHasCompleted(rs)) {
                    res[[ii]] <- dbFetch(rs, n=-1)
                }
                dbClearResult(rs)
            }
        }
    }

    return(res)
}



#' dbCopyTable
#'
#' @param db_file_src ...
#' @param db_file_tgt ...
#' @param table_name ...
#' @param drop_table ...
#' @param pk_exist ...
#' 
#' @import RSQLite
#' @export
dbCopyTable <- function(db_file_src, db_file_tgt,
                        table_name, drop_table = FALSE, pk_exist = TRUE) {
    if (substr(db_file_src, 1, 1) == ".") {
        db.file.src <- file.path(getwd(), db_file_src)
    }
    if (substr(db_file_tgt, 1, 1) == ".") {
        db.file.tgt <- file.path(getwd(), db_file_tgt)
    }

    dbsource <- dbConnect(dbDriver("SQLite"), db_file_src)
    dbtarget <- dbConnect(dbDriver("SQLite"), db_file_tgt)


    ## ---------------------------------------------------------------
    check <- dbGetQuery(dbtarget, paste("select name ",
        "from sqlite_master ",
        "where tbl_name='",
        table_name,
        "' and type='table'",
        sep = ""
    ))

    if (drop_table == FALSE && dim(check)[1] > 0) {
        stop("dbCopyTable: table already exists in target db.")
    }


    check <- dbGetQuery(dbsource, paste("select name ",
        "from sqlite_master ",
        "where tbl_name='",
        table_name,
        "' and type='table'",
        sep = ""
    ))

    if (dim(check)[1] == 0) {
        stop("dbCopyTable: table does not exist in source db.")
    }


    ## ---------------------------------------------------------------
    if (drop_table) {
        dbExecute(dbtarget, paste("DROP TABLE IF EXISTS ",
            table_name,
            sep = ""
        ))
    }


    ## ---------------------------------------------------------------
    sqlcmd <- dbGetQuery(dbsource, paste("select sql ",
        "from  sqlite_master ",
        "where type='table' ",
        "and   tbl_name='",
        table_name, "'",
        sep = ""
    ))

    dbExecute(dbtarget, sqlcmd[1, 1])


    ## ---------------------------------------------------------------
    sqlcmd <- paste("ATTACH DATABASE '",
        db_file_tgt, "' ",
        "AS TGT",
        sep = ""
    )

    dbExecute(dbsource, sqlcmd)

    sqlcmd <- paste("INSERT INTO TGT.",
        table_name,
        " SELECT * FROM ",
        table_name,
        sep = ""
    )

    dbExecute(dbsource, sqlcmd)

    dbExecute(dbsource, "DETACH DATABASE TGT")



    ## ---------------------------------------------------------------
    if (pk_exist) {
        sqlcmd <- dbGetQuery(dbsource, paste("select sql ",
            "from  sqlite_master ",
            "where type='index' ",
            "and   tbl_name='",
            table_name, "'",
            sep = ""
        ))
        if (dim(sqlcmd)[1] > 0) {
            for (ii in seq_len(dim(sqlcmd)[1])) {
                dbExecute(dbtarget, sqlcmd[ii, 1])
            }
        }
    }

    ## ---------------------------------------------------------------
    dbDisconnect(dbsource)

    dbDisconnect(dbtarget)
}



#' dbCreatePK
#'
#' @param dbcon ...
#' @param table_name ...
#' @param pk_fields ...
#'
#' @import RSQLite
#' @export
dbCreatePK <- function(dbcon, table_name, pk_fields) {
    sql.def <- paste("DROP INDEX IF EXISTS ",
                     paste(table_name, "_PK", sep = ""), ";", sep = "")
    dbExecute(dbcon, sql.def)

    sql.def <- paste("CREATE UNIQUE INDEX ",
                     paste(table_name, "_PK", sep = ""),
                     "ON ", table_name,
                     " (", paste(pk_fields, collapse = ", "), ");",
                     sep = " ")
    dbExecute(dbcon, sql.def)
}
