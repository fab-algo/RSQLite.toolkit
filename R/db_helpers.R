#' Execute SQL statements from a text file
#' 
#' @description
#' The `dbExecFile()` function executes the SQL statements contained
#' in a text file.
#'
#' This function reads the text in `input_file`, strips all comment lines
#' (i.e. all lines beginning with `--` characters) and splits the SQL statements
#' assuming that they are separeted by the `;` character. The list of SQL
#' statements is then executed, one at a time; the results of each statements
#' are stored in a list with length equal to the number of statements.
#'
#' @param input_file the file name (including path) containing the SQL
#'    statements to be executed
#' @param dbcon database connection, as created by the dbConnect function.
#' @param plist a list with values to be binded to the parameters of
#'    SQL statements. Defaults to `NULL`
#'
#' @returns a list with the results of each statement executed.
#'
#' @import RSQLite
#' @export
dbExecFile <- function(input_file, dbcon, plist = NULL) {
  sql <- readLines(input_file)
  sql <- sql[grep("^--", sql, invert = TRUE)]

  idx <- grep("--", sql)
  sql[idx] <- sub("(--).+", "", sql[idx])

  sql <- unlist(strsplit(paste(sql, collapse = " "), ";", fixed = TRUE))
  sql <- sql[grep("^ *$", sql, invert = TRUE)]

  res <- list()
  if (length(sql) > 0) {
    if (is.null(plist)) {

      for (ii in seq_along(sql)) {
        res[[ii]] <- dbExecute(dbcon, sql[ii])
      }

    } else {

      if (length(sql) != length(plist)) {
        stop("RSQLite.toolkit: dbExecFile: ",
             "number of parameters in list should match ",
             "the length of the statements.")
      }

      for (ii in seq_along(sql)) {
        rs <- dbSendQuery(dbcon, sql[ii])
        dbBind(rs, params = plist[[ii]])
        while (!dbHasCompleted(rs)) {
          res[[ii]] <- dbFetch(rs, n = -1)
        }
        dbClearResult(rs)
      }
    }
  }

  res
}



#' Copy a table from one SQLite database to another
#'
#' @description
#' The `dbCopyTable()` function can be used to create a copy of the data in a table
#' of a SQLite database in another database. The data can be appended
#' to an already existing table (with the same name of the source one), or
#' a new table can be created. It is possible to move also the indexes
#' from source to target.
#'
#' @param db_file_src character, the file name (including path) of the source
#'    database containing the table to be copied.
#' @param db_file_tgt character, the file name (including path) of the target
#'    database where the table will be copied.
#' @param table_name character, the table name.
#' @param drop_table logical, if `TRUE` the table in the target database will be
#'   dropped (if exists) before copying the data. If `FALSE`, the data will be
#'   appended to an existing table in the target database. Defaults to `FALSE`.
#' @param copy_indexes logical, if `TRUE` and also `drop_table` is `TRUE`,
#'   all indexes defined on the source table will be created on the target
#'   table. Defaults to `FALSE`.
#'
#' @returns nothing
#'
#' @import RSQLite
#' @export
dbCopyTable <- function(db_file_src, db_file_tgt, table_name,
                        drop_table = FALSE, copy_indexes = FALSE) {
  if (substr(db_file_src, 1, 1) == ".") {
    db_file_src <- file.path(getwd(), db_file_src)
  }
  if (substr(db_file_tgt, 1, 1) == ".") {
    db_file_tgt <- file.path(getwd(), db_file_tgt)
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
  if (copy_indexes && drop_table) {
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



#' Creates a unique index on a table in a SQLite database
#'
#' The `dbCreatePK()` function creates a `UNIQUE INDEX` named 
#' `<table_name>_PK` on the table specified by `table_name` in
#' the database connected by `dbcon`. The index is created on
#' the fields specified in the `pk_fields` argument.
#'
#' @param dbcon database connection, as created by the dbConnect function.
#' @param table_name character, the name of the table where the index
#'    will be created.
#' @param pk_fields character vector, the list of the fields' names that
#'    define the `UNIQUE INDEX`.
#' @param drop_index logical, if `TRUE` the index named `<table_name>_PK` will
#'   be dropped (if exists) before recreating it. If `FALSE`, it will check
#'   if an index with that name exists and eventually stops. Default to `FALSE`.
#'
#' @returns nothing
#'
#' @import RSQLite
#' @export
dbCreatePK <- function(dbcon, table_name, pk_fields, drop_index = FALSE) {

  sqlcmd <- dbGetQuery(dbcon, paste("select name ",
    "from  sqlite_master ",
    "where type='index' ",
    "and   tbl_name='",
    table_name, "' ",
    "and   name='",
    table_name, "_PK' ",
    sep = ""
  ))
  if (dim(sqlcmd)[1] > 0 && !drop_index) {
    stop("RSQLite.toolkit: dbCreatePK: '",
         table_name, "_PK' index already exists and drop_index=FALSE.")
  }

  sql_def <- paste("DROP INDEX IF EXISTS ",
                   paste(table_name, "_PK", sep = ""),
                   ";", sep = "")
  dbExecute(dbcon, sql_def)

  cnames <- dbListFields(dbcon, table_name)
  check_fields <- setdiff(pk_fields, cnames)
  if (length(check_fields) > 0) {
    stop(paste0("RSQLite.toolkit: dbCreatePK: ",
                "'pk_fields' contains unknown field names: "),
         check_fields)
  }

  sql_def <- paste("CREATE UNIQUE INDEX ",
                   paste(table_name, "_PK", sep = ""),
                   "ON ", table_name,
                   " (", paste(pk_fields, collapse = ", "), ");",
                   sep = " ")
  dbExecute(dbcon, sql_def)
}
