#' dbTableFromView create a table in a SQLite database from  a
#'    view already present in the same database.
#'
#' @param view_name character, name of the view.
#' @param dbcon database connection, as created by the dbConnect function.
#' @param table_name character, the name of the table.
#'
#' @param drop_table logical, if `TRUE` the target table will be dropped
#'    (if exists) and recreated when importing the data.  if `FALSE`, data
#'    from input file will be appended to an existing table.
#'    Defaults to `FALSE`.
#' @param build_pk logical, if `TRUE` creates a `UNIQUE INDEX` named
#'    `<table_name>_PK` defined by the combination of fields specified
#'    in the `pk_fields` parameter. It will be effective only if
#'    `pk_fields` is not null. Defaults to `FALSE`.
#' @param pk_fields character vector, the list of the fields' names that
#'    define the `UNIQUE INDEX`. Defaults to `NULL`.
#'
#' @returns integer, the number of records in `table_name` after writing data
#'    from the input view.
#'
#' @import RSQLite
#' @export
dbTableFromView <- function(view_name, dbcon, table_name,
                            drop_table = FALSE,
                            build_pk = FALSE, pk_fields = NULL) {
  ## local vars .................................
  fun_name <- match.call()[[1]]

  tryCatch({
    if (drop_table) {
      sql_def <- paste("DROP TABLE IF EXISTS ", table_name, ";", sep = "")
      dbExecute(dbcon, sql_def)
    }

    sql_def <- paste("CREATE TABLE ", table_name,
      " AS ",
      "SELECT * FROM ", view_name,
      ";",
      sep = ""
    )
    dbExecute(dbcon, sql_def)

  }, error = function(e) {
    err <- conditionMessage(e)
    step <- 105
    stop(error_handler(err, fun_name, step), .call = TRUE)
  })

  ## Indexing -------------------------------
  tryCatch({
    cnames1 <- dbListFields(dbcon, table_name)

    if (!is.null(pk_fields) && build_pk) {

      if (!is.character(pk_fields)) {
        stop("dbTableFromView: 'pk_fields' must be a character vector.")
      }

      check_fields <- setdiff(pk_fields, cnames1)
      if (length(check_fields) > 0) {
        stop("dbTableFromView: 'pk_fields' contains unknown field names: ",
             check_fields)
      }

      dbExecute(dbcon, paste(
        "CREATE UNIQUE INDEX ", paste(table_name, "_PK", sep = ""),
        "ON ", table_name, " (", paste(pk_fields, collapse = ", "),
        ");",
        sep = " "
      ))
    }

  }, error = function(e) {
    err <- conditionMessage(e)
    step <- 106
    stop(error_handler(err, fun_name, step), .call = TRUE)
  })

  dr <- dbGetQuery(dbcon, paste("select count(*) as nrows from ",
                                table_name, sep = ""))
  dr[1, 1]
}
