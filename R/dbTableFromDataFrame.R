#' Create a table in a SQLite database from a data frame
#' 
#' @description
#' The `dbTableFromDataFrame()` function reads the data from a rectangula region
#' of a sheet in an Excel file and copies it to a table in a SQLite
#' database. If table does not exist, it will create it.
#'
#' @param df the data frame to be saved in the SQLite table.
#' @param dbcon database connection, as created by the dbConnect function.
#' @param table_name character, the name of the table.
#'
#' @param id_quote_method character, used to specify how to build the SQLite
#'    columns' names using the fields' identifiers read from the input file.
#'    For details see the description of the `quote_method` parameter of
#'    the [format_column_names()] function. Defautls to `DB_NAMES`.
#' @param col_names character vector, names of the columuns to be imported.
#'    Used to override the field names derived from the data frame (using the
#'    quote method selected by `id_quote_method`). Must be of the same length
#'    of the number of columns in the data frame. If `NULL` the column names
#'    coming from the input (after quoting) will be used. Defaults to `NULL`.
#' @param col_types character vector of classes to be assumed for the columns.
#'    If not null, it will override the data types inferred from the input data
#'    frame. Must be of the same length of the number of columns in the input.
#'    If `NULL` the data type inferred from the input will be used.
#'    Defaults to `NULL`.
#'
#' @param drop_table logical, if `TRUE` the target table will be dropped
#'    (if exists) and recreated before importing the data.  if `FALSE`,
#'    data from input data frame will be appended to an existing table.
#'    Defaults to `FALSE`.
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
#' @returns integer, the number of records in `table_name` after reading data
#'    from the data frame.
#'
#' @examples
#' # Create a temporary database and load data frame
#' library(RSQLite.toolkit)
#' 
#' # Set up database connection
#' dbcon <- dbConnect(RSQLite::SQLite(), file.path(tempdir(), "example.sqlite"))
#' 
#' # Create a sample data frame
#' sample_data <- data.frame(
#'   id = 1:10,
#'   name = paste0("Item_", 1:10),
#'   value = runif(10, 1, 100),
#'   active = c(TRUE, FALSE),
#'   date = Sys.Date() + 0:9,
#'   stringsAsFactors = FALSE
#' )
#' 
#' # Load data frame with automatic primary key
#' dbTableFromDataFrame(
#'   df = sample_data,
#'   dbcon = dbcon,
#'   table_name = "SAMPLE_DATA",
#'   drop_table = TRUE,
#'   auto_pk = TRUE
#' )
#' 
#' # Check the imported data
#' dbListFields(dbcon, "SAMPLE_DATA")
#' dbGetQuery(dbcon, "SELECT * FROM SAMPLE_DATA LIMIT 5")
#' 
#' # Load with column selection and custom naming
#' dbTableFromDataFrame(
#'   df = sample_data,
#'   dbcon = dbcon,
#'   table_name = "SAMPLE_SUBSET",
#'   drop_table = TRUE,
#'   col_names = c("ID", "ITEM_NAME", "ITEM_VALUE", "IS_ACTIVE", "DATE_CREATED")
#' )
#' 
#' # Clean up
#' dbDisconnect(dbcon)
#'
#' @import RSQLite
#' @export
dbTableFromDataFrame <- function(df, dbcon, table_name,
                                 id_quote_method = "DB_NAMES",
                                 col_names = NULL, col_types = NULL,
                                 drop_table = FALSE,
                                 auto_pk = FALSE, build_pk = FALSE,
                                 pk_fields = NULL) {
  ## local vars .................................
  fun_name <- match.call()[[1]]

  ## read schema ................................
  tryCatch({
    src_names <- names(df)

    dnames <- format_column_names(src_names, quote_method = id_quote_method)
    cnames <- dnames$quoted
    cnames_unquoted <- dnames$unquoted 
    cclass <- vapply(df, function(col) class(col)[1], character(1))
    fields <- R2SQL_types(cclass)

    if (!is.null(col_names)) {
      if (length(col_names) != length(cnames)) {
        stop("dbTableFromFeather: wrong 'col_names' length, must be ",
             length(cnames), " elements but found ", length(col_names))
      }
      dnames <- format_column_names(x = col_names,
                                    quote_method = id_quote_method,
                                    unique_names = FALSE)
      cnames <- dnames$quoted
      cnames_unquoted <- dnames$unquoted
    }

    if (!is.null(col_types)) {
      if (length(col_types) != length(cclass)) {
        stop("dbTableFromFeather: wrong 'col_types' length, must be ",
             length(cclass), " elements but found ", length(col_types))
      }
      cclass <- col_types
      fields <- R2SQL_types(col_types)
    }

  }, error = function(e) {
    err <- conditionMessage(e)
    step <- 102
    stop(error_handler(err, fun_name, step), .call = TRUE)
  })


  ## create empty table .........................
  tryCatch({
    if (drop_table) {
      sql_def <- paste("DROP TABLE IF EXISTS ", table_name, ";", sep = "")
      dbExecute(dbcon, sql_def)
    }

    auto_pk1 <- FALSE
    if (is.null(pk_fields) && !build_pk && auto_pk) auto_pk1 <- TRUE

    sql_head <- paste("CREATE TABLE IF NOT EXISTS ",
                      table_name, " (", sep = "")
    sql_body <- paste(cnames, fields, sep = " ", collapse = ", ")

    if (auto_pk1) {
      sql_body <- paste(sql_body, ", SEQ INTEGER PRIMARY KEY", sep = "")
      cnames_unquoted2 <- c(cnames_unquoted, "SEQ")
    } else {
      cnames_unquoted2 <- cnames_unquoted
    }

    sql_tail <- ");"
    sql_def <- paste(sql_head, sql_body, sql_tail, sep = " ")

    dbExecute(dbcon, sql_def)

  }, error = function(e) {
    err <- conditionMessage(e)
    step <- 103
    stop(error_handler(err, fun_name, step), .call = TRUE)
  })


  ## Write data -------------------------------
  tryCatch({
    names(df) <- cnames_unquoted2

    if (auto_pk1) {
      df <- cbind(df, NA)
    }

    dbWriteTable(dbcon, table_name, as.data.frame(df),
                 row.names = FALSE, append = TRUE)

  }, error = function(e) {
    err <- conditionMessage(e)
    step <- 105
    stop(error_handler(err, fun_name, step), .call = TRUE)
  })


  ## Indexing -------------------------------
  tryCatch({
    if (!is.null(pk_fields) && build_pk) {

      if (!is.character(pk_fields)) {
        stop("dbCreateTableFromDF: 'pk_fields' must be a character vector.")
      }

      pk_fields <- format_column_names(
        pk_fields, quote_method = id_quote_method)$quoted

      check_fields <- setdiff(pk_fields, cnames)
      if (length(check_fields) > 0) {
        stop("dbCreateTableFromDF: 'pk_fields' contains unknown field names: ",
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
