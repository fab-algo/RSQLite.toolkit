#' dbTableFromXlsx create a table in a SQLite database from a
#'   range of an Excel worksheet
#'
#' The dbTableFromXlsx function reads the data from a range of
#' an Excel worksheet. If table does not exist, it will
#' create it.
#'
#' @param input_file character, the file name (including path) to be read.
#' @param dbcon database connection, as created by the dbConnect function.
#' @param table_name character, the name of the table.
#'
#' @param sheet_name character, the name of the worksheet containing the data
#'    table.
#' @param first_row integer, the row number where the data table starts.
#'    If present, it is the row number of the header row, otherwise it is
#'    the row number of the first row of data.
#' @param cols_range integer, a numeric vector specifying which columns in
#'    the worksheet to be read.
#' @param header logical, if `TRUE` the first row contains the fields'
#'    names. If `FALSE`, the column names will be the column names of the
#'    Excel worksheet (i.e. letters).
#'
#' @param id_quote_method character, used to specify how to build the SQLite
#'    columns' names using the fields' identifiers read from the input file.
#'    For details see the description of the `quote_method` parameter of
#'    the [format_column_names()] function. Defautls to `DB_NAMES`.
#'
#' @param col_names character vector, names of the columuns in the input file.
#'    Used to override the field names derived from the input file (using the
#'    quote method selected by `id_quote_method`). Must be of the same length
#'    of the number of columns in the input file. If `NULL` the column names
#'    coming from the input file (after quoting) will be used.
#'    Defaults to `NULL`.
#' @param col_types character vector of classes to be assumed for the columns
#'    of the input file. Must be of the same length of the number of columns
#'    in the input file. If not null, it will override the data types guessed
#'    from the input file.
#'    If `NULL` the data type inferred from the input files will be used.
#'    Defaults to `NULL`.
#'
#' @param col_import can be either:
#'    - a numeric vector (coherced to integers) with the columns' positions
#'      in the input file that will be imported in the SQLite table;
#'    - a character vector with the columns' names to be imported. The names
#'      are those in the input file (after quoting with `id_quote_method`),
#'      if `col_names` is NULL, or those expressed in `col_names` vector.
#'    Defaults to NULL, i.e. all columns will be imported.
#'
#' @param drop_table logical, if `TRUE` the target table will be dropped (if
#'    exists) and recreated before importing the data.  if `FALSE`, data from
#'    input file will be appended to an existing table. Defaults to `FALSE`.
#' @param auto_pk logical, if `TRUE`, and `pk_fields` parameter is `NULL`, an
#'    additional column named `SEQ` will be added to the table and it will be
#'    defined to be `INTEGER PRIMARY KEY` (i.e. in effect an alias for
#'    `ROWID`). Defaults to `FALSE`.
#' @param build_pk logical, if `TRUE` creates a `UNIQUE INDEX` named
#'    `<table_name>_PK` defined by the combination of fields specified
#'    in the `pk_fields` parameter. It will be effective only if
#'    `pk_fields` is not null. Defaults to `FALSE`.
#' @param pk_fields character vector, the list of the fields' names that
#'    define the `UNIQUE INDEX`. Defults to `NULL`.
#'
#' @param constant_values a one row data frame whose columns will be added to
#'    the table in the database. The additional table columns will be named
#'    as the data frame columns, and the corresponding values will be associeted
#'    to each record imported from the input file. It is useful to keep
#'    track of additional information (e.g., the input file name, additional
#'    context data not available in the data set, ...) when loading
#'    the content of multiple input files in the same table. Defults to `NULL`.
#'
#' @param ... additional arguments passed to [openxlsx2::wb_to_df()] function
#'    used to read input data.
#'
#' @returns integer, the number of records in `table_name` after reading data
#'    from `input_file`.
#'
#' @import RSQLite
#' @importFrom openxlsx2 wb_to_df
#' @export
dbTableFromXlsx <- function(input_file, dbcon, table_name,   #nolint
                            sheet_name, first_row, cols_range, header = TRUE,
                            id_quote_method = "DB_NAMES",
                            col_names = NULL, col_types = NULL,
                            col_import = NULL,
                            drop_table = FALSE,
                            auto_pk = FALSE, build_pk = FALSE, pk_fields = NULL,
                            constant_values = NULL, ...) {
  ## local vars .................................
  fun_name <- match.call()[[1]]

  ## formal checks on parameters ................
  if (!("data.frame" %in% class(constant_values)) &&
        !is.null(constant_values)) {
    stop("dbTableFromXlsx: 'constant_values' must be a data frame.")
  }

  if (("data.frame" %in% class(constant_values)) &&
        length(constant_values) == 0) {
    stop("dbTableFromXlsx: 'constant_values' must not be of zero length.")
  }

  if (("data.frame" %in% class(constant_values)) &&
        dim(constant_values)[1] > 1) {
    stop("dbTableFromXlsx: 'constant_values' must have one row.")
  }

  ## read schema ................................
  tryCatch({
    df_scm <- file_schema_xlsx(input_file, sheet_name = sheet_name,
                               first_row = first_row, cols_range = cols_range,
                               header = header,
                               id_quote_method = id_quote_method)

    cnames <- df_scm$col_names
    cnames_unquoted <- df_scm$col_names_unquoted
    cclass <- df_scm$col_types
    fields <- df_scm$sql_types

  }, error = function(e) {
    err <- conditionMessage(e)
    step <- 121
    stop(error_handler(err, fun_name, step), .call = TRUE)
  })


  tryCatch({
    if (!is.null(col_names)) {
      if (length(col_names) != length(cnames)) {
        stop("dbTableFromXlsx: wrong 'col_names' length, must be ",
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
        stop("dbTableFromXlsx: wrong 'col_types' length, must be ",
             length(cclass), " elements but found ", length(col_types))
      }
      cclass <- col_types
      fields <- R2SQL_types(col_types)
    }

    if (!is.null(col_import)) {

      if ("numeric" %in% class(col_import) ||
            "integer" %in% class(col_import)) {
        if (!all(col_import %in% c(seq_along(cnames)))) {
          stop("dbTableFromXlsx: wrong 'col_import' specification, columns ",
               "numbers must be between 1 and ", length(cnames))
        }
        idx_import <- which(c(seq_along(cnames)) %in% col_import)

      } else {
        if (!all(col_import %in% cnames)) {
          stop("dbTableFromXlsx: wrong 'col_import' specification, columns ",
               "names must be either quoted names in import file or in ",
               "'col_names'.")
        }
        idx_import <- which(cnames %in% col_import)
      }
    } else {
      idx_import <- c(seq_along(cnames))
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

    sql_head <- paste("CREATE TABLE IF NOT EXISTS ", table_name, " (", sep = "")
    sql_body <- paste(cnames[idx_import],
                      fields[idx_import], sep = " ", collapse = ", ")

    cv_names <- c()
    cv_types <- c()

    if (!is.null(constant_values)) {
      src_names <- names(constant_values)
      dnames <- format_column_names(x = src_names,
                                    quote_method = id_quote_method,
                                    unique_names = TRUE)
      cv_names <- dnames$quoted
      cv_names_unquoted <- dnames$unquoted
      cv_types <- vapply(constant_values,
                         function(col) class(col)[1], character(1))
      cv_sql <- R2SQL_types(cv_types)

      sql_ext <- paste(cv_names, cv_sql, sep = " ", collapse = ", ")
      sql_body <- paste(sql_body, ", ", sql_ext, sep = "")

      names(constant_values) <- cv_names

      cnames1 <- c(cnames, cv_names)
      cnames_unquoted1 <- c(cnames_unquoted, cv_names_unquoted)
      idx_import1 <- c(idx_import, length(cnames) + c(seq_along(cv_names)))
    } else {
      cnames1 <- cnames
      cnames_unquoted1 <- cnames_unquoted
      idx_import1 <- idx_import
    }

    auto_pk1 <- FALSE
    if (length(pk_fields) == 0 && !build_pk && auto_pk) auto_pk1 <- TRUE

    if (auto_pk1) {
      sql_body <- paste(sql_body, ", SEQ INTEGER PRIMARY KEY", sep = "")
      cnames_unquoted2 <- c(cnames_unquoted1, "SEQ")
      idx_import2 <- c(idx_import1, length(cnames1) + 1)
    } else {
      cnames_unquoted2 <- cnames_unquoted1
      idx_import2 <- idx_import1
    }

    sql_tail <- ");"
    sql_def <- paste(sql_head, sql_body, sql_tail, sep = " ")

    dbExecute(dbcon, sql_def)

  }, error = function(e) {
    err <- conditionMessage(e)
    step <- 103
    stop(error_handler(err, fun_name, step), .call = TRUE)
  })


  ## read data ..................................
  tryCatch({
    df <- openxlsx2::wb_to_df(
      file = input_file,
      sheet = sheet_name,
      start_row = first_row,
      col_names = header,
      cols = cols_range,
      ...
    )
  }, error = function(e) {
    err <- conditionMessage(e)
    step <- 104
    stop(error_handler(err, fun_name, step), .call = TRUE)
  })


  ## Write data ................................
  tryCatch({
    if (!is.null(constant_values)) {
      df <- cbind(df, constant_values)
    }

    df[, which(cclass == "Date")] <-
      as.data.frame(apply(df[, which(cclass == "Date")], 2,
                          function(x) format(x, format = "%Y-%m-%d"))
      )

    if (auto_pk1) {
      df <- cbind(df, NA)
    }

    names(df) <- cnames_unquoted2
    df <- df[, idx_import2]

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
        stop("dbTableFromXlsx: 'pk_fields' must be a character vector.")
      }

      pk_fields <- format_column_names(pk_fields,
                                       quote_method = id_quote_method)$quoted

      check_fields <- setdiff(pk_fields, cnames1)
      if (length(check_fields) > 0) {
        stop("dbTableFromXlsx: 'pk_fields' contains unknown field names: ",
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
