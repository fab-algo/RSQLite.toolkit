#' Create a table from a delimiter separated values (DSV) text file
#' 
#' @description
#' The `dbTableFromDSV()` function reads the data from a DSV file
#' and copies it to a table in a SQLite database. If table does
#' not exist, it will create it.
#'
#' The `dbTableFromDSV()` function reads the data from a DSV file
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
#' @param dec character, decimal separator (e.g., "." or "," depending on
#'    locale) in the input file. Defaults to ".".
#' @param grp character, character used for digit grouping. It defaults
#'    to `""` (i.e. no grouping).
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
#' @param drop_table logical, if `TRUE` the target table will be dropped
#'    (if exists) and recreated before importing the data.  if `FALSE`, data
#'    from input file will be appended to an existing table.
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
#'    number, will process the input file by blocks of `chunk_size` lines,
#'    avoiding to read all the data in memory at once. It can be useful
#'    for very large size files. If set to zero, it will process the whole
#'    text file in one pass. Default to zero.
#'
#' @param ... additional arguments passed to [base::scan()] function used
#'    to read input data. Please note that if the `quote` parameter is not
#'    specified, it will be set to `""` (i.e., no quoting) by default.
#'
#'
#' @returns integer, the number of records in `table_name` after reading data
#'    from `input_file`.
#'
#' @examples
#' # Create a temporary database and load CSV data
#' library(RSQLite.toolkit)
#' 
#' # Set up database connection
#' dbcon <- dbConnect(RSQLite::SQLite(), file.path(tempdir(), "example.sqlite"))
#' 
#' # Get path to example data
#' data_path <- system.file("extdata", package = "RSQLite.toolkit")
#' 
#' # Load abalone CSV data with automatic primary key
#' dbTableFromDSV(
#'   input_file = file.path(data_path, "abalone.csv"),
#'   dbcon = dbcon,
#'   table_name = "ABALONE",
#'   drop_table = TRUE,
#'   auto_pk = TRUE,
#'   header = TRUE,
#'   sep = ",",
#'   dec = "."
#' )
#' 
#' # Check the imported data
#' dbListFields(dbcon, "ABALONE")
#' head(dbGetQuery(dbcon, "SELECT * FROM ABALONE"))
#' 
#' # Load data with specific column selection
#' dbTableFromDSV(
#'   input_file = file.path(data_path, "abalone.csv"),
#'   dbcon = dbcon,
#'   table_name = "ABALONE_SUBSET",
#'   drop_table = TRUE,
#'   header = TRUE,
#'   sep = ",",
#'   dec = ".",
#'   col_import = c("SEX", "LENGTH", "DIAMETER", "WHOLE")
#' )
#' 
#' # Check available tables
#' dbListTables(dbcon)
#' 
#' # Clean up
#' dbDisconnect(dbcon)
#'
#' @import RSQLite
#' @export
dbTableFromDSV <- function(input_file, dbcon, table_name,  #nolint
                           header = TRUE, sep = ",", dec = ".", grp = "",
                           id_quote_method = "DB_NAMES",
                           col_names = NULL, col_types = NULL,
                           col_import = NULL,
                           drop_table = FALSE,
                           auto_pk = FALSE, build_pk = FALSE, pk_fields = NULL,
                           constant_values = NULL, chunk_size = 0, ...) {

  ## local vars ....................................
  fun_name <- match.call()[[1]]

  ## optional parameters passed to "scan" .........
  lpar <- list(...)

  if (!("quote" %in% names(lpar))) {
    lpar$quote <- ""
  }

  if (!("comment.char" %in% names(lpar))) {
    lpar$comment.char <- ""
  }

  if (!("skip" %in% names(lpar))) {
    lpar$skip <- 0
  }

  if (!("fileEncoding" %in% names(lpar))) {
    lpar$fileEncoding <- ""
  }

  ## formal checks on parameters ................
  if (!("data.frame" %in% class(constant_values)) &&
        !is.null(constant_values)) {
    stop("dbTableFromDSV: 'constant_values' must be a data frame.")
  }

  if (("data.frame" %in% class(constant_values)) &&
        length(constant_values) == 0) {
    stop("dbTableFromDSV: 'constant_values' must not be of zero length.")
  }

  if (("data.frame" %in% class(constant_values)) &&
        dim(constant_values)[1] > 1) {
    stop("dbTableFromDSV: 'constant_values' must have one row.")
  }

  ## read schema ................................
  tryCatch({
    df_scm <- file_schema_dsv(input_file,
                              header = header, sep = sep,
                              dec = dec, grp = grp,
                              id_quote_method = id_quote_method,
                              max_lines = 2000, ...)$schema

    cnames <- df_scm$col_names
    cnames_unquoted <- df_scm$col_names_unquoted
    cclass <- df_scm$col_types
    fields <- df_scm$sql_types

  }, error = function(e) {
    err <- conditionMessage(e)
    step <- 101
    stop(error_handler(err, fun_name, step), call. = FALSE)
  })


  ## handle col_ ................................
  tryCatch({
    if (!is.null(col_names)) {
      if (length(col_names) != length(cnames)) {
        stop("dbTableFromDSV: wrong 'col_names' length, must be ",
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
        stop("dbTableFromDSV: wrong 'col_types' length, must be ",
             length(cclass), " elements but found ", length(col_types))
      }
      cclass <- col_types
      fields <- R2SQL_types(col_types)
    }

    if (!is.null(col_import)) {

      if ("numeric" %in% class(col_import) ||
            "integer" %in% class(col_import)) {
        if (!all(col_import %in% c(seq_along(cnames)))) {
          stop("dbTableFromDSV: wrong 'col_import' specification, columns ",
               "numbers must be between 1 and ", length(cnames))
        }
        idx_import <- which(c(seq_along(cnames)) %in% col_import)

      } else {
        if (!all(col_import %in% cnames)) {
          stop("dbTableFromDSV: wrong 'col_import' specification, columns ",
               "names must be either quoted names in import file or ",
               "in 'col_names'.")
        }
        idx_import <- which(cnames %in% col_import)
      }
    } else {
      idx_import <- c(seq_along(cnames))
    }

  }, error = function(e) {
    err <- conditionMessage(e)
    step <- 102
    stop(error_handler(err, fun_name, step), call. = FALSE)
  })


  ## create empty table .........................
  tryCatch({
    if (drop_table) {
      sql_def <- paste("DROP TABLE IF EXISTS ", table_name, ";", sep = "")
      dbExecute(dbcon, sql_def)
    }

    sql_head <- paste("CREATE TABLE IF NOT EXISTS ", table_name, " (", sep = "")
    sql_body <- paste(cnames[idx_import], fields[idx_import], sep = " ",
                      collapse = ", ")
    cv_names <- c()
    cv_types <- c()

    if (!is.null(constant_values)) {
      src_names <- names(constant_values)
      dnames_cv <- format_column_names(x = src_names,
                                       quote_method = id_quote_method,
                                       unique_names = FALSE)
      cv_names <- dnames_cv$quoted
      cv_names_unquoted <- dnames_cv$unquoted

      cv_types <- vapply(constant_values,
                         function(col) class(col)[1], character(1))
      cv_sql <- R2SQL_types(cv_types)

      sql_ext <- paste(cv_names, cv_sql, sep = " ", collapse = ", ")
      sql_body <- paste(sql_body, ", ", sql_ext, sep = "")

      names(constant_values) <- cv_names_unquoted

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
    stop(error_handler(err, fun_name, step), call. = FALSE)
  })


  ## read & write data ..................................
  lclass <- list()
  for (ii in seq_along(cclass)) {
    if (cclass[ii] == "Date") {
      lclass[[ii]] <- vector("character", 0)
    } else if (cclass[ii] %in%
                 c("integer_grouped", "numeric_grouped", "double_grouped")) {
      lclass[[ii]] <- vector("character", 0)
    } else {
      lclass[[ii]] <- vector(cclass[ii], 0)
    }
  }

  tryCatch({
    fcon <- file(input_file, "r", blocking = FALSE)

    if (header) {
      scan(file = fcon, what = character(),
           nlines = 1, quiet = TRUE, ...)
    }

  }, error = function(e) {
    close(fcon)

    err <- conditionMessage(e)
    step <- 104
    stop(error_handler(err, fun_name, step), call. = FALSE)
  })


  nread <- 0
  repeat {
    tryCatch({
      allowed_par_scan <- c("quote", "comment.char", "na.strings",
                            "allowEscapes", "strip.white",
                            "skip", "fill",  "flush", "skipNul",
                            "blank.lines.skip",
                            "fileEncoding", "encoding")

      lpar <- lpar[which(names(lpar) %in% allowed_par_scan)]

      lpar1 <- append(x = lpar,
                      values = list(
                        file = fcon,
                        nlines = chunk_size,
                        sep = sep,
                        dec = dec,
                        what = lclass,
                        multi.line = FALSE,
                        quiet = TRUE
                      ),
                      after = 0)

      dfbuffer <- do.call(scan, lpar1)

      if (length(dfbuffer[[1]]) == 0) break

      dfbuffer <- as.data.frame(dfbuffer,
                                row.names = NULL,
                                stringsAsFactors = FALSE)

      ## Constant values ......................................
      if (!is.null(constant_values)) {
        dfbuffer <- cbind(dfbuffer, constant_values)
      }


      ## Additional conversions ...............................
      idx1 <- which(cclass %in% c("numeric_grouped", "double_grouped"))
      if (length(idx1) > 0) {
        dfbuffer[, idx1] <- as.data.frame(
          apply(X = dfbuffer[, idx1],
                MARGIN = 2,
                FUN = convert_grouped_digits,
                to = "numeric", dec = dec, grp = grp)
        )
      }

      idx2 <- which(cclass %in% c("integer_grouped"))
      if (length(idx2) > 0) {
        dfbuffer[, idx2] <- as.data.frame(
          apply(X = dfbuffer[, idx2], MARGIN = 2,
                FUN = convert_grouped_digits,
                to = "integer", dec = dec, grp = grp)
        )
      }

      idx3 <- which(cclass == "Date")
      if (length(idx3) > 0) {
        dfbuffer[, idx3] <- as.data.frame(
          apply(X = dfbuffer[, idx3], MARGIN = 2,
                FUN = function(x) format(x, format = "%Y-%m-%d"))
        )
      }

    }, error = function(e) {
      close(fcon)

      err <- conditionMessage(e)
      step <- 104
      stop(error_handler(err, fun_name, step), call. = FALSE)
    })


    ## Write data ...............................
    tryCatch({
      if (auto_pk1) {
        dfbuffer <- cbind(dfbuffer, SEQ = NA_integer_)
      }

      names(dfbuffer) <- cnames_unquoted2
      dfbuffer <- dfbuffer[, idx_import2]

      dbWriteTable(dbcon, table_name, as.data.frame(dfbuffer),
                   row.names = FALSE, append = TRUE)
      nread <- nread + chunk_size

    }, error = function(e) {
      close(fcon)

      err <- conditionMessage(e)
      step <- 105
      stop(error_handler(err, fun_name, step), call. = FALSE)
    })
  }

  close(fcon)

  ## Indexing -------------------------------
  tryCatch({
    if (!is.null(pk_fields) && build_pk) {

      if (!is.character(pk_fields)) {
        stop("dbTableFromDSV: 'pk_fields' must be a character vector.")
      }

      pk_fields <- format_column_names(pk_fields,
                                       quote_method = id_quote_method)$quoted

      check_fields <- setdiff(pk_fields, cnames1)
      if (length(check_fields) > 0) {
        stop("dbTableFromDSV: 'pk_fields' contains unknown field names: ",
             paste(check_fields, collapse = ", "))
      }

      dbExecute(dbcon, paste(
        "CREATE UNIQUE INDEX ",
        paste(table_name, "_PK", sep = ""),
        "ON ", table_name, " (",
        paste(pk_fields, collapse = ", "),
        ");",
        sep = " "
      ))
    }

  }, error = function(e) {
    close(fcon)

    err <- conditionMessage(e)
    step <- 106
    stop(error_handler(err, fun_name, step), call. = FALSE)
  })


  dr <- dbGetQuery(dbcon, paste("select count(*) as nrows from ",
                                table_name, sep = ""))
  dr[1, 1]
}
