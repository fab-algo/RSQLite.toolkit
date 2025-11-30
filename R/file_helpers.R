#' file_schema_feather returns a data frame with the schema of a Feather file
#'
#' Preview the table structure contained in a Feather file. This function
#' inspects the input file metadata to read the field identifiers' names and
#' data types, then converts them to the candidate data frame columns' names
#' and data types. The dataset contained in the input file is not read in to
#' memory, only meta-data are accessed.
#'
#' @param input_file File name (including path) to be read
#' @param id_quote_method character, used to specify how to build the SQLite
#'    columns' names using the fields' identifiers read from the input file.
#'    For details see the description of the `quote_method` parameter of
#'    the [format_column_names()] function. Defautls to `DB_NAMES`.
#'
#' @returns a data frame with these columns:
#'    - `col_names`: columns' names, after applying the selected quote method;
#'    - `col_types`: columns' R data types;
#'    - `sql_types`: columns' SQLite data types;
#'    - `src_names`: columns' names as they appear in the input file;
#'    - `src_types`: the Arrow's data type of each column.
#'
#' @section References:
#' The implementation is based on this question on
#' [Stackoverflow](https://stackoverflow.com/questions/66529055/how-to-read-column-names-and-metadata-from-feather-files-in-r-arrow).
#'
#' @importFrom arrow ReadableFile
#' @importFrom arrow FeatherReader
#' @export
file_schema_feather <- function(input_file, id_quote_method = "DB_NAMES") {

  if (!file.exists(input_file)) {
    stop("file_schema_feather: File does not exist: ", input_file)
  }

  rf <- arrow::ReadableFile$create(input_file)
  ft <- arrow::FeatherReader$create(rf)

  src_names <- ft$column_names

  col_names <- format_column_names(x = src_names,
                                   quote_method = id_quote_method,
                                   unique_names = TRUE)

  ss <- lapply(ft$schema$fields, FUN = function(x) `$`(x, "type"))
  src_types <- sapply(ss, FUN = function(x) `$`(x, "name"))

  rf$close()

  data.frame(col_names,
             col_types = Arrow2R_types(src_types),
             sql_types = R2SQL_types(Arrow2R_types(src_types)),
             src_names,
             src_types,
             stringsAsFactors = FALSE,
             row.names = NULL)
}


#' file_schema_dsv returns a data frame with the schema of a DSV file
#'
#' Reads only the first `max_lines` of a delimiter separated values (DSV)
#' text file to infer column names and data types, without reading the full
#' dataset into memory. Then it converts them to the candidate data
#' frame columns' names and data types.
#'
#' @param input_file character, file name (including path) to be read.
#' @param header logical, if `TRUE` the first line contains the fields'
#'    names. If `FALSE`, the column names will be formed sing a "V"
#'    followed by the column number (as specified in [utils::read.table()]).
#' @param sep character, field delimiter (e.g., "," for CSV, "\\t" for TSV)
#'    in the input file. Defaults to ",".
#' @param dec character, decimal separator (e.g., "." or "," depending on
#'    locale) in the input file. Defaults to ".".
#' @param grp character, character used for digit grouping. It defaults
#'    to `""` (i.e. no grouping).
#' @param id_quote_method character, used to specify how to build the SQLite
#'    columns' names using the fields' identifiers read from the input file.
#'    For details see the description of the `quote_method` parameter of
#'    the [format_column_names()] function. Defautls to `DB_NAMES`.
#' @param max_lines integer, number of lines (excluding the header) to be
#'    read to infer columns' data types. Defaults to 2000.
#' @param null_columns logical, if `TRUE` the col_type of columuns consisting
#'    only of NAs or zero-length strings will be marked as `"NULL"`, otherwise
#'    they will be marked as `character`. Defaults to `FALSE`
#' @param force_num_cols logical, if `TRUE` the returned schema will have
#'    `n_cols` rows (i.e. the guessed number of columns determined inspecting
#'     the first `max_lines` lines of the input file), even if there are rows
#'     in the input file with fewer or greater columns than `n_cols`.
#'     If `FALSE` and any of the tested lines has a number of columns not
#'     equal to `n_cols`, the function will return a list without the `schema`
#'     element. It defaults to `TRUE`.
#' @param ... Additional arguments passed to [utils::read.table()].
#'
#' @returns a list with the following named elements:
#'    - `schema`, a data frame with these columns:
#'      - `col_names`: columns' names, after applying the selected quote method;
#'      - `col_types`: columns' R data types;
#'      - `sql_types`: columns' SQLite data types;
#'      - `src_names`: columns' names as they appear in the input file.
#'      - `src_types`: dafults to `text` for all columns.
#'    - `col_counts`, a data frame with these columns:
#'      - `Num_col`: number of columns,
#'      - `Freq`: number of rows (within `max_lines`) that have the number
#'         of colums shown in `Num_col`.
#'    - `n_cols`, integer, the number of columns selected for the file.
#'    - `Num_col`, a vector of integers of length `max_lines` with the
#'       number of detected columns in each row tested.
#'    - `col_fill`, logical, it is set to `TRUE` if there are lines with
#'       less columns than `n_cols`.
#'    - `col_flush`, logical, it is set to `TRUE` if there are lines with
#'       more columns than `n_cols`.
#'
#' @importFrom utils read.table
#' @export
#'
#' @examples
#' \dontrun{
#' file_schema_dsv("data.csv", sep=",", dec=".", max_lines=50)
#' file_schema_dsv("euro.csv", sep=";", dec=",", header=FALSE)
#' }
file_schema_dsv <- function(input_file,
                            header = TRUE, sep = ",", dec = ".", grp = "",
                            id_quote_method = "DB_NAMES",
                            max_lines = 2000,
                            null_columns = FALSE,
                            force_num_cols = TRUE,
                            ...) {

  if (!file.exists(input_file)) {
    stop("file_schema_dsv: File does not exist: ", input_file)
  }

  if (any(is.na(grp)) || is.null(grp) ||
        !("character" %in% class(grp)) || length(grp) == 0 ||
        length(grp) > 1 || nchar(grp[1]) > 1) {
    stop("file_schema_dsv: error in 'grouping_char' parameter: ",
         paste(paste("\"", grp, "\"", sep = ""), collapse = ", "))
  }


  ## ---------------------------------------------
  lpar <- eval(substitute(alist(...)))

  if (!("comment.char" %in% names(lpar))) {
    lpar$comment.char <- ""
  }

  if (!("skip" %in% names(lpar))) {
    lpar$skip <- 0
  }

  if (!("fileEncoding" %in% names(lpar))) {
    lpar$fileEncoding <- ""
    fileEncoding <- ""
  } else {
    fileEncoding <- eval(lpar$fileEncoding)
  }



  ## ---------------------------------------------
  Num_col <- integer(max_lines)

  allowed_par_scan <- c("quote", "na.strings", "strip.white",
                        "blank.lines.skip", "comment.char",
                        "allowEscapes", "fileEncoding", "encoding",
                        "skipNul", "skip")

  lpar_scan <- lpar
  lpar_scan <- lpar_scan[which(names(lpar_scan) %in% allowed_par_scan)]


  fcon <- file(description = input_file, open = "r",
               blocking = FALSE)

  for (kk in 1:max_lines) {
    if (kk > 1)
      lpar_scan$skip <- 0

    lpar1 <- append(x = lpar_scan,
                    values = list(
                      file = fcon,
                      nlines = 1,
                      sep = sep,
                      what = "",
                      flush = FALSE,
                      fill = FALSE,
                      multi.line = FALSE,
                      quiet = TRUE
                    ),
                    after = 0)
    x <- do.call(scan, lpar1)
    if (length(x) == 0) {
      kk <- kk - 1
      break
    }

    Num_col[kk] <- length(x)

    if (kk == 1)
      raw_names <- x

  }

  close(fcon)

  Num_col <- Num_col[1:kk]
  col_counts <- as.data.frame(table(Num_col), stringsAsFactors = FALSE)
  col_counts$Num_col <- as.integer(col_counts$Num_col)

  if (min(col_counts$Num_col) <= 0)
    stop("file_schema_dsv: no columns found in file.")

  col_flush <- FALSE
  col_fill <- FALSE

  if (nrow(col_counts) == 1) {
    n_cols <- col_counts$Num_col[1]
  } else {
    idx <- which(col_counts$Freq == max(col_counts$Freq))
    n_cols <- max(col_counts$Num_col[idx])
    max_cols <- max(col_counts$Num_col)
    min_cols <- min(col_counts$Num_col)

    if (n_cols > min_cols)
      col_fill <- TRUE
    if (n_cols < max_cols)
      col_flush <- TRUE

    if (force_num_cols == FALSE) {
      return(
        list(
             col_counts = col_counts,
             n_cols = n_cols,
             Num_col = Num_col,
             col_fill = col_fill,
             col_flush = col_flush)
      )
    }
  }

  if (header) {
    src_names <- raw_names

    if (length(src_names) < n_cols) {
      add_cols <- paste0("X_", (length(src_names) + 1):n_cols)
      src_names <- c(src_names, add_cols)

    } else if (length(src_names) > n_cols) {
      src_names <- src_names[1:n_cols]
    }

    header <- FALSE
    lpar$skip <- lpar$skip + 1

  } else {
    src_names <- paste0("V", 1:n_cols)

  }

  col_classes <- rep("character", n_cols)

  col_names <- format_column_names(x = src_names,
                                   quote_method = id_quote_method,
                                   unique_names = TRUE,
                                   encoding = fileEncoding)
  names(col_classes) <- col_names


  allowed_par_read <- c("quote", "na.strings", "skip",
                        "strip.white", "blank.lines.skip",
                        "comment.char", "allowEscapes",
                        "fileEncoding",
                        "encoding", "skipNul", "numerals",
                        "as.is", "tryLogical", "check.names")

  lpar_read <- lpar
  lpar_read <- lpar_read[which(names(lpar_read) %in% allowed_par_read)]

  lpar_read$fill <- col_fill
  lpar_read$flush <- col_flush

  lpar1 <- append(x = lpar_read,
                  values = list(
                    file = input_file,
                    header = header,
                    sep = sep,
                    dec = dec,
                    nrows = max_lines,
                    col.names  = col_names,
                    colClasses = col_classes,
                    stringsAsFactors = FALSE,
                    row.names = NULL
                  ),
                  after = 0)
  df <- do.call(read.table, lpar1)

  col_types <- vapply(df, function(col) class(col)[1], character(1))

  date_format <- c("%Y-%m-%d")
  if (length(df) > 0) {
    for (ii in seq_along(df)) {
      test <- df[, ii]

      test <- gsub("^\\s+|\\s+$", "", test)

      id1 <- which(is.na(test))
      id2 <- which(test == "")
      if (length(c(id1, id2)) > 0) {
        test <- test[-c(id1, id2)]
      }

      if (length(test) > 0) {
        check1 <-  grep(pattern = "[.\\|()[{^$*+?]", x = dec)
        if (length(check1) > 0) {
          pd <- paste0("\\", dec)
        } else {
          pd <- dec
        }

        if (grp == "") {
          test1 <- gsub(pattern = pd, replacement = ".", x = test)
          grp_suffix <- ""

        } else {
          check2 <-  grep(pattern = "[.\\|()[{^$*+?]", x = grp)
          if (length(check2) > 0) {
            pg <- paste0("\\", grp)
          } else {
            pg <- grp
          }

          test1 <- gsub(pattern = pd, replacement = ".",
            x = gsub(pattern = pg, replacement = "", x = test)
          )
          grp_suffix <- "_grouped"
        }

        if (all(grepl(pattern = "^[\\+\\-]{0,1}[0-9]+$", x = test1))) {
          col_types[ii] <- paste0("integer", grp_suffix)
        } else if (!any(is.na(suppressWarnings(as.numeric(test1))))) {
          col_types[ii] <- paste0("numeric", grp_suffix)
        } else if (!any(is.na(suppressWarnings(as.Date(test1, optional = TRUE,
                                                       format = date_format))))
        ) {
          col_types[ii] <- "Date"
        }

      } else {
        if (null_columns == TRUE) {
          col_types[ii] <- "NULL"
        }
      }
    }
  }

  schema <- data.frame(
    col_names, col_types,
    sql_types = R2SQL_types(col_types),
    src_names,
    src_types = c("text"),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  list(
    schema = schema,
    col_counts = col_counts,
    n_cols = n_cols,
    Num_col = Num_col,
    col_fill = col_fill,
    col_flush = col_flush
  )
}



#' file_schema_xlsx returns a data frame with the schema of an Excel data table
#'
#' Preview the table structure contained in a rectangular range of worksheet
#' of an Excel file.
#'
#' @param input_file character, file name (including path) to be read.
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
#' @param id_quote_method character, used to specify how to build the SQLite
#'    columns' names using the fields' identifiers read from the input file.
#'    For details see the description of the `quote_method` parameter of
#'    the [format_column_names()] function. Defautls to `DB_NAMES`.
#' @param max_lines integer, number of lines (excluding the header) to be
#'    read to infer columns' data types. Defaults to 100.
#' @param null_columns logical, if `TRUE` the col_type of columuns consisting
#'    only of NAs or zero-length strings will be marked as `NA`, otherwise they
#'    will be marked as `character`. Defaults to `FALSE`
#' @param ...  parameters passed to [openxlsx2::wb_to_df()] function.
#'
#' @returns a data frame with these columns:
#'    - `col_names`: columns' names, after applying the selected quote method;
#'    - `col_types`: columns' R data types;
#'    - `sql_types`: columns' SQLite data types;
#'    - `src_names`: columns' names as they appear in the input file;
#'    - `src_types`: data type attribute of each column, as determined by the
#'       [openxlsx2::wb_to_df()] function.
#'
#' @importFrom openxlsx2 wb_to_df
#' @export
file_schema_xlsx <- function(input_file,
                             sheet_name, first_row, cols_range, header = TRUE,
                             id_quote_method = "DB_NAMES",
                             max_lines = 100,
                             null_columns = FALSE,
                             ...) {

  if (!file.exists(input_file)) {
    stop("file_schema_xlsx: File does not exist: ", input_file)
  }

  df <- openxlsx2::wb_to_df(
    file = input_file,
    sheet = sheet_name,
    start_row = first_row,
    col_names = header,
    cols = cols_range,
    rows = c(first_row:(first_row + max_lines)),
    ...
  )

  src_names <- names(df)
  col_types <- vapply(df, function(col) class(col)[1], character(1))

  col_names <- format_column_names(x = src_names,
                                   quote_method = id_quote_method,
                                   unique_names = TRUE)

  if (length(df) > 0) {
    for (ii in seq_along(df)) {
      test <- df[, ii]

      id1 <- which(is.na(test))
      id2 <- which(test == "")
      if (length(c(id1, id2)) > 0) {
        test <- test[-c(id1, id2)]
      }

      if (length(test) == 0 && null_columns == TRUE) {
        col_types[ii] <- NA
      }
    }
  }

  xt <- attr(df, "types")

  data.frame(
    col_names,
    col_types,
    sql_types = R2SQL_types(col_types),
    src_names,
    src_types = paste0(xt, ": ", Xlsx2R_types(xt)),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

}
