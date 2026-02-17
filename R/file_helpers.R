#' Preview the table structure contained in a Feather file.
#' 
#' @description
#' The `file_schema_feather()` function returns a data frame with the
#' schema of a Feather file. This function is used to preview the table
#' structure contained in a Feather file, by reading only the metadata of
#' the file. It inspects the input file metadata to read the field identifiers'
#' names and data types, then converts them to the candidate data frame
#' columns' names and data types. The dataset contained in the input file
#' is not read in to memory, only meta-data are accessed.
#'
#' @param input_file File name (including path) to be read
#' @param id_quote_method character, used to specify how to build the SQLite
#'    columns' names using the fields' identifiers read from the input file.
#'    For details see the description of the `quote_method` parameter of
#'    the [format_column_names()] function. Defaults to `DB_NAMES`.
#'
#' @returns a data frame with these columns:
#'    - `col_names`: columns' names, after applying the selected quote method;
#'    - `col_names_unquoted`: columns' names, unquoted; if `id_quote_method`
#'      is set to `DB_NAMES` they will be the same as `col_names`; for other
#'      quote methods they will be the unquoted versions of `col_names`, that
#'      is generally the same as `src_names` unless `src_names` contain the
#'      quoting characters;
#'    - `col_types`: columns' R data types;
#'    - `sql_types`: columns' SQLite data types;
#'    - `src_names`: columns' names as they appear in the input file;
#'    - `src_types`: the Arrow's data type of each column.
#'
#' @section References:
#' The implementation is based on this question on
#' [Stackoverflow](https://stackoverflow.com/questions/66529055/how-to-read-column-names-and-metadata-from-feather-files-in-r-arrow). # nolint: line_length_linter.
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

  dnames <- format_column_names(x = src_names,
                                quote_method = id_quote_method,
                                unique_names = TRUE)
  col_names <- dnames$quoted
  col_names_unquoted <- dnames$unquoted

  ss <- lapply(ft$schema$fields, FUN = function(x) `$`(x, "type"))
  src_types <- sapply(ss, FUN = function(x) `$`(x, "name"))

  rf$close()

  data.frame(col_names, col_names_unquoted,
             col_types = Arrow2R_types(src_types),
             sql_types = R2SQL_types(Arrow2R_types(src_types)),
             src_names,
             src_types,
             stringsAsFactors = FALSE,
             row.names = NULL)
}


#' Preview the table structure contained in a DSV file. 
#' 
#' @description 
#' The `file_schema_dsv()` function returns a data frame with the schema
#' of a DSV file reading only the first `max_lines` of a delimiter
#' separated values (DSV) text file to infer column names and data types
#' (it does not read the full dataset into memory). Then it converts them
#' to the candidate data frame columns' names and data types.
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
#'    all rows with `n_cols` columns (i.e. the guessed number of columns
#'    determined inspecting the first `max_lines` lines of the input file),
#'    even if there are rows in the input file with fewer or greater columns
#'    than `n_cols`.
#'    If `FALSE` and any of the tested lines has a number of columns not
#'    equal to `n_cols`, the function will return a list without the `schema`
#'    element. It defaults to `TRUE`.
#' @param ... Additional arguments for quoting and data interpretation as
#'    described in the [base::scan()] function. The parameters used
#'    by `file_schema_dsv` are:
#'    - `quote`, character, the set of quoting characters. Defaults to `""`
#'      (i.e., no quoting).
#'    - `comment.char`, character, the comment character. Defaults to `""`
#'      (i.e., no comments).
#'    - `skip`, integer, the number of lines to skip before reading data.
#'      Defaults to `0`.
#'    - `fileEncoding`, character, the name of the encoding of the input file.
#'      Defaults to `""`.
#'    - `na.strings`, character vector, the strings to be interpreted as NAs.
#'      Defaults to `c("NA")`.
#'
#' @returns a list with the following named elements:
#'    - `schema`, a data frame with these columns:
#'      - `col_names`: columns' names, after applying the selected quote method;
#'      - `col_names_unquoted`: columns' names, unquoted; if `id_quote_method`
#'        is set to `DB_NAMES` they will be the same as `col_names`; for other
#'        quote methods they will be the unquoted versions of `col_names`,that
#'        is generally the same as `src_names` unless `src_names` contain the
#'        quoting characters;
#'      - `col_types`: columns' R data types;
#'      - `sql_types`: columns' SQLite data types;
#'      - `src_names`: columns' names as they appear in the input file.
#'      - `src_types`: defaults to `text` for all columns.
#'      - `src_is_quoted`: logical vector indicating if each column has at least
#'         one value enclosed in quotes.
#'    - `col_counts`, a data frame with these columns:
#'      - `num_col`: number of columns,
#'      - `Freq`: number of rows (within `max_lines`) that have the number
#'         of colums shown in `num_col`.
#'    - `n_cols`, integer, the number of columns selected for the file.
#'    - `num_col`, a vector of integers of length `max_lines` with the
#'       number of detected columns in each row tested.
#'    - `col_fill`, logical, it is set to `TRUE` if there are lines with
#'       less columns than `n_cols`.
#'    - `col_flush`, logical, it is set to `TRUE` if there are lines with
#'       more columns than `n_cols`.
#'
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

  if (!("na.strings" %in% names(lpar))) {
    lpar$na.strings <- "NA"
  }

  ## reading all lines -----------------------------------------
  text_con <- file(input_file, open = "r",
                   encoding = lpar$fileEncoding)
  on.exit(close(text_con), add = TRUE)

  text <- readLines(con = text_con, n = max_lines,
                    ok = TRUE, skipNul = TRUE)

  if (lpar$comment.char != "") {
    rx_comment <- paste0("^", lpar$comment.char, ".*")
    text <- gsub(rx_comment, "", text, perl = TRUE)
  }

  idx <- which(nchar(text) == 0)
  if (length(idx) > 0)
    text <- text[-idx]

  if (lpar$skip > 0) {
    text <- text[-(1:lpar$skip)]
  }

  ## clean sep character from quoted columns -------------------
  if (lpar$quote == "" || length(lpar$quote) == 0) {
    text2 <- text

  } else {
    rx <- paste0("([", lpar$quote, "])(?:\\\\.|(?!\\1).)*\\1")

    list_matches <- gregexpr(rx, text, perl = TRUE)

    text2 <- character(length(text))
    for (ii in seq_along(list_matches)) {

      if (list_matches[[ii]][1] > 0) {

        out_text <- ""

        for (jj in seq_along(list_matches[[ii]])) {

          match_start <- list_matches[[ii]][jj]
          if (jj == 1 && match_start > 1) {
            pre_text <- substr(text[ii], 1, match_start - 1)
          } else {
            pre_text <- ""
          }

          match_length <- attr(list_matches[[ii]], "match.length")[jj]
          match_text <- substr(text[ii], match_start,
                              match_start + match_length - 1)
          match_text <- gsub(sep,  "", match_text, fixed = TRUE)
          if (lpar$comment.char != "") {
            match_text <- gsub(lpar$comment.char,  "", match_text, fixed = TRUE)
          }
          out_text <- paste0(out_text, pre_text, match_text)

          if (jj <= length(list_matches[[ii]]) - 1) {
            post_start <- match_start + match_length
            if (post_start != list_matches[[ii]][jj + 1]) {
              pos_length <- list_matches[[ii]][jj + 1] - post_start
              post_text <- substr(text[ii], post_start,
                                  post_start + pos_length - 1)
              out_text <- paste0(out_text, post_text)
            }
          } else if (jj == length(list_matches[[ii]])) {
            post_start <- match_start + match_length
            if (post_start <= nchar(text[ii])) {
              post_text <- substr(text[ii], post_start, nchar(text[ii]))
              out_text <- paste0(out_text, post_text)
            }
          }
        }

      }  else {
        out_text <- text[ii]
      }
      text2[ii] <- out_text
    }
  }

  if (lpar$comment.char != "") {
    rx_comment <- paste0(lpar$comment.char, ".*")
    text2 <- gsub(rx_comment, "", text2, perl = TRUE)
  }

  ## splitting lines into columns ------------------------------
  list_split <- strsplit(text2, split = sep, fixed = TRUE)

  num_col <- sapply(list_split, length, simplify = TRUE)
  col_counts <- as.data.frame(table(num_col), stringsAsFactors = FALSE)
  col_counts$num_col <- as.integer(col_counts$num_col)

  if (min(col_counts$num_col) <= 0)
    stop("file_schema_dsv: no columns found in file.")

  col_flush <- FALSE
  col_fill <- FALSE

  if (nrow(col_counts) == 1) {
    n_cols <- col_counts$num_col[1]
  } else {
    idx <- which(col_counts$Freq == max(col_counts$Freq))
    n_cols <- max(col_counts$num_col[idx])
    max_cols <- max(col_counts$num_col)
    min_cols <- min(col_counts$num_col)

    if (n_cols > min_cols)
      col_fill <- TRUE
    if (n_cols < max_cols)
      col_flush <- TRUE

    if (force_num_cols == FALSE) {
      return(
        list(col_counts = col_counts,
             n_cols = n_cols,
             num_col = num_col,
             col_fill = col_fill,
             col_flush = col_flush)
      )
    }
  }

  ## align rows and map text in a data frame -------------------
  align_columns <- function(x, n_cols) {
    if (length(x) > n_cols) {
      x  <- x[1:n_cols]
    } else if (length(x) < n_cols) {
      x <- c(x, rep(NA, n_cols - length(x)))
    }
    x <- gsub("^\\s+|\\s+$", "", x)
    x
  }

  list_split_fixed <- sapply(list_split, align_columns, n_cols = n_cols)
  df <- as.data.frame(t(list_split_fixed), stringsAsFactors = FALSE)

  ## header and column names ------------------------------------
  if (header) {
    src_names <- as.vector(df[1, ], mode = "character")
    src_names <- gsub(paste0("^[", lpar$quote, "]+|[", lpar$quote, "]+$"), "",
                      src_names)
    df <- df[-1, ]
    idx <- which(is.na(src_names) | src_names == "")

    if (length(idx) > 0) {
      src_names[idx] <- paste0("X_", idx)

    } else if (length(src_names) > n_cols) {
      src_names <- src_names[1:n_cols]
    }

  } else {
    src_names <- paste0("V", 1:n_cols)

  }
  names(df) <- src_names

  dnames <- format_column_names(x = src_names,
                                quote_method = id_quote_method,
                                unique_names = TRUE,
                                encoding = lpar$fileEncoding)
  col_names <- dnames$quoted
  col_names_unquoted <- dnames$unquoted

  ## find quoted columns ------------------------------------
  check_quotes <- function(text_line, quote, n_cols) {
    if (quote == "" || length(quote) == 0) {
      return(rep(FALSE, n_cols))
    }

    rx_quote <- paste0("(\\s*)([", quote, "])(?:\\\\.|(?!\\2).)*\\2(\\s*)")
    matches <- grep(rx_quote, text_line, perl = TRUE)
    no_matches <- setdiff(seq_along(text_line), matches)

    quoted_line <- rep(NA, n_cols)
    if (length(matches) > 0) {
      quoted_line[matches] <- TRUE
    }
    if (length(no_matches) > 0) {
      quoted_line[no_matches] <- FALSE
    }
    quoted_line
  }

  is_quoted_matrix <- sapply(df, check_quotes,
                             quote = lpar$quote, n_cols = n_cols)

  is_quoted <- apply(X = is_quoted_matrix, MARGIN = 2,
                     FUN = function(x) any(x, na.rm = TRUE))

  ## infer column types ----------------------------------------
  col_types <- vapply(df, function(col) class(col)[1], character(1))

  if (length(df) > 0) {
    idx <- which(is_quoted == FALSE)
    for (ii in idx) {
      test <- df[, ii]

      test <- gsub("^\\s+|\\s+$", "", test)

      id1 <- which(is.na(test))
      id2 <- which(test %in% lpar$na.strings)
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
        }

        if (max(nchar(test1)) > 512) {
          test1 <- substr(test1, 1, 512)
        }
        date_format <- c("%Y-%m-%d")
        if (!any(is.na(suppressWarnings(as.Date(test1, optional = TRUE,
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
    col_names, col_names_unquoted, col_types,
    sql_types = R2SQL_types(col_types),
    src_names,
    src_types = c("text"),
    src_is_quoted = is_quoted,
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  list(
    schema = schema,
    col_counts = col_counts,
    n_cols = n_cols,
    num_col = num_col,
    col_fill = col_fill,
    col_flush = col_flush
  )
}


#' Preview the structure of a range of an Excel worksheet.
#' 
#' @description
#' The `file_schema_xlsx()` function returns a data frame with the
#' schema of an Excel data table. It will read only a range of
#' the specified worksheet to infer column names and data types.
#' Then it converts them to the candidate data frame columns' names
#' and data types.
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
#'    the [format_column_names()] function. Defaults to `DB_NAMES`.
#' @param max_lines integer, number of lines (excluding the header) to be
#'    read to infer columns' data types. Defaults to 100.
#' @param null_columns logical, if `TRUE` the col_type of columuns consisting
#'    only of NAs or zero-length strings will be marked as `NA`, otherwise they
#'    will be marked as `character`. Defaults to `FALSE`
#' @param ...  Additional parameters passed to [openxlsx2::wb_to_df()]
#'    function.
#'
#' @returns a data frame with these columns:
#'    - `col_names`: columns' names, after applying the selected quote method;
#'    - `col_names_unquoted`: columns' names, unquoted; if `id_quote_method`
#'      is set to `DB_NAMES` they will be the same as `col_names`; for other
#'      quote methods they will be the unquoted versions of `col_names`,that
#'      is generally the same as `src_names` unless `src_names` contain the
#'      quoting characters;
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

  ## ---------------------------------------------
  lpar <- list(...)

  allowed_params <- c(
    "start_col", "row_names", "skip_empty_rows",
    "skip_empty_cols", "skip_hidden_rows",
    "skip_hidden_cols", "detect_dates", "na.strings",
    "na.numbers", "fill_merged_cells", "dims",
    "show_formula", "convert", "types", "named_region"
  )

  lpar <- lpar[which(names(lpar) %in% allowed_params)]

  lpar1 <- append(
    x = lpar,
    values = list(
      file = input_file,
      sheet = sheet_name,
      start_row = first_row,
      col_names = header,
      cols = cols_range,
      rows = c(first_row:(first_row + max_lines)),
      keep_attributes = TRUE
    ),
    after = 0
  )

  df <- do.call(openxlsx2::wb_to_df, lpar1)

  xt <- attr(df, "types")
  src_names <- names(df)
  col_types <- vapply(df, function(col) class(col)[1], character(1))

  dnames <- format_column_names(x = src_names,
                                quote_method = id_quote_method,
                                unique_names = TRUE)

  col_names <- dnames$quoted
  col_names_unquoted <- dnames$unquoted

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

  data.frame(
    col_names, col_names_unquoted,
    col_types,
    sql_types = R2SQL_types(col_types),
    src_names,
    src_types = paste0(xt, ": ", Xlsx2R_types(xt)),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

}
