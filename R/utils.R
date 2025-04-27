#' error_handler manage error messages for package
#'
#' @param err character, error message
#' @param fun character, function name where error happened
#' @param step integer, code identifying the step in the function
#'   where error happened.
#'   For dbTableFrom... functions steps are:
#'   - 101,121: read file schema (DSV, Xlsx)
#'   - 102: handle col_names and col_types
#'   - 103: create empty table
#'   - 104: read data
#'   - 105: write data
#'   - 106: indexing
#'
#' @returns nothing
#' 
#'
error_handler <- function(err, fun, step) {

    if (step == 101) {
        step_msg <- paste0("reading file schema: \n",
                           "please check 'sep', 'dec', 'grp' ",
                           "params and those used by 'scan' ",
                           "and 'read.table' for quoting, ",
                           "encoding, ...")
        
    } else if (step == 121) {
        step_msg <- paste0("reading file schema: \n",
                           "please check file name and ",
                           "'sheet_name', 'first_row', ",
                           "'cols_range' params.")
        
    } else if (step == 131) {
        step_msg <- paste0("reading file schema: \n",
                           "please check file name and ",
                           "file format.")
        
    } else if (step == 102) {
        step_msg <- paste0("handling 'col_names' and ",
                           "'col_types' parameters.")
        
    } else if (step == 103) {
        step_msg <- paste0("creating empty table in db.")
        
    } else if (step == 104) {
        step_msg <- paste0("reading data from input file.")
        
    } else if (step == 105) {
        step_msg <- paste0("writing data to db table.")
        
    } else if (step == 106) {
        step_msg <- paste0("indexing db table.")
    }

    msg <- paste0("Blocking error in ", fun, " while ", step_msg,
                  "\n", "Original error msg: ", err)
    msg
}



Arrow2R_types <- function(x) {
    arrow2r_dict <- c(
        "boolean"   ="logical",
        "int8"      ="integer",
        "int16"     ="integer",
        "int32"     ="integer",
        "int64"     ="integer",
        "uint8"     ="integer",
        "uint16"    ="integer",
        "uint32"    ="integer",
        "uint64"    ="integer",
        "float16"   =NA,
        "float32"   ="double",
        "float64"   ="double",
        "decimal"   ="double",
        "double"    ="double",
        "utf8"      ="character",
        "large_utf8"="character",
        "binary"           =NA,
        "large_binary"     =NA,
        "fixed_size_binary"=NA,
        "date32"    ="Date",
        "date64"    ="POSIXct",
        "time32"    =NA,
        "time64"    =NA,
        "timestamp" ="POSIXct",
        "duration"  ="difftime",
        "dictionary"="character",
        "list"      =NA,
        "large_list"=NA,
        "fixed_size_list"=NA,
        "struct"    =NA,
        "null"      =NA,
        "map"       =NA,
        "union"     =NA
    )

    y <- arrow2r_dict[x]

    y
}


R2SQL_types <- function(x) {
    r2sql_dict <- c("character"= "TEXT",
                    "double"   = "REAL",
                    "integer"  = "INTEGER",
                    "logical"  = "INTEGER",
                    "numeric"  = "REAL",
                    "Date"     = "DATE",
                    "double_grouped"   = "REAL",
                    "integer_grouped"  = "INTEGER",
                    "numeric_grouped"  = "REAL")
    
    y <- r2sql_dict[x]
    y[which(is.na(y))] <- "TEXT"

    y    
}

convert_grouped_digits <- function(x, to, dec, grp) {

    check1 <-  grep(pattern="[.\\|()[{^$*+?]", x=grp)
    if (length(check1) > 0) {
        pg <- paste0("\\", grp)
    } else {
        pg <- grp
    }
    
    check2 <-  grep(pattern="[.\\|()[{^$*+?]", x=dec)
    if (length(check1) > 0) {
        pd <- paste0("\\", dec)
    } else {
        pd <- dec
    }
    
    y <- gsub(pattern = pd, replacement = ".", 
              x=gsub(pattern = pg, replacement = "", x = x)
              )
    if (to %in% c("numeric", "double")) {
        y <- as.numeric(y)
    } else if (to == "integer") {
        y <- as.integer(y)
    }

    y
}


#' format_column_names format a vector of strings to be
#'   used as columns' names for a table in a SQLite database
#'
#' @param x character vector with the identifiers' names to be quoted.
#' @param quote_method character, used to specify how to build the SQLite
#'    columns' names from the identifiers passed through the `x`
#'    parameter.
#'    Supported values for `quote_method`:
#'    - `DB_NAMES` tries to build a valid SQLite column name:
#'      a. substituting all characters, that are not letters or digits or
#'         the `_` character, with the `_` character;
#'      b. prefixing `N_` to all strings starting with a digit;
#'      c. prefixing `F_` to all strings equal to any SQL92 keyword.
#'    - `SINGLE_QUOTES` encloses each string in single quotes.
#'    - `SQL_SERVER` encloses each string in square brackets.
#'    - `MYSQL` encloses each string in back ticks.
#' 
#'    Defaults to `DB_NAMES`.
#' @param unique_names logical, checks for any duplicate name after
#'    applying the selected quote methods. If duplicates exist, they
#'    will be made unique by adding a postfix `_[n]`, where `n` is
#'    a progressive integer. Defaults to `TRUE`.
#'
#' @returns A character vector containing the columns' identifiers
#' 
#' @importFrom DBI .SQL92Keywords
#' @export
#' 
format_column_names <- function(x, quote_method="DB_NAMES", unique_names=TRUE) {
    allowed_methods <- c("DB_NAMES",
                         "SINGLE_QUOTES", "DOUBLE_QUOTES",
                         "SQL_SERVER",    "MYSQL")

    if (!quote_method %in% allowed_methods) {
        stop("RSQLite.toolkit: error in quote_method: ", quote_method, " unknown." )
    }

    x1 <- x
    
    if (quote_method=="DB_NAMES") {
        reg1 <- "([^[:alpha:]0-9_]+)"
        x1 <- gsub(pattern=reg1, replacement="_", x=x1)

        reg2 <- "(^[0-9])"
        idx <- grep(pattern=reg2, x=x1)
        if (length(idx)>0) {
            x1[idx] <- paste0("N_", x1[idx])
        }
        
        reg3 <- "(^sqlite_)"
        x1 <- gsub(pattern=reg3, replacement="", x=x1)

        idx <- which(x1 %in% DBI::.SQL92Keywords)
        if (length(idx)>0) {
            x1[idx] <- paste0("F_", x1[idx])
        }
        
    } else if (quote_method=="SINGLE_QUOTES") {
        x1 <- gsub(pattern="'", replacement="\"", x=x1)
        x1 <- paste0("'", x1, "'")
        
        ## } else if (quote_method=="DOUBLE_QUOTES") {
        ##     x1 <- gsub(pattern="\"", replacement="'", x=x1)
        ##     x1 <- paste0("\"", x1, "\"")
        
    } else if (quote_method=="SQL_SERVER") {
        x1 <- gsub(pattern="[\\[\\]]", replacement="_", x=x1)
        x1 <- paste0("[", x1, "]")
        
    } else if (quote_method=="MYSQL") {
        x1 <- gsub(pattern="`", replacement="'", x=x1)
        x1 <- paste0("`", x1, "`")
    }
    

    if (unique_names) {
        idx <- which(x1 %in% x1[duplicated(x1)])
        if (length(idx)>0) {
            id <- c(1:length(idx))
            x1[idx] <- paste0(x1[idx], "_", id)
            
        }
    }

    x1
}
