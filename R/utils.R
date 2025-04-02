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
    r2sql_dict <- c("character"="TEXT",  "double"="REAL", "integer"="INTEGER",
                    "logical"="INTEGER", "numeric"="REAL", "Date"="DATE")
    
    y <- r2sql_dict[x]
    y[which(is.na(y))] <- "TEXT"

    y    
}


#' format_field_names Format a vector of strings so that they can be
#'   used as field names for a table in a database
#'
#' @param x character vector with the identifier names to be quoted.
#' @param quote_method used to specify how to build the SQLite table's
#'    field names from the column identifiers read from `input_file`.
#'    It can assume the following values:
#'    - `DB_NAMES` tries to build a valid field name:
#'      a. substituting all characters, that are not letters or digits or
#'         the `_` character, with the `_` character;
#'      b. prefixing `N_` to all strings starting with a digit;
#'      c. prefixing `F_` to all strings equal to a SQL92 keyword.
#'    - `SINGLE_QUOTES` encloses each string in single quotes.
#'    - `DOUBLE_QUOTES` encloses each string in double quotes.
#'    - `SQL_SERVER` encloses each string in square brackets.
#'    - `MYSQL` encloses each string in back ticks.
#'    Defaults to `DB_NAMES`.
#' @param unique_names Logical, checks for any duplicate name after
#'    applying the selected quote methods. If duplicates exist, they
#'    will be made unique by adding a postfix `_[n]`, where `n` is
#'    a progessive integer.
#' 
#' @importFrom DBI .SQL92Keywords
#' 
format_field_names <- function(x, quote_method="DB_NAMES", unique_names=TRUE) {
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
        
    } else if (quote_method=="DOUBLE_QUOTES") {
        x1 <- gsub(pattern="\"", replacement="'", x=x1)
        x1 <- paste0("\"", x1, "\"")
        
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
