SQLtype <- function(x) {
    if (x == "character") {
        fld.type <- "TEXT"
    } else if (x == "double") {
        fld.type <- "REAL"
    } else if (x == "integer") {
        fld.type <- "INTEGER"
    } else if (x == "Date") {
        fld.type <- "DATE"
    } else {
        fld.type <- "TEXT"
    }
    fld.type
}


format_field_names <- function(x) {
    x <- gsub(pattern="\\+|\\-", replacement="", x=x)
    x <- gsub(pattern="\\(|\\)|\\[|\\]|\\{|\\}]", replacement="", x=x)
    x <- gsub(pattern="\\!|\\?|\\*|\\&|\\%|\\^]", replacement="", x=x)
    x <- gsub(pattern="\\s+", replacement="_", x=x)
    x <- gsub(pattern="\\$", replacement="USD", x=x)
    x <- gsub(pattern="[\\.\\,\\;\\:\\|]", replacement="_", x=x)
    x
}
