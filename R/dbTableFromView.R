#' dbTableFromView create a table in a SQLite database from  a
#'    view already present in the same database.
#'
#' @param view_name character, name of the view.
#' @param dbcon database connection, as created by the dbConnect function.
#' @param table_name character, the name of the table.
#'
#' @param drop_table logical, if `TRUE` the target table will be dropped (if exists)
#'    and recreated when importing the data.  if `FALSE`, data from input
#'    file will be appended to an existing table. Defaults to `FALSE`.
#' @param build_pk logical, if `TRUE` creates a `UNIQUE INDEX` named
#'    `<table_name>_PK` defined by the combination of fields specified
#'    in the `pk_fields` parameter. It will be effective only if 
#'    `pk_fields` is not null. Defaults to `FALSE`.
#' @param pk_fields character vector, the list of the fields' names that
#'    define the `UNIQUE INDEX`. Defaults to `NULL`.
#'
#' @returns nothing
#'
#' @import RSQLite
#' @export
dbTableFromView <- function(view_name, dbcon, table_name, 
                            drop_table=FALSE,
                            build_pk = FALSE, pk_fields = NULL) {

    if (drop_table) {
        sql.def <- paste("DROP TABLE IF EXISTS ", table_name, ";", sep = "")
        dbExecute(dbcon, sql.def)
    }
    
    sql.def <- paste("CREATE TABLE ", table_name,
        " AS ",
        "SELECT * FROM ", view_name,
        ";",
        sep = ""
    )
    dbExecute(dbcon, sql.def)

    cnames1 <- dbListFields(dbcon, table_name)

    ## Indexing -------------------------------
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
}
