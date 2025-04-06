#' dbTableFromView create a table in a SQLite database from  a
#'    view already present in the same database.
#'
#' @param view_name ...
#' @param dbcon database connection, as created by the dbConnect function.
#' @param table_name string, the name of the table.
#'
#' @param drop_table ...
#' @param pk_fields ...
#' @param build_pk ...
#' 
#' @returns nothing
#'
#' @import RSQLite
#' @export
dbTableFromView <- function(view_name, dbcon, table_name = NULL, 
                            drop_table=FALSE,
                            pk_fields = NULL, build_pk = FALSE) {

    if (is.null(table_name)) {
        table.name <- paste(gsub("VW_", "", view_name), "_TBL", sep = "")
    }

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

    rs <- dbSendQuery(dbcon, paste0("SELECT * FROM ", table_name))
    df <- dbColumnInfo(rs)
    dbClearResult(rs)

    cnames1 <- df$name

    ## Indexing -------------------------------
    if (drop_table && !is.null(pk_fields) && build_pk) {
        
        if (!is.character(pk_fields)) {
            stop("dbTableFromView: 'pk.fields' must be a character vector.")
        }

        check_fields <- setdiff(pk_fields, cnames1)
        if (length(check_fields) > 0) {
            stop("dbTableFromView: 'pk.fields' contains unknown field names: ",
                 check_fields)
        }

        sql.def <- paste("CREATE UNIQUE INDEX ",
                         paste(table_name, "_PK", sep = ""),
                         "ON ", table_name, " (",
                         paste(pk_fields, collapse = ", "), ");",
                         sep = " "
                         )
        
        dbExecute(dbcon, sql.def)
    }
}
