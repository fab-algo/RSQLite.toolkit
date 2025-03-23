#' The dbTableFromView function reads the data from a rectangula region
#' of a sheet in an Excel file and copies it to a table in a SQLite
#' database. If table does not exist, it will create it.
#'
#' @param view_name ...
#' @param dbcon ...
#' @param table_name ...
#' @param pk_fields ...
#' @param build_pk ...
#' @returns nothing
#'
#' @import RSQLite
#' @export
dbTableFromView <- function(view_name, dbcon, table_name = NULL, 
                            pk_fields = NULL, build_pk = FALSE) {
    if (is.null(table_name)) {
        table.name <- paste(gsub("VW_", "", view_name), "_OUT", sep = "")
    }

    sql.def <- paste("DROP TABLE IF EXISTS ", table_name, ";", sep = "")
    dbExecute(dbcon, sql.def)

    sql.def <- paste("CREATE TABLE ", table_name,
        " AS ",
        "SELECT * FROM ", view_name,
        ";",
        sep = ""
    )

    dbExecute(dbcon, sql.def)

    if (build_pk) {
        sql.def <- paste("CREATE UNIQUE INDEX ",
            paste(table_name, "_PK", sep = ""),
            "ON ", table_name, " (", paste(pk_fields, collapse = ", "), ");",
            sep = " "
        )
        dbExecute(dbcon, sql.def)
    }
}
