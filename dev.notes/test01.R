## TESTING RSQLite.toolkit

library(RSQLite.toolkit)

file_path <- "../RSQLite.toolkit-tests/assets/2530009"
file_name <- "D1.4_Dataset_RESPOND_770564.xlsx"

dd <- file_schema_xlsx(input_file = file.path(file_path, file_name),
                       sheet_name = "Foglio1",
                       first_row = 1, cols_range = c("A:EV"), header = TRUE,
                       id_quote_method = "DB_NAMES",
                       max_lines = 2000,
                       null_columns = FALSE,
                       na.strings = ".")


d2 <- openxlsx2::wb_to_df(file = file.path(file_path, file_name),
  sheet = "Foglio1",
  start_row = 1, cols = c("A:EV"), col_names = TRUE,
  na.strings = ".", keep_attr = TRUE
)


## -------------------------------
library(RSQLite.toolkit)

dbcon <- dbConnect(RSQLite::SQLite(), file.path(tempdir(), "tests.sqlite"))

data_path <- system.file("extdata", package = "RSQLite.toolkit")

## creates the table ABALONE from a CSV file, adding the field 'SEQ' with the
## ROWID of each record through the use of the parameter 'auto_pk=TRUE'
dbTableFromDSV(input_file = file.path(data_path, "abalone.csv"),
               dbcon = dbcon, table_name = "ABALONE",
               drop_table = TRUE, auto_pk = TRUE,
               header = TRUE, sep = ",", dec = ".")

sql_text <- paste("SELECT SEX, RINGS, COUNT(*) AS NUM, AVG(LENGTH) AS AVG_LENGTH ", 
                  "FROM ABALONE GROUP BY SEX, RINGS;",
                  "SELECT SEX, round(WHOLE/0.02,0)*0.02 as WHOLE_GRP, COUNT(*) AS NUM, AVG(LENGTH) AS AVG_LENGTH ", 
                  "FROM ABALONE GROUP BY SEX, round(WHOLE/0.02,0)*0.02;",
                  sep = "\n")

sql_file <- tempfile(fileext = ".sql")
writeLines(sql_text, con = sql_file)

res <- dbExecFile(input_file = sql_file, dbcon = dbcon)

res[[1]][1:10, ]
res[[2]][1:10, ]

unlink(sql_file)
dbDisconnect(dbcon)