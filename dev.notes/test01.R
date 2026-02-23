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

## Load data from a DSV file into a database table ------------
data_path <- system.file("extdata", package = "RSQLite.toolkit")
dbTableFromDSV(input_file = file.path(data_path, "abalone.csv"),
               dbcon = dbcon, table_name = "ABALONE",
               drop_table = TRUE, auto_pk = TRUE,
               header = TRUE, sep = ",", dec = ".")
## ------------------------------------------------------------

## SQL file content: each statement is separated by a semicolon
sql_text <- paste("DROP TABLE IF EXISTS AVG_LEN_BY_SEX_RINGS;",
                  " ",
                  "CREATE TABLE AVG_LEN_BY_SEX_RINGS ",
                  "(SEX TEXT, RINGS INTEGER, NUM INTEGER, AVG_LENGTH REAL);",
                  " ",
                  "INSERT INTO AVG_LEN_BY_SEX_RINGS ",
                  "SELECT SEX, RINGS, COUNT(*) AS NUM, AVG(LENGTH) ", 
                  "AS AVG_LENGTH FROM ABALONE GROUP BY SEX, RINGS;",
                  " ",
                  "SELECT * FROM AVG_LEN_BY_SEX_RINGS;",
                  " ",
                  "SELECT SEX, round(WHOLE/:bin_size,0)*:bin_size as WHOLE_GRP, ",
                  "COUNT(*) AS NUM, AVG(LENGTH) AS AVG_LENGTH ", 
                  "FROM ABALONE GROUP BY SEX, round(WHOLE/:bin_size,0)*:bin_size;",
                  sep = "\n")

sql_file <- tempfile(fileext = ".sql")
writeLines(sql_text, con = sql_file)
## ------------------------------------------------------------

## Execute SQL statements from the file -----------------------
plist=list(q1=NULL,
           q2=NULL,
           q3=NULL,
           q4=NULL,
           q5=list(bin_size = 0.025))
res <- dbExecFile(input_file = sql_file, dbcon = dbcon, 
                  plist = plist)
## ------------------------------------------------------------

## Check results ----------------------------------------------
dbListTables(dbcon)
dbListFields(dbcon, "AVG_LEN_BY_SEX_RINGS")

res[[1]]
res[[2]]
res[[3]]

res[[4]][1:10, ]

res[[5]][1:10, ]
## ------------------------------------------------------------

## Cleanup ----------------------------------------------------
unlink(sql_file)
dbDisconnect(dbcon)
