## TESTING RSQLite.toolkit

## ------------------------------------------------------------
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
## ------------------------------------------------------------



## ------------------------------------------------------------
## ------------------------------------------------------------
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
## ------------------------------------------------------------




## ------------------------------------------------------------
## dbCopyTable() examples -------------------------------------

# Copy a table from one database to another
db_source <- tempfile(fileext = ".sqlite")
db_target <- tempfile(fileext = ".sqlite")

# Load some sample data
dbcon <- dbConnect(RSQLite::SQLite(), db_source)

data_path <- system.file("extdata", package = "RSQLite.toolkit")
dbTableFromDSV(
  input_file = file.path(data_path, "abalone.csv"),
  dbcon = dbcon,
  table_name = "ABALONE",
  drop_table = TRUE,
  auto_pk = TRUE,
  header = TRUE,
  sep = ",",
  dec = "."
)

dbDisconnect(dbcon)

# Copy the table to a new database, recreating it 
# if it already exists and copying indexes
dbCopyTable(
  db_file_src = db_source, 
  db_file_tgt = db_target,
  table_name = "ABALONE",
  drop_table = TRUE,        # Recreate table if it exists
  copy_indexes = TRUE       # Copy indexes too
)

# Check that the table was copied correctly
dbcon_tgt <- dbConnect(RSQLite::SQLite(), db_target)
print(dbListTables(dbcon_tgt))
print(dbListFields(dbcon_tgt, "ABALONE"))
print(dbGetQuery(dbcon_tgt, "SELECT COUNT(*) AS TOTAL_ROWS FROM ABALONE;"))
dbDisconnect(dbcon_tgt)

# Clean up temporary database files
unlink(c(db_source, db_target))
## ------------------------------------------------------------



## ------------------------------------------------------------
## dbExecFile() examples --------------------------------------

# Create a database and execute SQL from a file
library(RSQLite.toolkit)

# Set up database connection
dbcon <- dbConnect(RSQLite::SQLite(), file.path(tempdir(), "example.sqlite"))

# Load some sample data
data_path <- system.file("extdata", package = "RSQLite.toolkit")
dbTableFromDSV(
  input_file = file.path(data_path, "abalone.csv"),
  dbcon = dbcon,
  table_name = "ABALONE",
  drop_table = TRUE,
  auto_pk = TRUE,
  header = TRUE,
  sep = ",",
  dec = "."
)

# Create a SQL file with multiple statements
sql_content <- "
-- Create a summary table
DROP TABLE IF EXISTS ABALONE_SUMMARY;

CREATE TABLE ABALONE_SUMMARY AS 
SELECT SEX, 
       COUNT(*) as TOTAL_COUNT,
       ROUND(AVG(LENGTH), 3) as AVG_LENGTH,
       ROUND(AVG(WHOLE), 3) as AVG_WEIGHT
FROM ABALONE 
GROUP BY SEX;

-- Query the results  
SELECT * FROM ABALONE_SUMMARY ORDER BY SEX;

-- Parameterized query example
SELECT SEX, COUNT(*) as COUNT 
FROM ABALONE 
WHERE LENGTH > :min_length
GROUP BY SEX;
"

sql_file <- tempfile(fileext = ".sql")
writeLines(sql_content, sql_file)

# Execute SQL statements with parameters
plist <- list(
  NULL,  # DROP TABLE statement (no parameters)
  NULL,  # CREATE TABLE statement (no parameters) 
  NULL,  # First SELECT (no parameters)
  list(min_length = 0.5)  # Parameterized SELECT
)

results <- dbExecFile(
  input_file = sql_file,
  dbcon = dbcon,
  plist = plist
)

# Check results
print(results[[3]])  # Summary data
print(results[[4]])  # Filtered data

# Clean up
unlink(sql_file)
dbDisconnect(dbcon)
## ------------------------------------------------------------


## ------------------------------------------------------------
# Create a database and table, then add a primary key
library(RSQLite.toolkit)

# Set up database connection
dbcon <- dbConnect(RSQLite::SQLite(), file.path(tempdir(), "example.sqlite"))

# Load sample data
data_path <- system.file("extdata", package = "RSQLite.toolkit")

dbTableFromFeather(
  input_file = file.path(data_path, "penguins.feather"),
  dbcon = dbcon, table_name = "PENGUINS",
  drop_table = TRUE
)

dbGetQuery(dbcon, "select species, sex, body_mass_g,
                   culmen_length_mm, culmen_depth_mm 
                   from PENGUINS 
                   group by species, sex, body_mass_g,
                   culmen_length_mm, culmen_depth_mm 
                   having count(*) > 1")

# Create a primary key on multiple fields
dbCreatePK(dbcon, "PENGUINS", 
           c("species", "sex", "body_mass_g",
             "culmen_length_mm", "culmen_depth_mm"))

# Check that the index was created
indexes <- dbGetQuery(dbcon, 
  "SELECT name, sql FROM sqlite_master WHERE type='index' AND tbl_name='PENGUINS'")
print(indexes)

# Clean up
dbDisconnect(dbcon)
## ------------------------------------------------------------


## ------------------------------------------------------------
## dbTableFromDataFrame() examples ----------------------------

# Create a temporary database and load data frame
library(RSQLite.toolkit)

# Set up database connection
dbcon <- dbConnect(RSQLite::SQLite(), file.path(tempdir(), "example.sqlite"))

# Create a sample data frame
sample_data <- data.frame(
  id = 1:10,
  name = paste0("Item_", 1:10),
  value = runif(10, 1, 100),
  active = c(TRUE, FALSE),
  date = Sys.Date() + 0:9,
  stringsAsFactors = FALSE,
  row.names = NULL
)

# Load data frame with automatic primary key
dbTableFromDataFrame(
  df = sample_data,
  dbcon = dbcon,
  table_name = "SAMPLE_DATA",
  drop_table = TRUE,
  auto_pk = TRUE  
)

# Check the imported data
dbListFields(dbcon, "SAMPLE_DATA")
dbGetQuery(dbcon, "SELECT * FROM SAMPLE_DATA LIMIT 5")

# Load with column selection and custom naming
dbTableFromDataFrame(
  df = sample_data,
  dbcon = dbcon,
  table_name = "SAMPLE_SUBSET",
  drop_table = TRUE,
  col_names = c("ID", "ITEM_NAME", "ITEM_VALUE", "IS_ACTIVE", "DATE_CREATED")
)

dbGetQuery(dbcon, "SELECT * FROM SAMPLE_SUBSET LIMIT 5")

# Clean up
dbDisconnect(dbcon)
## ------------------------------------------------------------



## ------------------------------------------------------------
## dbTableFromDSV() examples ----------------------------------
library(RSQLite.toolkit)

# Set up database connection
dbcon <- dbConnect(RSQLite::SQLite(), file.path(tempdir(), "example.sqlite"))

# Get path to example data
data_path <- system.file("extdata", package = "RSQLite.toolkit")

# Load abalone CSV data with automatic primary key
dbTableFromDSV(
  input_file = file.path(data_path, "abalone.csv"),
  dbcon = dbcon,
  table_name = "ABALONE",
  drop_table = TRUE,
  auto_pk = TRUE,
  header = TRUE,
  sep = ",",
  dec = "."
)

# Check the imported data
dbListFields(dbcon, "ABALONE")
head(dbGetQuery(dbcon, "SELECT * FROM ABALONE"))

# Load data with specific column selection
dbTableFromDSV(
  input_file = file.path(data_path, "abalone.csv"),
  dbcon = dbcon,
  table_name = "ABALONE_SUBSET",
  drop_table = TRUE,
  header = TRUE,
  sep = ",",
  dec = ".",
  col_import = c("Sex", "Length", "Diam", "Whole")
)

head(dbGetQuery(dbcon, "SELECT * FROM ABALONE_SUBSET"))

# Check available tables
dbListTables(dbcon)

# Clean up
dbDisconnect(dbcon)
## ------------------------------------------------------------






## ------------------------------------------------------------
## dbTableFromFeather() examples -------------------------------
# Create a temporary database and load Feather data
library(RSQLite.toolkit)

# Set up database connection
dbcon <- dbConnect(RSQLite::SQLite(), file.path(tempdir(), "example.sqlite"))

# Get path to example data
data_path <- system.file("extdata", package = "RSQLite.toolkit")

# Load penguins Feather data
dbTableFromFeather(
  input_file = file.path(data_path, "penguins.feather"),
  dbcon = dbcon,
  table_name = "PENGUINS",
  drop_table = TRUE
)

# Check the imported data
dbListFields(dbcon, "PENGUINS")
head(dbGetQuery(dbcon, "SELECT * FROM PENGUINS"))

# Load with custom column selection and types
dbTableFromFeather(
  input_file = file.path(data_path, "penguins.feather"),
  dbcon = dbcon,
  table_name = "PENGUINS_SUBSET",
  drop_table = TRUE,
  col_import = c("species", "flipper_length_mm", "body_mass_g", "sex")
)

# Check the imported data
dbListFields(dbcon, "PENGUINS_SUBSET")
head(dbGetQuery(dbcon, "SELECT * FROM PENGUINS_SUBSET"))

# Check available tables
dbListTables(dbcon)

# Clean up
dbDisconnect(dbcon)
## ------------------------------------------------------------



## ------------------------------------------------------------
# Create a temporary database and demonstrate view to table conversion
library(RSQLite.toolkit)

# Set up database connection
dbcon <- dbConnect(RSQLite::SQLite(), file.path(tempdir(), "example.sqlite"))

# Load some sample data first
data_path <- system.file("extdata", package = "RSQLite.toolkit")
dbTableFromDSV(
  input_file = file.path(data_path, "abalone.csv"),
  dbcon = dbcon,
  table_name = "ABALONE",
  drop_table = TRUE,
  header = TRUE,
  sep = ",",
  dec = "."
)

# Create a view with aggregated data
dbExecute(dbcon, "DROP VIEW IF EXISTS VW_ABALONE_SUMMARY;")

dbExecute(dbcon, 
  "CREATE VIEW VW_ABALONE_SUMMARY AS 
   SELECT SEX, 
          COUNT(*) as COUNT, 
          AVG(LENGTH) as AVG_LENGTH,
          AVG(WHOLE) as AVG_WEIGHT
   FROM ABALONE 
   GROUP BY SEX"
)

# Convert the view to a permanent table
dbTableFromView(
  view_name = "VW_ABALONE_SUMMARY",
  dbcon = dbcon,
  table_name = "ABALONE_STATS",
  drop_table = TRUE
)

# Check the result
dbListTables(dbcon)
dbGetQuery(dbcon, "SELECT * FROM ABALONE_STATS")

# Clean up
dbDisconnect(dbcon)
## ------------------------------------------------------------


## ------------------------------------------------------------
## dbTableFromXlsx() examples ---------------------------------
# Create a temporary database and load Excel data
library(RSQLite.toolkit)

# Set up database connection
dbcon <- dbConnect(RSQLite::SQLite(), file.path(tempdir(), "example.sqlite"))

# Get path to example data
data_path <- system.file("extdata", package = "RSQLite.toolkit")

# Check if Excel file exists (may not be available in all installations)
xlsx_file <- file.path(data_path, "stock_portfolio.xlsx")

fschema <- file_schema_xlsx(xlsx_file, sheet_name="all period", 
                            first_row=2, cols_range="A:S", header=TRUE,
                            id_quote_method="DB_NAMES", max_lines=10)

fschema[, c("col_names", "src_names")]

# Load Excel data from specific sheet and range
dbTableFromXlsx(
  input_file = xlsx_file,
  dbcon = dbcon,
  table_name = "PORTFOLIO_PERF",
  sheet_name = "all period",
  first_row = 2,
  cols_range = "A:S",
  drop_table = TRUE,
  col_import = c("ID", "Large_B_P", "Large_ROE", "Large_S_P",
                 "Annual_Return_7", "Excess_Return_8", "Systematic_Risk_9")
)


# Check the imported data
dbListFields(dbcon, "PORTFOLIO_PERF")
head(dbGetQuery(dbcon, "SELECT * FROM PORTFOLIO_PERF"))

# Clean up
dbDisconnect(dbcon)
## ------------------------------------------------------------


## ------------------------------------------------------------
# Inspect Feather file schema without loading data into memory
data_path <- system.file("extdata", package = "RSQLite.toolkit")

# Get schema information for penguins Feather file
schema_info <- file_schema_feather(
  input_file = file.path(data_path, "penguins.feather")
)

# Display schema information
print(schema_info[, c("col_names", "col_types", "sql_types", "src_names")])

# Check specific columns
print(paste("Number of columns:", nrow(schema_info)))
print(paste("Column names:", paste(schema_info$col_names, collapse = ", ")))
## ------------------------------------------------------------


## ------------------------------------------------------------
# Inspect CSV file schema without loading full dataset
data_path <- system.file("extdata", package = "RSQLite.toolkit")

# Get schema information for abalone CSV
schema_info <- file_schema_dsv(
  input_file = file.path(data_path, "abalone.csv"),
  header = TRUE,
  sep = ",",
  dec = ".",
  max_lines = 100
)

# Display schema information
print(schema_info$schema[, c("col_names", "col_types", "sql_types")])

# Check column consistency
print(schema_info$col_counts)
print(paste("Guessed columns:", schema_info$n_cols))

# Example with different parameters
schema_custom <- file_schema_dsv(
  input_file = file.path(data_path, "abalone.csv"),
  header = TRUE,
  sep = ",",
  dec = ".",
  max_lines = 50,
  id_quote_method = "SQL_SERVER"
)

schema_custom$schema
## ------------------------------------------------------------


## ------------------------------------------------------------
# Inspect xlsx file schema
data_path <- system.file("extdata", package = "RSQLite.toolkit")

# Get schema information for Excel file
schema_info <- file_schema_xlsx(
  input_file = file.path(data_path, "stock_portfolio.xlsx"),
  sheet_name = "all period",
  first_row = 2,
  cols_range = "A:S",
  header = TRUE,
  id_quote_method = "DB_NAMES",
  max_lines = 10
)

# Display schema information
head(schema_info[, c("col_names", "src_names")])

# Check specific columns
print(paste("Number of columns:", nrow(schema_info)))
## ------------------------------------------------------------


## ------------------------------------------------------------
# Convert R data types to SQLite types
r_types <- c("character", "integer", "numeric", "logical", "Date")
sql_types <- R2SQL_types(r_types)

# Display the mapping
data.frame(
  R_type = r_types,
  SQLite_type = sql_types,
  row.names = NULL
)

# Handle unknown types (converted to TEXT)
mixed_types <- c("character", "unknown_type", "integer")
R2SQL_types(mixed_types)
## ------------------------------------------------------------


## ------------------------------------------------------------
# Example with DB_NAMES method
col_names <- c("column 1", "column-2", "3rd_column", "SELECT")

formatted_names <- format_column_names(col_names, quote_method = "DB_NAMES")
print(formatted_names)

# Example with SINGLE_QUOTES method
formatted_names_sq <- format_column_names(col_names, quote_method = "SINGLE_QUOTES")
print(formatted_names_sq)

# Example with SQL_SERVER method
formatted_names_sqlsrv <- format_column_names(col_names, quote_method = "SQL_SERVER")
print(formatted_names_sqlsrv)
## ------------------------------------------------------------


## END OF TESTS