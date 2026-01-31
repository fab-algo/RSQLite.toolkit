require(RSQLite.toolkit)

db_path <- "../notes"
dbcon <- dbConnect(RSQLite::SQLite(), file.path(db_path, "tests.sqlite"))




input_path <- "../RSQLite.toolkit-tests/assets/DOSE_V2.10"
input_file <- file.path(input_path,
                        "DOSE_V2.10.csv")
sep <- ","
dec <- "."
grp <- ""
quote1 <- "\""
fileEncoding <- "UTF-8"

dbTableFromDSV(input_file = input_file, dbcon = dbcon, table_name = "DOSE",
               sep = sep, dec = dec, grp = grp,
               header = TRUE, drop_table = TRUE,
               fileEncoding = fileEncoding, quote = quote1,
               na.strings = "", comment.char = "")





input_path <- "../RSQLite.toolkit-tests/assets/Blockchain_Banking_Scopus"
input_file <- file.path(input_path,
                        "Blockchain_Banking_Scopus_Dataset_2015_2025.csv")
sep <- ","
dec <- "."
grp <- ""
quote1 <- "\""
fileEncoding <- "UTF-8"

dbTableFromDSV(input_file = input_file, dbcon = dbcon,
               table_name = "BLOCKCHAIN_BANKING",
               sep = sep, dec = dec, grp = grp,  quote = quote1,
               header = TRUE, na.strings = "", comment.char = "",
               fileEncoding = fileEncoding,
               drop_table = TRUE)





input_path <- "../RSQLite.toolkit-tests/assets/IT_INCOME_TAX_BY_AGE"
input_file <- file.path(input_path, "cla_anno_bonus_irpef_2024.csv")
header <- TRUE
sep <- ";"
dec <- ","
grp <- "."
quote1 <- "\""
fileEncoding <- ""

dbTableFromDSV(input_file = input_file, dbcon = dbcon,
               table_name = "IT_INCOME_TAX_BY_AGE",
               sep = sep, dec = dec, grp = grp,  quote = quote1,
               header = TRUE, na.strings = "", comment.char = "",
               fileEncoding = fileEncoding,
               drop_table = TRUE)



input_path <- "../RSQLite.toolkit-tests/assets/3690826"
input_file <- file.path(input_path, "Collection-of-global-datasets-v2-0.xlsx")
header <- TRUE

dbTableFromXlsx(input_file = input_file, dbcon = dbcon,
                table_name = "DATASETS_FLOODS", sheet_name = "Datasets",
                first_row = 6, cols_range = "A:W", header = TRUE,
                drop_table = TRUE, na.strings = "na")



input_path <- "../RSQLite.toolkit-tests/assets/2530009"
input_file <- file.path(input_path, "D1.4_Dataset_RESPOND_770564.xlsx")
header <- TRUE

dd <- file_schema_xlsx(input_file = input_file, sheet_name = "Foglio1",
                       first_row = 1, cols_range = "A:EV",
                       header = TRUE, na.strings = ".")
target_types <- dd$col_types
target_types[1] <- "integer"

dbTableFromXlsx(input_file = input_file, dbcon = dbcon,
                table_name = "RESPOND", sheet_name = "Foglio1",
                first_row = 1, cols_range = "A:EV", header = TRUE,
                col_types = target_types,
                drop_table = TRUE, na.strings = ".")


dbDisconnect(dbcon)
