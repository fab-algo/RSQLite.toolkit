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
                         


