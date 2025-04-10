% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/file_helpers.R
\name{Xlsx_file_schema}
\alias{Xlsx_file_schema}
\title{Xlsx_file_schema returns a data frame with the schema of an Excel data table}
\usage{
Xlsx_file_schema(
  input_file,
  sheet_name,
  first_row,
  cols_range,
  header = TRUE,
  id_quote_method = "DB_NAMES",
  max_lines = 100,
  ...
)
}
\arguments{
\item{input_file}{character, file name (including path) to be read.}

\item{sheet_name}{character, the name of the worksheet containing the data
table.}

\item{first_row}{integer, the row number where the data table starts.
If present, it is the row number of the header row, otherwise it is
the row number of the first row of data.}

\item{cols_range}{integer, a numeric vector specifying which columns in
the worksheet to be read.}

\item{header}{logical, if \code{TRUE} the first row contains the fields'
names. If \code{FALSE}, the column names will be the column names of the
Excel worksheet (i.e. letters).}

\item{id_quote_method}{character, used to specify how to build the SQLite
columns' names using the fields' identifiers read from the input file.
For details see the description of the \code{quote_method} parameter of
the \code{\link[=format_field_names]{format_field_names()}} function. Defautls to \code{DB_NAMES}.}

\item{max_lines}{integer, number of lines (excluding the header) to be
read to infer columns' data types. Defaults to 100.}

\item{...}{parameters passed to \code{\link[openxlsx2:wb_to_df]{openxlsx2::wb_to_df()}} function.}
}
\value{
a data frame with these columns:
\itemize{
\item \code{col_names}: columns' names, after applying the selected quote method;
\item \code{col_types}: columns' R data types;
\item \code{sql_types}: columns' SQLite data types;
\item \code{src_names}: columns' names as they appear in the input file;
\item \code{src_types}: data type attribute of each column, as determined by the
\code{\link[openxlsx2:wb_to_df]{openxlsx2::wb_to_df()}} function.
}
}
\description{
Preview the table structure contained in a rectangular range of worksheet
of an Excel file.
}
