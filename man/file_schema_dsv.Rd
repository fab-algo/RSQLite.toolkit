% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/file_helpers.R
\name{file_schema_dsv}
\alias{file_schema_dsv}
\title{file_schema_dsv returns a data frame with the schema of a DSV file}
\usage{
file_schema_dsv(
  input_file,
  header = TRUE,
  sep = ",",
  dec = ".",
  grp = "",
  id_quote_method = "DB_NAMES",
  max_lines = 2000,
  null_columns = FALSE,
  force_num_cols = TRUE,
  ...
)
}
\arguments{
\item{input_file}{character, file name (including path) to be read.}

\item{header}{logical, if \code{TRUE} the first line contains the fields'
names. If \code{FALSE}, the column names will be formed sing a "V"
followed by the column number (as specified in \code{\link[utils:read.table]{utils::read.table()}}).}

\item{sep}{character, field delimiter (e.g., "," for CSV, "\\t" for TSV)
in the input file. Defaults to ",".}

\item{dec}{character, decimal separator (e.g., "." or "," depending on locale)
in the input file. Defaults to ".".}

\item{grp}{character, character used for digit grouping. It defaults
to \code{""} (i.e. no grouping).}

\item{id_quote_method}{character, used to specify how to build the SQLite
columns' names using the fields' identifiers read from the input file.
For details see the description of the \code{quote_method} parameter of
the \code{\link[=format_column_names]{format_column_names()}} function. Defautls to \code{DB_NAMES}.}

\item{max_lines}{integer, number of lines (excluding the header) to be
read to infer columns' data types. Defaults to 2000.}

\item{null_columns}{logical, if \code{TRUE} the col_type of columuns consisting
only of NAs or zero-length strings will be marked as \code{"NULL"}, otherwise
they will be marked as \code{character}. Defaults to \code{FALSE}}

\item{force_num_cols}{logical, if \code{TRUE} the returned schema will have
\code{n_cols} rows (i.e. the guessed number of columns determined inspecting
the first \code{max_lines} lines of the input file), even if there are rows
in the input file with fewer or greater columns than \code{n_cols}.
If \code{FALSE} and any of the tested lines has a number of columns not
equal to \code{n_cols}, the function will return a list without the \code{schema}
element. It defaults to \code{TRUE}.}

\item{...}{Additional arguments passed to \code{\link[utils:read.table]{utils::read.table()}}.}
}
\value{
a list with the following named elements:
\itemize{
\item \code{schema}, a data frame with these columns:
\itemize{
\item \code{col_names}: columns' names, after applying the selected quote method;
\item \code{col_types}: columns' R data types;
\item \code{sql_types}: columns' SQLite data types;
\item \code{src_names}: columns' names as they appear in the input file.
\item \code{src_types}: dafults to \code{text} for all columns.
}
\item \code{col_counts}, a data frame with these columns:
\itemize{
\item \code{Num_col}: number of columns,
\item \code{Freq}: number of rows (within \code{max_lines}) that have the number
of colums shown in \code{Num_col}.
}
\item \code{n_cols}, integer, the number of columns selected for the file.
\item \code{Num_col}, a vector of integers of length \code{max_lines} with the
number of detected columns in each row tested.
\item \code{col_fill}, logical, it is set to \code{TRUE} if there are lines with
less columns than \code{n_cols}.
\item \code{col_flush}, logical, it is set to \code{TRUE} if there are lines with
more columns than \code{n_cols}.
}
}
\description{
Reads only the first \code{max_lines} of a delimiter separated values (DSV)
text file to infer column names and data types, without reading the full
dataset into memory. Then it converts them to the candidate data
frame columns' names and data types.
}
\examples{
\dontrun{
file_schema_dsv("data.csv", sep=",", dec=".", max_lines=50)
file_schema_dsv("euro.csv", sep=";", dec=",", header=FALSE)
}
}
