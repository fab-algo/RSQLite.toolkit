% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dbTableFromFeather.R
\name{dbTableFromFeather}
\alias{dbTableFromFeather}
\title{dbTableFromFeather create a table in SQLite database from a
Feather file}
\usage{
dbTableFromFeather(
  input_file,
  dbcon,
  table_name,
  columns = NULL,
  drop_table = FALSE,
  auto_pk = FALSE,
  pk_fields = NULL,
  build_pk = FALSE,
  constant_values = NULL
)
}
\arguments{
\item{input_file}{the file name (including path) to be read.}

\item{dbcon}{database connection, as created by the dbConnect function.}

\item{table_name}{string, the name of the table.}

\item{columns}{character vector, names of the columuns to be imported.
If \code{NULL} all columns will be imported in the db table. Defaults to \code{NULL}.}

\item{drop_table}{logical, if \code{TRUE} the target table will be dropped (if exists)
and recreated before importing the data. Defaults to \code{FALSE}.}

\item{auto_pk}{...}

\item{pk_fields}{...}

\item{build_pk}{...}

\item{constant_values}{...}
}
\value{
nothing
}
\description{
The dbTableFromFeather function reads the data from a Apache
Arrow table serialized in a Feather (Arrow IPC) file and copies it
to a table in a SQLite database. If table does not exist, it will
create it.
}
