% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_excel_astext.R
\name{read_excel_astext}
\alias{read_excel_astext}
\title{Read Excel Files as Text}
\usage{
read_excel_astext(path, sheet = 1, col_names = TRUE, na = "",
  skip = 0)
}
\arguments{
\item{path}{Path to the xls/xlsx file.}

\item{sheet}{Sheet to read. Either a string (the name of a sheet), or an integer (the position of the sheet). Defaults to the first sheet.}

\item{col_names}{Either TRUE to use the first row as column names, 
FALSE to number columns sequentially from X1 to Xn, or a character vector giving a name for each column.}

\item{na}{Missing value. 
By default, readxl converts blank cells to missing data. 
Set this value if you have used a sentinel value for missing values.}

\item{skip}{Number of rows to skip before reading any data.}
}
\description{
A wrapper for \code{read_excel} function from \code{readxl} package. 
Reads data from an Excel file (*.xls or *.xlsx) into a dataframe. 
Key difference -- reads all columns as text, i.e. does not need the type to be specified for every column.
}
\author{
Ivan Grishagin
}
