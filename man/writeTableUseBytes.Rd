% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/writeTableUseBytes.R
\name{writeTableUseBytes}
\alias{writeTableUseBytes}
\title{Write Dataframe to File As-Is}
\usage{
writeTableUseBytes(dFrame, fileName, sep = "\\t", eol = "\\n",
  useBytes = TRUE, quote = "\\"")
}
\arguments{
\item{dFrame}{Dataframe to modify.}

\item{fileName}{Output filename.}

\item{sep}{Separator to use in the output file.}

\item{eol}{End of line symbol.}

\item{useBytes}{Write as-is? Defaults to \code{TRUE}.
If \code{TRUE}, suppresses the re-encoding  
of marked strings so they are passed byte-by-byte to the connection.}

\item{quote}{(String) Which marks to use around each entry?
Defaults to \code{"\""}.
If \code{quote=""}, the text will be written "as is".}
}
\description{
Allows to use \code{useBytes} parameter 
of the \code{writeLines} function. \cr
Useful to avoid encoding conversion issues.
}
\author{
Ivan Grishagin
}
