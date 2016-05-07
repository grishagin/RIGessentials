\name{writeTableUseBytes}
\alias{writeTableUseBytes}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Write Dataframe to Text File
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Write a dataframe to text file using writeLines with option UseBytes = TRUE
}
\usage{
writeTableUseBytes(dFrame, fileName, sep="\t", eol="\n", useBytes=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dFrame}{
the object to be written (a matrix, a data frame or a data table).
  }
  \item{fileName}{
either a character string naming a file or a connection open for writing.
}
  \item{sep}{
	the field separator string. 
	Values within each row of x are separated by this string.
  }
  \item{eol}{
	the end of line character. 
  }
  \item{useBytes}{
	logical.  
	If TRUE, suppresses the re-encoding of marked strings so they are passed byte-by-byte to the connection.
  }
}