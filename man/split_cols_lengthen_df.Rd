% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/split_cols_lengthen_df.R
\name{split_cols_lengthen_df}
\alias{split_cols_lengthen_df}
\title{Split Column(s) Based on Pattern}
\usage{
split_cols_lengthen_df(dFrame, colsToSplit, patternToSplit = "\\\\|",
  at_once = TRUE)
}
\arguments{
\item{dFrame}{Dataframe to process.}

\item{colsToSplit}{Columns to process.}

\item{patternToSplit}{Pattern to split the columns on.}

\item{at_once}{(Boolean) Split all columns at once or one-by-one?}
}
\description{
Wrapper for a \code{strsplit} function. 
In a given dataframe, split specified column values based on certain pattern, 
which results in additional rows. All elements in other columns are duplicated.
With multiple columns, it can work in two ways: 
either one after the other with \code{at_once=FALSE} 
or all at the same time with \code{at_once=TRUE} (default). 
When all columns are processed at once, the function attempts to find 
the same number of elements (that are produced after splitting) 
in all columns for every row that contains the pattern.
If some columns have just one element, it is replicated to match the maximum number of elements.
If the number of elements is otherwise not equal between the columns, an error is thrown.
}
