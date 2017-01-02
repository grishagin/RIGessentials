\name{merge_cols_shorten_df}
\alias{merge_cols_shorten_df}
\title{
Merge Columns in Dataframe Using Pattern
}
\description{
Looks up duplicate values in the \code{colKey} column, and then merges all corresponding
values in the \code{colToMerge} column, separating them with a \code{patternToMerge} separator.
}
\usage{
merge_cols_shorten_df(dFrame = NULL
                        ,colKey = NULL
                        ,colsToMerge = NULL
                        ,patternToMerge = "|")
}
\arguments{
  \item{dFrame}{
}
  \item{colKey}{
  key column. Duplicate values are looked up in this column.
}
  \item{colToMerge}{
  columns to merge and return. If \code{NULL}, merges all but the \code{colKey} column. In \code{colKey} column, duplicate values will be removed, and then this column will be bound to the merged columns.
}
  \item{patternToMerge}{
  pattern to use for merging of the values. Defaults to pipe (\code{|}).
  }
}
\author{
Ivan Grishagin
}