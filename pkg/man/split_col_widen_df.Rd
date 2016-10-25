\name{expand.df.via.split.col}
\alias{expand.df.via.split.col}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Split Column Based on Pattern, Expand DF Widthwise
}
\description{
Wrapper for a \code{strsplit} function. In a given dataframe, splits one column values based on certain pattern, then adds one extra column for each member of a split vector of maximum length to the dataframe. If after splitting the number of resultant elements in each row is not the same, fills missing values with NA.

}
\usage{
split_col_widen_df(DF
                 ,colToSplit
                 ,split="\\\\|"
                 ,newcolnames=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{DF}{
%%     ~~Describe \code{dFrame} here~~
}
  \item{colToSplit}{
%%     ~~Describe \code{colToSplit} here~~
}
  \item{split}{
%%     ~~Describe \code{patternToSplit} here~~
}
  \item{newcolnames}{
%%     ~~Describe \code{patternToSplit} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Ivan Grishagin
}