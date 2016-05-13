\name{getAllFunctionsFromPackage}
\alias{getAllFunctionsFromPackage}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Extract All Functions from Package
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Function extracts all functions from a given package into one or multiple files.
}
\usage{
getAllFunctionsFromPackage(package=NULL,path=getwd(),one_file=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{package}{
character string. Name of the package to be subjected to dismantling.
  }
   \item{path}{
character string. Path to directory where to save the file(s) with functions.
  }
   \item{path}{
logical. One file (TRUE) or many (FALSE)?
  }
}