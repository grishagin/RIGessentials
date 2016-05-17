\name{file_name_from_path}
\alias{file_name_from_path}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Extract File Name from Path
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Takes in a vector of paths to files, returns the file names. Uses base function \code{strsplit}.
}
\usage{
file_name_from_path(path_vect
                   ,split="/")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{path_vect}{
character vector. Paths to files.
  }
  \item{path_vect}{
regular expression. Directory separator (usually "/" or "\\\\").
  }
}