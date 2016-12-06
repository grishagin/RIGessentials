\name{split_fun_file_into_many}
\alias{split_fun_file_into_many}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Split One File with Functions into Individual Files
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Function takes in a file with multiple functions and records them into separate files.
}
\usage{
split_fun_file_into_many(fun_file_path
                         ,write_to_files=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fun_file_path}{
character string. Path to a file with R functions.
  }
  \item{write_to_files}{
logical. Write to files? If \code{TRUE} (default), will write to files, if not -- will return list of functions, where each function is represented as a vector of strings.
  }
}