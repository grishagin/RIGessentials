\name{check_functions_in_main}
\alias{check_functions_in_main}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Remove Functions No Longer Needed
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Function takes in a "functions" file and a file with "main" script code.\cr
Then, it evaluates which functions from "functions" file are present in "main" script, and keeps only those ones (separate file).
}
\usage{
check_functions_in_main(fun_file_path,main_file_path)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fun_file_path}{
character string. Path to a file with R functions.
  }
   \item{main_file_path}{
character string. Path to a file with "main" R script.
  }
}