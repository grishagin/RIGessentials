\name{trim_fun_file_based_on_main}
\alias{trim_fun_file_based_on_main}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Remove Functions No Longer Needed
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Function takes in a "functions" file and a file with "main" script code.\cr
Then, it evaluates which functions from "functions" file are present in "main" script, and keeps only those ones in separate file(s).
}
\usage{
trim_fun_file_based_on_main(fun_file_path
                            ,main_file_path
                            ,one_file = TRUE
                            ,save_dir = getwd())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fun_file_path}{
character string. Path to a file with R functions.
  }
   \item{main_file_path}{
character string. Path to a file with "main" R script.
  }
  \item{one_file}{
logical. Save as one file (\code{TRUE}) or multiple (\code{FALSE})?
  }
  \item{save_dir}{
character string. Path to a directory where to save the trimmed file(s).
  }
}