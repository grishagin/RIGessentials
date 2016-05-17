\name{merge_fun_files}
\alias{merge_fun_files}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Merge Files into One File
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Interactive function that takes in a directory or a list of files (using a directory or file picker), and outputs one file with combined contents.
}
\usage{
merge_fun_files(source_dir=NULL
                ,pattern="\\\\.R"
                ,result_filename="merged.R")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{source_dir}{
character string. Path to a directory with files (optional).
}

  \item{pattern}{
regular expression. What kind of files to look for? 
}
  \item{result_filename}{
character string. Output file name. 
}
}