\name{prepareSession}
\alias{prepareSession}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Prepare New Session
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Function prepares a new session:\cr
1) sets working directory to the desired one;\cr
2) sets options(stringsAsFactors = FALSE);\cr
3) sets system locale to a "neutral" one that does not change/re-encode the input.
}
\usage{
prepareSession(work_dir = "C:/")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{work_dir}{
string specifying desired working directory.  
If directory does not exist, prompts the user to choose one using a tkwidget.
}
}