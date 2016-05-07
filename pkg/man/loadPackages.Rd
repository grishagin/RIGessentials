\name{loadPackages}
\alias{loadPackages}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Install and Load Packages
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Function checks which packages are installed, and installs the missing ones from CRAN and BioConductor.
After that, all packages are loaded.
}
\usage{
loadPackages(packages = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{packages}{
character vector of packages to be loaded/installed.
  }
}