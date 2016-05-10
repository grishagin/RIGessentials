\name{amatch_vect}
\alias{amatch_vect}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Vectorized Approximate String Matching
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Function uses approximate matching function \code{amatch} from package \code{stringdist} to compare one character vector to another. \cr
Before the matching is performed, the strings are cleaned-up:\cr
-- patterns from the default or custom dictionary are replaced with values from replacement vector;\cr
-- words are reordered alphabetically;\cr
-- extra spaces are removed.
}
\details{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
The output is a dataframe with 6 columns:\cr
-- 2 columns of inidices of the matching elements of the two vectors;\cr
-- 2 columns of the matching elements of the two vectors;\cr
-- 2 columns of the matching cleaned-up string elements of the two vectors;\cr

}
\usage{
amatch_vect(what_vect, to_what_vect, maxDist=0.4,
            dictionary_list=NULL, replacement_vect=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{what_vect}{
  vector to compare, element-by-element.
  }
  \item{to_what_vect}{
  vector to compare to.
  }
  \item{maxDist}{
  
  }
  \item{dictionary_list}{
  }
  \item{replacement_vect}{
  }
}