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
amatch_vect(what_vect
            ,to_what_vect
            ,maxDist=0.4
            ,dictionary_list=NULL
            ,replacement_vect=NULL
            ,no_exact_match=FALSE)
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
  degree of difference, measured as a fraction of string length. Can take values from 0 to 1.
  }
  \item{dictionary_list}{
  list of values to replace before matching.
  }
  \item{replacement_vect}{
  vector of values to replace those values in the \code{dictionary_list}.
  }
  \item{no_exact_match}{
  logical. Discard all exact matches? May be useful if your list has exact matches (which you know about), and you just want to focus on approximate matches.
  }
}