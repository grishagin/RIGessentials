amatch_vect<-function(what_vect
                      ,to_what_vect
                      ,maxDist=0.4
                      ,dictionary_list=NULL
                      ,replacement_vect=NULL
                      ,no_exact_match=FALSE){
		#' @export
		#' @title
        #' Vectorized Approximate String Matching
        #' @description 
        #' Uses approximate matching function \code{amatch} from package \code{stringdist} 
		#' to compare one character vector to another. \cr
		#' Before the matching is performed, the strings are cleaned-up:\cr
		#' -- patterns from the default or custom dictionary are replaced with values from replacement vector;\cr
		#' -- words are reordered alphabetically;\cr
		#' -- extra spaces are removed.
		#' @details 
		#' A concise (1-5 lines) description of what the function does. ~~
		#' The output is a dataframe with 6 columns:\cr
		#' -- 2 columns of inidices of the matching elements of the two vectors;\cr
		#' -- 2 columns of the matching elements of the two vectors;\cr
		#' -- 2 columns of the matching cleaned-up string elements of the two vectors;\cr
        #' @param what_vect Vector to compare, element-by-element.
		#' @param to_what_vect vector to compare to.
		#' @param maxDist Measure of difference between vectors, measured as a fraction of string length. Can take values from 0 to 1.
		#' @param dictionary_list List of values to replace before matching.
		#' @param replacement_vect Vector of values to replace those values in the \code{dictionary_list}.
		#' @param no_exact_match (Boolean) Discard all exact matches? 
		#' May be useful if your list has exact matches (which you know about), 
		#' and you just want to focus on approximate matches.
		#' @author Ivan Grishagin
		
    require("dplyr")
    require("qdap")
    require("stringdist")
    #remove meaningless words from both 
    #prep dictionaries
    
    if(is.null(dictionary_list)){
        message("Setting default dictionary and replacement...")
        dictionary_list<-
            list(c("\\band\\b"
                   ,"\\bof\\b"
                   ,"\\bits\\b"
                   ,"\\bin\\b"
                   ,"\\bby\\b"
                   ,"\\bduring\\b"
                   ,"\\bthe\\b"
                   ,"-"
                   ,"/"
                   ,"\\bsignaling\\b"
                   ,"\\bpathway[s]*\\b"
                   ,"\\bregulat(ion|ed)\\b"
                   ,"\\(.*?\\)")
                 ,"interleukin"
                 )
        replacement_vect<-c(" ","il")
    } else {
        dictionary_list<-list(dictionary_list)
        if (length(dictionary_list)!= length(replacement_vect)){
            stop("dictionary_list length of "
                 ,length(dictionary_list)
                 ," is not equal to replacement_vect length of "
                 ,length(replacement_vect))
        }
    }
    
    #...and remove those words
    #in addition, replace some words for consistency
    #...lower
    what_vect_clean<-
        what_vect %>%
        tolower
    to_what_vect_clean<-
        to_what_vect %>%
        tolower
    #...remove/replace words
    for(index in 1:length(dictionary_list)){
        what_vect_clean<-
            what_vect_clean %>%
            mgsub(dictionary_list[[index]]
                  ,replacement_vect[index],.,fixed=FALSE)

        to_what_vect_clean<-
            to_what_vect_clean %>%
            mgsub(dictionary_list[[index]]
                  ,replacement_vect[index],.,fixed=FALSE)
    }
    #...clean up spaces
    what_vect_clean<-
        what_vect_clean %>%
        gsub("\\s+"," ",.,fixed=FALSE) %>%
        trimws
    
    to_what_vect_clean<-
        to_what_vect_clean %>%
        gsub("\\s+"," ",.,fixed=FALSE) %>%
        trimws
    
    #reorder words alphabetically
    what_vect_clean<-
        sapply(what_vect_clean        
               ,function(element){
                   strsplit(element,split=" ") %>%
                       unlist %>%
                       sort %>%
                       paste(collapse=" ")
               })
    to_what_vect_clean<-
        sapply(to_what_vect_clean        
               ,function(element){
                   strsplit(element,split=" ") %>%
                       unlist %>%
                       sort %>%
                       paste(collapse=" ")
               }) 
    
    
    
    #go element-by-element and search for matches
    amatch_result<-
        sapply(what_vect_clean
               ,function(element){
                   #perform amatch using distance 
                   #as fraction of the string length
                   maxDist_char<-
                       round(maxDist*nchar(element))
                   if(maxDist_char<1){
                       maxDist_char<-1
                   }
                   #check if we want to remove exact matches
                   #and if so, remove them before performing the search
                   to_what_temp<-
                       to_what_vect_clean
                   
                   if (no_exact_match){
                       to_what_temp[to_what_temp==element]<-NA
                   } 
                   match.element<-
                       amatch(element
                              ,to_what_temp
                              ,maxDist = maxDist_char)
                   return(match.element)
                   
               })
    
    
    
    #assemble the data frame
    amatch_df<-data.frame(what_ind=1:length(what_vect)
                          ,to_what_ind=amatch_result)
    
    amatch_df<-amatch_df[complete.cases(amatch_df),]
    amatch_df<-amatch_df %>%
        mutate(what_vect=what_vect[what_ind]
               ,to_what_vect=to_what_vect[to_what_ind]
               ,what_vect_clean=what_vect_clean[what_ind]
               ,to_what_vect_clean=to_what_vect_clean[to_what_ind])
    return(amatch_df)
    
}
