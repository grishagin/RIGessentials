find_all_matches<-
    function(x_vect
             ,y_vect
             ,separator="|"
             ,return_first_match="FALSE"){
		    #' @export
        #' @title
        #' Find All Matches in List
        #' @description 
        #' For each element of one list, finds all matches in the other list.
        #' @details 
        #' Returns a vector. By default, merges multiple matches via a \code{separator}.
        #' @param x_vect Vector, elements of which are to be compared.
        #' @param y_vect Vector, where matches are to be found.
        #' @param separator See \code{Details}.
        #' @param return_first_match (Boolean) Return only the first match? 
        #' In case multiple matches are found, sorts them and returns the first one.

        #' @author Ivan Grishagin
		
        internal_find_all_matches<-
            function(x
                     ,y_vect){
                result<-
                    which(y_vect %in% x)
                if(length(result)<1){
                    result<-NA
                }
                return(result)
            }
        
        result<-
            x_vect %>%
            lapply(internal_find_all_matches
                   ,y_vect=y_vect)

        #if unlisted result is of same length as result
        #return unlisted result and inform user
        unlisted_result<-
            unlist(result)
        if(length(unlisted_result)==length(result)){
            result<-
                unlisted_result  
            message("Hey, all matches are unique!")
        } else if(return_first_match) {
            result<-
                result %>% 
                lapply(sort) %>% 
                lapply("[",1) %>% 
                unlist
            message("Hey, some items returned more than one matches, but will return only first ones!")
        } else {
            result[!is.na(result)]<-
                result[!is.na(result)] %>% 
                lapply(paste
                       ,collapse=separator) 
            result<-
                unlist(result)
            message("Hey, some items returned more than one matches, hence merging them via separator!")
        }
        return(result)
    }