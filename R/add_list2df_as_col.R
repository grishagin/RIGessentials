add_list2df_as_col<-
    function(dframe
             ,list2add
             ,newcolname="list2add"){
		#' @export
        #' @title
        #' Add List as Column to Dataframe
        #' @description 
        #' Takes in a dataframe and a list, such that \code{length(list2add) == nrow(dframe)}. 
        #' Each dataframe row is replicated as many times as the length of the corresponding list element. 
        #' After that, the list is unlisted and added to the dataframe. 
        #' @param dframe Dataframe to modify.
        #' @param list2add A list that has the same length as the number of rows in a dataframe.
        #' @param newcolname A name for the new column.
        
        if(nrow(dframe)!=length(list2add)){
            warning("add_list2df_as_col: length(list2add) != nrow(dframe). Returning original dframe.")
            return(dframe)
        }
        #lengths of list elements
        lens<-
            list2add %>% 
            sapply(length)
        #replicate each row a number of times 
        #equal to the length of the corresponding list element
        newrows<-
            rep(1:nrow(dframe)
                ,times=lens)
        
        #cbind the new column (unlisted list) to the modified dframe
        dframe<-
            dframe[newrows,] %>% 
            cbind(unlist(list2add))
        
        #replace column name
        colnames(dframe)[ncol(dframe)]<-
            newcolname
        
        return(dframe)
    }