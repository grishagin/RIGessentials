######################################## idvect_to_df_by_index ########################################
idvect_to_df_by_index<-
    function(id_vect
             ,split="-"
             ,asnum=TRUE){
        #converts vector of strings that have a specific split
        #into a dataframe by splitting each string by split
        #and placing each resultant vector in a separate row
        #of the df
        #' @title
        #' Vector to DataFrame
        #' @description 
        #' Splits each element of a vector by the \code{split}, and converts the resultant list into a dataframe.
        #' @details 
        #' Returns a dataframe. By default, returns only numbers (see \code{asnum}) argument.
        #' @param id_vect Vector, elements of which are to be processed.
        #' @param split Character, on which to split the vector (not a pattern!). Defaults to \code{\"-"}.
        #' @param asnum Logical. Keep only numeric values? Defaults to \code{TRUE}.

        #' @author 
        #' Ivan Grishagin

        #if only numbers requested 
        #define function that does that
        #else function with the same name that returns the vector unchanged
        if (asnum){
            asnum_func<-
                function(vect){
                    as.numeric(vect)
                }
        } else {
            asnum_func<-
                function(vect){
                    vect
                }
        }
        
        dF<-
            suppressWarnings(
                #split
                strsplit(id_vect
                         ,split=split
                         ,fixed=TRUE) %>%
                    #keep only numeric and non-NA components
                    lapply(FUN=function(listel){
                        listel<-
                            listel %>%
                            asnum_func %>%
                            .[!is.na(.)]
                    }) %>%
                    #turn each vector into matrix
                    lapply(matrix
                           ,nrow=1) %>%
                    #combine by row-binding
                    do.call(plyr::rbind.fill.matrix
                            ,.) %>%
                    as.data.frame 
            )
        #add colnames corresponding to nesting levels
        if(nrow(dF)<1 | ncol(dF)<1){
            return(NULL)
        }
        colnames(dF)<-
            1:ncol(dF)
        #add original ids
        # dF$id<-
        #     id_vect
        #replace NA with blanks
        dF[is.na(dF)]<-""
        
        return(dF)
    }
######################################## idvect_to_df_by_index ########################################
