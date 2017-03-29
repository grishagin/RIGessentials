match_list_in_list<-
    function(list1
             ,list2){
        #' @title
        #' Match List in List
        #' @description 
        #' Compares two lists of vectors. 
        #' Each element of the first list is tested for overlap with all elements of the second list of vectors. 
        #' Indices of the elements, for which such overlap has been found, are returned.
        #' @details 
        #' Returns a list that has a length of \code{list1}, and contains \code{list2} indices (or NA values). 
        #' The function is quite efficient: 50K list vs. 4K list with ~50 members per list element comparison takes only ~5 min (i5 laptop).
        #' @param list1 List to compare.
        #' @param list2 List to compare to.
        #' 
        #' @author 
        #' Ivan Grishagin

        #compares two lists of vectors
        #i.e. finds overlaps between all members
        #of said lists 
        #and for each vector in the first list
        #returns **indices** of the vectors in the second list, 
        #which contain elements of that first list vector
        
        
        #convert second list into matrix
        #also check that each vector in the transposed list has one row
        #abort if not
        list2_m<-
            list2 %>% 
            lapply(t) %>% 
            rbind.fill.matrix 
        
        if(nrow(list2_m)!=length(list2)){
            stop("\nmatch_list_in_list: some elements of the list either are not vectors or are transposed.\nAborting!")
        }
        
        #for each vector in the first list...
        # comparison_results_list<-
        #     list1 %>% 
        #     lapply(FUN=function(vect){
        #         
        #         #...lookup all those elements in the matrix
        #         #then see in which rows those elements are
        #         #since matrix is evaluated by column,
        #         #doing modulo on a matrix item position number returns its row
        #         result<-
        #             which(list2_m %in% vect) %% nrow(list2_m) %>% 
        #             unique 
        #         return(result)
        #     }) 
        
        #...lookup all those elements in the matrix
        #then see in which rows those elements are
        #since matrix is evaluated by column,
        #doing modulo on a matrix item position number returns its row
        
        #which comparison matrix items are in each of the vectors
        which_list<-
            list1 %>% 
            lapply(FUN=function(vect){
                which(list2_m %in% vect)
            })
        #which of those results are zero length -- make them NA
        zero_len_logi<-
            which_list %>% 
            sapply(length)==0
        which_list[zero_len_logi]<-NA
        
        #make a matrix and calculate modulo of each matrix member
        modulo_m<-
            which_list %>% 
            lapply(t) %>% 
            do.call(rbind.fill.matrix
                    ,.) %% 
            nrow(list2_m)
        
        #those which are zeroes must be exact multiple of number of rows
        #i.e. they belong to last row
        modulo_m[modulo_m==0]<-
            nrow(list2_m)
        
        results_list<-
            1:nrow(modulo_m) %>% 
            lapply(FUN=function(rindex){
                modulo_m[rindex,]
            }) %>% 
            lapply(as.vector) %>% 
            lapply(unique)
        #remove NA values from non-NA list elements
        results_list[!is.na(results_list)]<-
            results_list[!is.na(results_list)] %>% 
            lapply(FUN=function(vect){
                vect[!is.na(vect)]
            })
        
        return(results_list)
    }