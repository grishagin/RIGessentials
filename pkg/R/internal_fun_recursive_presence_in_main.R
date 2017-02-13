internal_fun_recursive_presence_in_main<-
    function(funs_list
             ,str_vect){
        #first, get a logical vector of function names 
        #that are present in a given vector of strings
        present_logi<-
            names(funs_list) %>%
            sapply(function(fname){
                result<-
                    any(grepl(fname
                              ,str_vect))
            })
        result<-
            names(funs_list)[present_logi]
        
        if(any(present_logi)){
            #then for each present function represented as a string vector (fun_vect)
            #look if any other function name is mentioned inside these string vectors
            #i.e. if the present function relies on some other function on the list
            ref_fun_names<-
                funs_list[present_logi] %>%
                lapply(FUN=function(pres_fun_vect){
                    internal_fun_recursive_presence_in_main(funs_list=funs_list[!present_logi]
                                                            ,str_vect=pres_fun_vect)
                }) %>%
                unlist
            
            result<-
                c(result
                  ,ref_fun_names)
        } 
        return(unique(result))
    }