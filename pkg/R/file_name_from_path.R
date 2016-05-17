file_name_from_path<-
    function(path_vect
             ,split="/"){
        result<-
            lapply(path_vect
                   ,FUN=function(path){
                       result<-
                           strsplit(path,split=split) %>%
                           unlist %>%
                           .[length(.)]
                       return(result)
                   })
        
    }