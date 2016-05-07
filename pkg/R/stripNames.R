stripNames <-
function(input){
        tryCatch(names(input)<-NULL
                 ,error=function(e){
                     message("Can't strip the names. Please, check input.")
                 })
        return(input)
    }
