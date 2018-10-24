stripNames <-
	function(input){
		#' @export
		#' @title
    #' Strip Names from Object
    #' @description 
    #' Takes in an object and tries to remove names. 
		#' @param input Object from which the names are to be removed. 
		
        tryCatch(names(input)<-NULL
                 ,error=function(e){
                     message("Can't strip the names. Please, check input.")
                 })
        return(input)
    }
