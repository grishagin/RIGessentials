update_RIGessentials<-
    function(){
		#' @export
		#' @title
        #' Update RIGessentials
        #' @description 
        #' Update RIGessentials using devtools.
		
        unloadNamespace(ns = "RIGessentials")
        devtools::install_github("grishagin/RIGessentials")
    }
