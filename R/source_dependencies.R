source_dependencies<-
    function(dirs){
	    #' @export
		#' @title
        #' Source Adjunct R Scripts
        #' @description 
		#' Source all R scripts in the specified vector of directories.
		#' @param dirs vector of names of directories where to look for the R scripts.

        dependencies_files<-
            unlist(lapply(dirs
                          ,list.files
                          ,pattern = "\\.R"
                          ,full.names = TRUE
                          ,recursive = FALSE
                          ,ignore.case = TRUE))
        
        invisible(sapply(dependencies_files,source))   

        if(length(dependencies_files)<1){
            message("R scripts have not been found in the specified folder!")
        } else {
            message("All ",length(dependencies_files)," dependencies have been loaded!")
        }
        
    }
