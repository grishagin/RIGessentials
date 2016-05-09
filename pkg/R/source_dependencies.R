source_dependencies<-
    function(dirs){
        dependencies_files<-
            unlist(lapply(dependencies_dir
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
