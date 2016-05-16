prepareSession <-
    function(work_dir="C:/"
             ,nolocale=FALSE){
        #avoid changing the encoding
        if(nolocale){
            Sys.setlocale(category = "LC_ALL", locale = "C")
        }
        #say no to stings as factors in dataframe
        options(stringsAsFactors = FALSE)
        tryCatch(setwd(work_dir)
                 ,error=function(e){
                     message("Look for a directory picker (it's likely in the back of the current window)...")
                     setwd(tcltk::tk_choose.dir())
                     message("Working directory set to "
                             ,getwd())
                 })
        
    }
