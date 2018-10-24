prepareSession <-
    function(work_dir=getwd()){
        #' @export
        #' @title Prepare New Session
        #' @description Function prepares a new session:\cr
        #'      1) sets working directory to the desired one;\cr
        #'      2) sets options(stringsAsFactors = FALSE);\cr
        #' @param work_dir (String) string specifying desired working directory.  
        #' If directory does not exist, prompts the user to choose one using a tkwidget (Windows-only).
        #' @author Ivan Grishagin
        
        #avoid changing the encoding (deprecated in v4.0)
        #if(nolocale){
        #    Sys.setlocale(category = "LC_ALL", locale = "C")
        #}
		
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
