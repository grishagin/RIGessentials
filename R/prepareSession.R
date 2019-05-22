prepareSession <-
  function(work_dir=getwd()
           ,create=FALSE){
    #' @export
    #' @title Prepare New Session
    #' @description Function prepares a new session:\cr
    #'      1) sets working directory to the desired one;\cr
    #'      2) sets options(stringsAsFactors = FALSE);\cr
    #' @param work_dir (String) String specifying desired working directory.  
    #' If directory does not exist, prompts the user to choose one using a tkwidget (Windows-only).
    #' @param create (Boolean) Create work directory?
    #' @author Ivan Grishagin
    
    #avoid changing the encoding (deprecated in v4.0)
    #if(nolocale){
    #    Sys.setlocale(category = "LC_ALL", locale = "C")
    #}
    
    #say no to stings as factors in dataframe
    options(stringsAsFactors = FALSE)
    
    if(create == TRUE){
      dir.create(work_dir)
    }
    
    tryCatch(setwd(work_dir)
             ,error=function(e){
               message("Look for a directory picker (it's likely in the back of the current window)...")
               setwd(tcltk::tk_choose.dir())
               message("Working directory set to "
                       ,getwd())
             })
    
  }
