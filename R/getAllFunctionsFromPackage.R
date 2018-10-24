getAllFunctionsFromPackage<-
    function(package=NULL
             ,path=getwd()
             ,one_file=TRUE){
		#' @export
		#' @title
        #' Extract All Functions from Package
        #' @description 
        #' Extracts all functions from a given package into one or multiple files.
        #' @param package Name of the package to be subjected to dismantling.
        #' @param path Path to directory where to save the file(s) with functions.
        #' @param one_file (Boolean) One file (\code{TRUE}) or many (\code{FALSE})?
		#' @author Ivan Grishagin
		
        #extract all functions from any package
        
        if(is.null(package)) stop("Package name not supplied! Aborting.")
        
        funs_names<-ls(envir=asNamespace(package))
        
        funs_list<-
            lapply(funs_names
                   ,FUN=function(fun_name){
                       #define separator
                       separator<-
                           paste0(rep("#", 40),collapse = "") %>%
                           paste(., fun_name, ., sep=" ")
                       
                       #find function and extract its code
                       code<-
                           get(fun_name, envir=asNamespace(package))
                       codeText<-
                           capture.output(code)
                       #add separators and function name
                       #and remove <> line at the bottom
                       codeText<-
                           c(separator
                             ,paste0(fun_name,"<-")
                             ,codeText[-length(codeText)]
                             ,separator
                             ,"")
                       return(codeText)
                   })

        if(one_file){
            #define filename to save to
            fileName<-
                file.path(path
                          ,paste(Sys.Date()
                                 ,package
                                 ,"functions.R"
                                 ,sep="_"))
            write(unlist(funs_list),
                  fileName)
        } else {
            #define filename to save to
            invisible(lapply(1:length(funs_list)
                             ,FUN=function(index){
                                 fileName<-
                                     file.path(path
                                               ,paste(Sys.Date()
                                                      ,package
                                                      ,"functions"
                                                      ,funs_names[index]
                                                      ,".R"
                                                      ,sep="_"))
                                 write(funs_list[[index]],
                                       fileName)
                                 
                             }))
        }
    }
