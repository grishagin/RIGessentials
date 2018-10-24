split_fun_file_into_many<-
    function(fun_file_path
             ,write_to_files=TRUE){
    		#' @export
    		#' @title
            #' Split One File with Functions into Many
            #' @description 
            #' Takes in a file with functions and splits it up into separate files.
    		#' @param fun_file_path Path to file with functions.
    		#' @param write_to_files Write to files? 
    		#' If \code{TRUE} (default), will write to files, 
    		#' if not -- will return list of functions, 
    		#' where each function is represented as a vector of strings.
		
        library(dplyr)
        
        #source all functions
        invisible(sys.source(fun_file_path
                             ,envir = environment()))
        #find all of their names
        fun_names<-
            ls(envir = environment()) %>%
            .[!(. %in% c("fun_file_path"
                         ,"write_to_files"))]
        
        #vector of strings from functions file
        char_vect<-
            readLines(fun_file_path)
        
        #indices strings that contain names of all functions
        #and the <- symbol on the same line
        fun_indices_START<-
            fun_names %>%
            sapply(FUN=function(fname){
                findex<-
                    which(grepl(paste0("(?<!\\.)\\b"
                                       ,fname
                                       ,"\\b(?!\\.)")
                                ,char_vect
                                ,perl = TRUE) &
                              grepl("<-"
                                    ,char_vect))
                if(length(findex)!=1){
                    stop("Found "
                         ,length(findex)
                         ," matches for name "
                         ,fname
                         ,". Expecting one, hence aborting.")
                }
                return(findex)
            }) %>%
            #sort the indices from lower to higher
            #to accommodate further search for fun_indices_END
            sort
        #replace names with re-sorted names
        fun_names<-
            names(fun_indices_START)
        
        #and indices of where those functions end
        fun_indices_END<-
            c(fun_indices_START[2:length(fun_indices_START)]-1
              ,length(char_vect)) 
        names(fun_indices_END)<-
            fun_names
        
        funs_list<-
            lapply(1:length(fun_names)
                   ,FUN=function(findex){
                       
                       fstart<-
                           fun_indices_START[findex]
                       fend<-
                           fun_indices_END[findex]
                       
                       while(length(grep("}"
                                         ,char_vect[fend]))<1){
                           #if the closing function bracket is not in the line
                           #step one line up
                           fend<-fend-1
                           if(fend<=fstart){
                               stop("Check start position "
                                    ,fstart
                                    ,". Can't find the end.")
                           }
                       }
                       separator<-
                           paste0(rep("#", 40),collapse = "") %>%
                           paste(.
                                 ,fun_names[findex]
                                 ,.
                           )
                       
                       return(c(separator
                                ,char_vect[fstart:fend]
                                ,separator)
                       )
                   })
        names(funs_list)<-
            fun_names
        
        if(write_to_files){
            #define filename to save to
            invisible(lapply(1:length(funs_list)
                             ,FUN=function(index){
                                 dir.create("./separate_functions"
                                            ,showWarnings = FALSE)
                                 fileName<-
                                     file.path("./separate_functions"
                                               ,paste0(fun_names[index]
                                                       ,".R"))
                                     
                                 write(funs_list[[index]]
                                       ,fileName)
                                 })
                      )
            return("All done!")
        } else {
            return(funs_list)
        }
        
    }