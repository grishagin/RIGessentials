trim_fun_file_based_on_main<-
    function(fun_file_path
             ,main_file_path
             ,one_file=TRUE
             ,save_dir=getwd()){
        require(dplyr)
        #function takes in a file with functions and "main" code
        #then evaluates which functions are present in "main" code
        #and keeps only those ones
        
        #source all functions
        invisible(sys.source(fun_file_path
                             ,envir = environment()))
        #find all of their names
        funs_all<-
            ls(envir = environment()) %>%
            .[!(. %in% c("fun_file_path","main_file_path"))]
        #read "main" script
        main<-
            readLines(main_file_path)
        #find which funs are in use in "main" code
        funs_present<-
            sapply(funs_all
                   ,FUN=function(fun_name){
                       length(grep(fun_name,main))>0
                   }) %>%
            unlist %>%
            funs_all[.]
        
        #vector of strings from functions file
        funs_vect<-
            readLines(fun_file_path)
        #indices strings that contain names of all functions
        all_fun_indices<-
            which(grepl(paste(funs_all
                              ,collapse="|"),funs_vect) &
                      grepl("<-",funs_vect))
        #indices of lines with names of present functions
        present_fun_indices<-
            which(grepl(paste(funs_present
                              ,collapse="|"),funs_vect) &
                      grepl("<-",funs_vect))
        #vector to store correct order of functions
        funs_present_new<-NULL
        #split vector of lines by functions
        funs_list<-
            lapply(present_fun_indices
                   ,FUN=function(fstart){
                       #get the end as the next function index minus one line
                       fend<-
                           all_fun_indices[which(all_fun_indices==fstart)+1]-1
                       
                       while(length(grep("}",funs_vect[fend]))<1){
                           #if the closing function bracket is not in the line
                           #step one line up
                           fend<-fend-1
                           if(fend==fstart){
                               stop("Check start position ", fstart,". Can't find the end.")
                           }
                       }
                       #now ensure that line with start index 
                       #contains function name
                       while(length(grep(paste(funs_present
                                               ,collapse="|"),funs_vect[fstart]))<1){
                           #if the closing function bracket is not in the line
                           #step one line up
                           fstart<-fstart-1
                       }
                       #get the actual function name
                       #by looking up each of the "present" functions in the current "start" line
                       fun_name<-
                           sapply(funs_present
                                  ,FUN=function(fname){
                                      #use negative lookaheads and lookbehinds
                                      found_ind<-
                                          length(grep(paste0("(?<!\\.)\\b"
                                                             ,fname
                                                             ,"\\b(?!\\.)")
                                                      ,funs_vect[fstart]
                                                      ,perl = TRUE)) 
                                      if(found_ind>1){
                                          stop("Function ",fname," found ",found_ind,"times! Aborting.")
                                      } else if(found_ind<1){
                                          return(NULL)
                                      } else {
                                          return(fname)
                                      }
                                  }) %>%
                           unlist
                       if(is.null(fun_name)){
                           stop("Couldn't find any function in line ",fstart," (",funs_vect[fstart],")")
                       }
                       env<-parent.env(env = environment())
                       env$funs_present_new[length(env$funs_present_new)+1]<-
                           fun_name

                       separator<-
                           paste0(rep("#", 40),collapse = "") %>%
                           paste(., fun_name, ., sep=" ")
                       
                       return(c(separator
                                ,funs_vect[fstart:fend]
                                ,separator
                                ,""))
                   })

        #file name
        fun_file_name<-
            strsplit(fun_file_path,split="/") %>%
            unlist %>%
            .[length(.)]
        #write those pertaining funs to file
        if(one_file){
            #define filename to save to
            fileName<-
                file.path(save_dir
                          ,paste(fun_file_name
                                 ,"TRIMMED.R"
                                 ,sep="-"))
            write(unlist(funs_list),
                  fileName)
        } else {
            #define filename to save to
            invisible(lapply(1:length(funs_list)
                             ,FUN=function(index){

                                 fileName<-
                                     file.path(save_dir
                                               ,paste0(Sys.Date()
                                                      ,"_"
                                                      ,funs_present_new[index]
                                                      ,".R"))
                                 write(funs_list[[index]],
                                       fileName)
                                 
                             }))
        }
    }