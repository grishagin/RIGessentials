check_functions_in_main<-
    function(fun_file_path
             ,main_file_path){
        require(dplyr)
        #function takes in a file with functions and "main" code
        #then evaluates which functions are present in "main" code
        #and keeps only those ones
        
        #source all functions
        invisible(sys.source(fun_file_path
                             ,envir = environment()))
        #find all of their names
        fun_all<-
            ls(envir = environment()) %>%
            .[!(. %in% c("fun_file_path","main_file_path"))]
        #read "main" script
        main<-
            readLines(main_file_path)
        #find which funs are in use in "main" code
        funs_present<-
            sapply(fun_all
                   ,FUN=function(fun_name){
                       length(grep(fun_name,main))>0
                   }) %>%
            unlist %>%
            fun_all[.]

        #write those pertaining funs to file
        for(item in funs_present){
            #define separator
            separator<-
                paste0(rep("#", 40),collapse = "") %>%
                paste(., item, ., sep=" ")
            #define filename to save to
            fileName<-
                paste0(fun_file_path
                       ,"-TRIMMED.R")
            #write separator to file
            write(separator,
                  fileName,
                  append=TRUE)
            #write function name to file
            write(paste0(item,"<-"),
                  fileName,
                  append=TRUE)
            #find function and extract its code
            code<-
                get(item, envir=environment())
            codeText<-
                capture.output(code)
           
            #write code followed by separator to file
            write(codeText,
                  file = fileName,
                  append = TRUE)
            write(separator,
                  fileName,
                  append = TRUE)
            
        }
    }