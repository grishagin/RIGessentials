trim_fun_file_based_on_main<-
    function(fun_file_path
             ,main_file_path
             ,one_file=TRUE
             ,save_dir=getwd()){
		#' @export
		#' @title
        #' Trim Functions File Based on Main
        #' @description 
        #' Takes in a file with functions and a file with "main" code 
        #' then evaluates which functions are present in "main" code
        #' and keeps only those ones.
		#' @param fun_file_path Path to file with functions.
		#' @param main_file_path Path to file with main code.
		#' @param one_file (Boolean) Output as one file? Defaults to \code{TRUE}.
		#' @param save_dir Where to save the file?
		
        require(dplyr)
        
        
        #get all functions from the functions file
        all_funs_list<-
            split_fun_file_into_many(fun_file_path
                                     ,write_to_files=FALSE)
       
        #read "main" script
        main<-
            readLines(main_file_path)
        
        
        #find all functions that are required for the main scrip to run
        #including all 
        req_funs<-
            funs_in_main(funs_list=all_funs_list
                         ,str_vect=main)
        #list of required functions
        req_funs_list<-
            all_funs_list[names(all_funs_list) %in% req_funs]
        
        #file name
        fun_file_name<-
            file_name_from_path(fun_file_path)
        
        #write those pertaining funs to file
        if(one_file){
            #define filename to save to
            fileName<-
                file.path(save_dir
                          ,paste(fun_file_name
                                 ,"TRIMMED.R"
                                 ,sep="-"))
            write(unlist(req_funs_list),
                  fileName)
        } else {
            #define filename to save to
            invisible(lapply(1:length(req_funs_list)
                             ,FUN=function(index){
                                 fileName<-
                                     file.path(save_dir
                                               ,paste0(names(req_funs_list)[index]
                                                      ,".R"))
                                 write(req_funs_list[[index]],
                                       fileName)
                                 
                             }))
        }
    }