trim_fun_file_based_on_main<-
    function(fun_file_path
             ,main_file_path
             ,one_file=TRUE
             ,save_dir=getwd()){
        require(dplyr)
        #function takes in a file with functions and "main" code
        #then evaluates which functions are present in "main" code
        #and keeps only those ones
        
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
            inner_fun_recursive_presence_in_main(funs_list=all_funs_list
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