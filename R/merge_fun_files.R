merge_fun_files<-
    function(source_dir=NULL
             ,pattern="\\.R"
             ,result_filename="merged.R"){
		#' @export
		#' @title 
        #' Merge Files into One File
        #' @description 
        #' Interactive function that takes in a directory or a list of files 
		#' (using a directory or file picker), and outputs one file with combined contents.
        #' @param source_dir Path to a directory with files (optional).
        #' @param pattern (Regular expression) What kind of files to look for? 
        #' @param result_filename Output file name.
        #'
        #' @author Ivan Grishagin
		
        require(RIGessentials)
        #see if all files in a give dir need to be merged
        response<-
            tcltk::tk_messageBox(type="yesno"
                                 ,message = paste0("Merge all files with pattern '",pattern,"' in directory?")
                                 ,icon="question")
        if(response=="yes"){
            #if so, check if dir has been provided
            if(is.null(source_dir)) {
                #if not, inquire for dir
                message("Look for a directory picker (it's likely in the back of the current window)...")
                source_dir<-
                    tcltk::tk_choose.dir()
            }
            #get a list of files
            files<-
                list.files(path = source_dir
                           ,pattern = pattern
                           ,full.names = TRUE
                           ,recursive = FALSE
                           ,ignore.case = TRUE)
        } else {
            #if not all files are desired, call a file picker
            files<-
                tcltk::tk_choose.files(caption = "Choose all pertaining files."
                                       ,multi = TRUE
                                       ,filters = matrix(c("Text", ".txt"
                                                           ,"R code", ".R"
                                                           ,"R code", ".r")
                                                         ,nrow = 4,ncol = 2, byrow = TRUE))
        }
        
        file_contents<-
            lapply(files
                   ,readLines)
        file_names<-
            file_name_from_path(files)
        separators<-
            paste(paste(rep("#",40),collapse="")
                  ,file_names
                  ,paste(rep("#",40),collapse=""))
        
        output_list<-
            as.list(rep(NA,3*length(separators)))
        #add separators
        output_list[c(seq(1,length(output_list),by=3)
                      ,seq(3,length(output_list),by=3))]<-
            separators
        #add file contents
        output_list[seq(2,length(output_list),by=3)]<-
            file_contents
        
        writeLines(text=unlist(output_list)
                   ,con=result_filename)              
            
        
    }