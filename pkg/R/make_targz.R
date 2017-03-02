make_targz<-
    function(input_names
             ,output_name="default"){
        #' @title
        #' Make tar.gz Archive
        #' @description 
        #' Performs a "tar -cvzf ..." system call. Only works in Linux OS.
        #' @param input_names A vector of input file names.
        #' @param output_name Desired output file name.
        
        #' @author 
        #' Ivan Grishagin
        
        #OS check
        sysname<-
            Sys.info()["sysname"] %>% 
            tolower
        if(sysname!="linux"){
            message("make_tar: your OS is not supported!")
        }
        #default output name
        if(output_name=="default"){
            output_name<-
                paste(Sys.Date()
                      ,"make_targz_output.tar.gz"
                      ,sep="_")
        }
        
        archive_command<-
            paste("tar -cvzf"
                  ,output_name
                  ,paste(input_names
                         ,collapse=" "))
        system(archive_command)
    }