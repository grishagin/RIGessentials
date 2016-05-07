loadPackages <-
function(packages=NULL){
        #function checks if packages are installed
        #if not - installs them
        #then loads them all
        require("tcltk"
                ,quietly = TRUE
                ,character.only = TRUE
                ,warn.conflicts=FALSE)  
        if(is.null(packages)){
            packages<-c("arrayQualityMetrics"
                        ,"limma"
                        ,"beadarray"
                        ,"statmod"
                        ,"illuminaMousev2.db"
                        ,"gplots"
                        ,"xlsx"
                        ,"XLConnect"
                        ,"VennDiagram"
                        ,"annotate"
                        ,"stringr"
                        ,"gtools")
        }

        #find out what's missing
        missing_packages<-packages[!(packages %in% installed.packages()[,1])]
        
        #try installing missing packages from R repository
        if(length(missing_packages)>0){
                suppressWarnings(install.packages(missing_packages,
                                                  repos='http://cran.us.r-project.org'))
        }
        
        #find out what's missing again in case it is
        missing_packages<-packages[!(packages %in% installed.packages()[,1])]
        
        #try installing missing packages from bioconductor
        if(length(missing_packages)>0){
                source("http://bioconductor.org/biocLite.R")
                suppressWarnings(BiocInstaller:::biocLite(missing_packages
                                                          ,suppressUpdates=TRUE
                                                          ,ask=FALSE))
        }
        
        #find out what's missing again in case it is
        missing_packages<-packages[!(packages %in% installed.packages()[,1])]
        
        if (length(missing_packages)>0){
                errorMessage<-paste0("Some of the packages could not be installed:\n ", 
                                     paste(missing_packages,
                                           collapse=" "),
                                     "Click 'OK' to abort. After that, install these packages.")
                tkmessageBox(message = errorMessage, icon = "error", type = "ok")
                
                if ("rJava" %in% missing_packages){
                        errorMessage<-paste0("Java is likely missing.\n ",
                                             "Click 'OK' to abort. After that, install Java.")
                        tkmessageBox(message = errorMessage, icon = "error", type = "ok")
                }
                stop()
        }
        
        #load all packages
        suppressWarnings(sapply(packages
                                ,require
                                ,quietly = TRUE
                                ,character.only = TRUE
                                ,warn.conflicts=FALSE))     
        cat("All packages were successfully loaded!")
        tkmessageBox(message = "All packages were successfully loaded!", icon = "info", type = "ok")
        
}
