writeTableUseBytes <-
    function(dFrame
             ,fileName
             ,sep="\t"
             ,eol="\n"
             ,useBytes=TRUE){
        lines_to_write<-rep(NA,nrow(dFrame)+1)
        lines_to_write[1]<-
            paste0(colnames(dFrame),collapse=sep)
        
        lines_to_write[2:length(lines_to_write)]<-
            apply(dFrame
                  ,MARGIN = 1
                  ,FUN=paste0
                  ,collapse=sep)
        
        writeLines(text=lines_to_write
                   ,con=fileName
                   ,sep=eol
                   ,useBytes=useBytes)
    }
