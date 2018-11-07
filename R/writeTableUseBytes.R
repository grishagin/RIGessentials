writeTableUseBytes <-
    function(dFrame
             ,fileName
             ,sep="\t"
             ,eol="\n"
             ,useBytes=TRUE
             ,quote="\""){
		    #' @export
		    #' @title
        #' Write Dataframe to File As-Is
        #' @description 
        #' Allows to use \code{useBytes} parameter 
        #' of the \code{writeLines} function. \cr
		    #' Useful to avoid encoding conversion issues.
        #' @param dFrame Dataframe to modify.
        #' @param fileName Output filename.
        #' @param sep Separator to use in the output file.
        #' @param eol End of line symbol.
        #' @param useBytes Write as-is? Defaults to \code{TRUE}.
        #' If \code{TRUE}, suppresses the re-encoding  
        #' of marked strings so they are passed byte-by-byte to the connection.
        #' @param quote (String) Which marks to use around each entry?
        #' Defaults to \code{"\""}.
        #' If \code{quote=""}, the text will be written "as is".
        #' 
        #' @author Ivan Grishagin
        
        lines_to_write<-
			      rep(NA
			          ,nrow(dFrame)+1)
        
        lines_to_write[1]<-
            paste0(quote
                   ,colnames(dFrame)
                   ,quote
                   ,collapse=sep)
        
        lines_to_write[2:length(lines_to_write)]<-
            apply(dFrame
                  ,MARGIN = 1
                  ,FUN=function(x){
                    paste0(quote
                           ,x
                           ,quote
                           ,collapse=sep)
                  })
        
        writeLines(text=lines_to_write
                   ,con=fileName
                   ,sep=eol
                   ,useBytes=useBytes)
    }
