merge_cols_shorten_df <-
    function(dFrame=NULL
             ,colKey=NULL
             ,colsToMerge=NULL
             ,patternToMerge="|"){
        require(plyr)
        require(data.table)
        
        if (is.null(colsToMerge)) {
            colsToMerge<- 
                colnames(dFrame)
            colsToMerge<- 
                colsToMerge[colsToMerge != colKey]
        }
        if(is.null(dFrame)){
            stop("merge_cols_shorten_df: dFrame was not specified and cannot be NULL!")
        }
        
        #ensure dFrame is a data table
        dFrame<-
            dFrame %>% 
            as.data.table
        
        dFrame_proc<-
            dFrame[,.(newcol=paste(get(colsToMerge[1])
                                   ,collapse=patternToMerge))
                   ,by=colKey]
        
        if(length(colsToMerge)>1){
            for (cndex in 2:length(colsToMerge)){
                dFrame_proc<-
                    dFrame[,.(newcol=paste(get(colsToMerge[cndex])
                                           ,collapse=patternToMerge))
                           ,by=colKey] %>% 
                    dplyr::select(newcol) %>% 
                    cbind.data.frame(dFrame_proc
                                     ,.)
            }
        }
        colnames(dFrame_proc)<-
            c(colKey,colsToMerge)

        return(dFrame_proc)
    }