merge_cols_shorten_df<-
    function(dFrame=NULL
             ,colKey=NULL
             ,colsToMerge=NULL
             ,patternToMerge="|"){
        #'@title 
        #'Merge Columns in Dataframe Using Pattern
        #'@description 
        #'Looks up duplicate values in the \code{colKey} column, and then merges all corresponding
        #'values in the \code{colToMerge} column, separating them with a \code{patternToMerge} separator.
        #'@param dFrame Dataframe or data.table object.
        #'@param colKey	Key column. Duplicate values are looked up in this column.
        #'@param colsToMerge Columns to merge and return. If \code{NULL}, merges all but the \code{colKey} column. 
        #'In \code{colKey} column, duplicate values will be removed, and then this column will be bound to the merged columns.
        #'@param patternToMerge Pattern to use for merging of the values. Defaults to pipe (\code{|}).
        #'
        #'@author 
        #'Ivan Grishagin
        
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