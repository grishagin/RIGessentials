merge_cols_shorten_df<-
    function(dFrame=NULL
             ,colKey=NULL
             ,colsToMerge=NULL
             ,patternToMerge="|"
             ,return_other_cols=FALSE){
        #'@title 
        #'Merge Columns in Dataframe Using Pattern
        #'@description 
        #'Looks up duplicate values in the \code{colKey} column, and then merges all corresponding
        #'values in the \code{colsToMerge} column, separating them with a \code{patternToMerge} separator.
        #'@param dFrame Dataframe or data.table object.
        #'@param colKey	Key column. Duplicate values are looked up in this column.
        #'@param colsToMerge Columns to merge and return. If \code{NULL}, merges all but the \code{colKey} column. 
        #'In \code{colKey} column, duplicate values will be removed, and then this column will be bound to the merged columns.
        #'@param patternToMerge Pattern to use for merging of the values. Defaults to pipe (\code{|}).
        #'@param return_other_cols Boolean. Return any columns besides \code{colKey} and \code{colsToMerge}?
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
                           ,by=colKey]$newcol %>% 
                    cbind(dFrame_proc
                          ,.)
            }
        }
        colnames(dFrame_proc)<-
            c(colKey
              ,colsToMerge)
        
        if(return_other_cols){
            #convert to data.frame for simplicity of processing
            dFrame<-
                as.data.frame(dFrame)
            dFrame_proc<-
                as.data.frame(dFrame_proc)
          
            #take rows of old dataframe and match them up with the new, shrunk one
            dFrame<-
                dFrame[match(dFrame_proc[,colKey]
                             ,dFrame[,colKey])
                       ,]
            #replace old cols with new ones in a shrunk old dFrame
            dFrame[,c(colKey,colsToMerge)]<-
                dFrame_proc
            
            dFrame_proc<-
                dFrame %>% 
                as.data.table
        }

        return(dFrame_proc)
    }
