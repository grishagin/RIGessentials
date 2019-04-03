merge_cols_shorten_df<-
  function(dFrame=NULL
           ,colKey=NULL
           ,colsToMerge=NULL
           ,patternToMerge="|"
           ,return_other_cols=FALSE){
    #' @export
    #' @title 
    #' Merge Columns in Dataframe Using Pattern
    #' @description 
    #' Looks up duplicate values in the \code{colKey} column, and then merges all corresponding
    #' values in the \code{colsToMerge} column, separating them with a \code{patternToMerge} separator.
    #' @param dFrame Dataframe or data.table object.
    #' @param colKey	Key column. Duplicate values are looked up in this column.
    #' @param colsToMerge Columns to merge and return. If \code{NULL}, merges all but the \code{colKey} column. 
    #' In \code{colKey} column, duplicate values will be removed, and then this column will be bound to the merged columns.
    #' @param patternToMerge Pattern to use for merging of the values. Defaults to pipe (\code{|}).
    #' @param return_other_cols (boolean) 
    #' Return any columns besides \code{colKey} and \code{colsToMerge}?
    #' If so, all merged values in cols to merge will be repeated.
    #'
    #' @author Ivan Grishagin
    
    library(tidyverse)
    library(data.table)
    
    message("line 22")
    if (is.null(colsToMerge)) {
      colsToMerge<- 
        colnames(dFrame)
      colsToMerge<- 
        colsToMerge[colsToMerge != colKey]
    }
    message("line 29")
    
    if(is.null(dFrame)){
      stop("merge_cols_shorten_df: dFrame was not specified and cannot be NULL!")
    }
    message("line 34")
    #ensure dFrame is a data table
    dFrame<-
      dFrame %>% 
      as.data.table
    message("line 39")

    dFrame_proc<-
      dFrame[,lapply(.SD,paste,collapse=patternToMerge)
             ,.SD=colsToMerge
             ,by=colKey]

    
    message("line 55")
    
    if(return_other_cols){
      dFrame_proc<-
        dFrame_proc %>% 
        merge(y = dFrame[,(colsToMerge) := NULL]
              ,by=colKey
        )
    }
    
    return(dFrame_proc)
  }
