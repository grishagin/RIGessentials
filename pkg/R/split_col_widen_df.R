split_col_widen_df<-
    function(DF
             ,colToSplit
             ,split="\\|"
             ,newcolnames=NULL){
        
        #'@title 
        #'Split Column Based on Pattern, Expand DF Widthwise
        #'@description 
        #'Wrapper for a \code{strsplit} function. 
        #'In a given dataframe, splits values in \code{colToSplit} column using the \col{split} pattern, 
        #'then adds one extra column for each member of a split vector of maximum length to the dataframe. 
        #'If after splitting the number of resultant elements in each row is not the same, fills missing values with NA.
        #'@param DF Dataframe or data.table object.
        #'@param colToSplit	Column that is to be split.
        #'@param split Pattern to use for merging of the values. Defaults to pipe (\code{|}).
        #'@param newcolnames New column names (optional).
        #'
        #'@author 
        #'Ivan Grishagin
        
        
        #convert to data.table
        DF<-
            DF %>% 
            as.data.table
        
        #use data.table built-in function
        newcol_df<-
            DF[,tstrsplit(get(colToSplit
                              ,envir = environment())
                          ,split=split)]
        
        #add column names
        # if (is.null(newcolnames) |
        #     length(newcolnames)!=ncol(newcol_df)){
        #     colnames(newcol_df)<-
        #         paste(colToSplit
        #               ,1:ncol(newcol_df)
        #               ,sep="")
        # } else {
        #     colnames(newcol_df)<-
        #         newcolnames
        # }
        if(is.null(newcolnames) |
           length(newcolnames)<ncol(newcol_df)){
            newcolnames<-
                paste(colToSplit
                      ,1:ncol(newcol_df)
                      ,sep="")
        }else if(length(newcolnames)>ncol(newcol_df)){
            na_cols<-
                newcolnames[(ncol(newcol_df)+1):length(newcolnames)]
            newcol_df[,(na_cols) := NA_character_]
        }
        
        colnames(newcol_df)<-
                newcolnames
        
        #find # of column that was split
        colToSplit_num<-
            match(colToSplit
                  ,colnames(DF))
        #then merge all cols before this one
        #new cols
        #and all cols after the one that was split
        DF<-
            cbind.data.frame(DF[,1:(colToSplit_num-1),with=FALSE]
                             ,newcol_df
                             ,DF[,(colToSplit_num+1):ncol(DF),with=FALSE])
        
        return(DF)
        
    }