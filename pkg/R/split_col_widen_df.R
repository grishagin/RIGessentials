split_col_widen_df<-
    function(DF
             ,colToSplit
             ,split="\\|"
             ,newcolnames=NULL){
        #splits a column of a df using a provided split pattern
        #then adds one extra column for each member of a split vector to the dataframe
        newcols<-
            #split
            DF[,colToSplit] %>%
            strsplit(split=split) %>%
            #convert each row to matrix
            lapply(matrix
                   ,nrow=1) %>%
            #bind them and fill  (if necessary)
            do.call(plyr::rbind.fill.matrix
                    ,.) %>%
            as.data.frame
        if (is.null(newcolnames) |
            length(newcolnames)!=ncol(newcols)){
            colnames(newcols)<-
                paste(colToSplit
                      ,1:ncol(newcols))
        } else {
            colnames(newcols)<-
                newcolnames
        }
        
        #replace orig column with new ones
        #find original col number
        colToSplit_num<-
            match(colToSplit
                  ,colnames(DF))
        #then merge all cols before this one
        #new cols
        #and all cols after the one that was split
        DF<-
            cbind.data.frame(DF[,1:(colToSplit_num-1)]
                             ,newcols
                             ,DF[,(colToSplit_num+1):ncol(DF)])
        return(DF)
        
    }