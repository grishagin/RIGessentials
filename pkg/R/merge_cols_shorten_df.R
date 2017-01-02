merge_cols_shorten_df <-
    function(dFrame=NULL
             ,colKey=NULL
             ,colsToMerge=NULL
             ,patternToMerge="|"){
        require(plyr)
        require(data.table)
        
        #ensure dFrame is dataframe
        dFrame<-
            as.data.frame(dFrame)
        
        #if the columns to merge were not indicated
        #merge all of them
        if (is.null(colsToMerge)){
            colsToMerge<-
                colnames(dFrame)
            #exclude colKey
            colsToMerge<-
                colsToMerge[colsToMerge!=colKey]
        }
        
        #find duplicate keys in key column
        duplicate_keys<-
            table(dFrame[,colKey]) %>%
            .[.>1] %>%
            names
        
        #cycle through columns and collapse values in cols to merge
        #based on duplicate ids in key column
        
        merged_id_df<-
            adply(.data = duplicate_keys
                  ,.margins = 1
                  ,.fun = function(KEY){
                      merged_id_df<-
                          dFrame[dFrame[,colKey] %chin% KEY
                                 ,colsToMerge
                                 ,drop = FALSE] %>%
                          lapply(paste,collapse=patternToMerge) %>%
                          as.data.frame
                      
                      merged_id_df$KEY<-KEY
                      
                      colnames(merged_id_df)<-
                          c(colsToMerge
                            ,colKey)
                      
                      return(merged_id_df)
                  }) %>%
            dplyr::select(-1)
        
        #remove duplicate rows from original df
        #append merged rows df to original one
        dFrame<-
            dFrame %>%
            .[!(dFrame[,colKey] %chin% duplicate_keys)
              ,c(colKey,colsToMerge)] %>%
            filter(!(colKey %chin% duplicate_keys)) %>%
            rbind(merged_id_df)
        
        return(dFrame)
    }
