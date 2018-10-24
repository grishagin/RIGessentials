split_cols_lengthen_df <-
    function(dFrame
             ,colsToSplit
             ,patternToSplit="\\|"
             ,at_once=TRUE){
		#' @export
		#' @title
        #' Split Column(s) Based on Pattern
        #' @description 
        #' Wrapper for a \code{strsplit} function. 
		#' In a given dataframe, split specified column values based on certain pattern, 
		#' which results in additional rows. All elements in other columns are duplicated.
        #' With multiple columns, it can work in two ways: 
		#' either one after the other with \code{at_once=FALSE} 
		#' or all at the same time with \code{at_once=TRUE} (default). 
		#' When all columns are processed at once, the function attempts to find 
		#' the same number of elements (that are produced after splitting) 
		#' in all columns for every row that contains the pattern.
		#' If some columns have just one element, it is replicated to match the maximum number of elements.
		#' If the number of elements is otherwise not equal between the columns, an error is thrown.
		
		#' @param dFrame Dataframe to process.
		#' @param colsToSplit Columns to process.
		#' @param patternToSplit Pattern to split the columns on.
		#' @param at_once (Boolean) Split all columns at once or one-by-one?

        if(!all(colsToSplit %in% colnames(dFrame))){
            message("split_cols_lengthen_df: not all desired columns were found in dataframe!")
            colsToSplit<-
                colsToSplit %>%
                .[. %in% colnames(dFrame)]
            message("Using only the following columns:\n"
                    ,paste(colsToSplit
                           ,collapse = ", "))
        }
        
        #ensure it's not a tibble
        dFrame<-
            dFrame %>%
            as.data.frame 
        
        if(at_once){
            #find affected rows
            rowsAffected<-
                lapply(colsToSplit
                       ,FUN=function(colN){
                           rowN<-
                               grep(patternToSplit
                                    ,dFrame[,colN])
                       }) %>%
                unlist %>%
                unique
            
            #if none affected, return original dataframe
            if (length(rowsAffected)<1){
                return(dFrame)
            }
            #add original order of rows
            dFrame<-
                dFrame %>% 
                mutate(orig_order=1:nrow(dFrame))
            
            aff_list<-
                aff_list_lens<-
                as.list(rep(NA,length(colsToSplit)))
            
            for (cndex in 1:length(colsToSplit)){
                #split the affected values
                aff_list[[cndex]]<-
                    dFrame[rowsAffected
                           ,colsToSplit[cndex]] %>% 
                    as.character %>% 
                    strsplit(split=patternToSplit)
                
                #find the lengths of each list element
                aff_list_lens[[cndex]]<-
                    aff_list[[cndex]] %>% 
                    lapply(length) %>% 
                    unlist
            }
            #find consensus max length for each element in each columns
            aff_list_lens_max<-
                aff_list_lens %>% 
                do.call(cbind
                        ,.) %>% 
                apply(MARGIN = 1
                      ,FUN = max)
            
            new_rowsAffected<-
                rep(rowsAffected
                    ,aff_list_lens_max)
            
            dFrame_aff<-
                dFrame[new_rowsAffected,]
            dFrame_nonaff<-
                dFrame[-rowsAffected,]
            
            #compare each column length to the consensus max
            #and replicate corresponding values if there's fewer of them
            for (cndex in 1:length(colsToSplit)){
                #tempcolumn -- actually, temp dataframe, 
                #as we also take into consideration original order
                tempcol<-
                    dFrame[,c("orig_order"
                              ,colsToSplit[cndex])] %>% 
                    #and only take affected rows
                    .[rowsAffected,]
                
                #which entries to simply replicate = where max number of elements exceeds
                #current column number of elements
                to_rep_rows<-
                    which(aff_list_lens_max > aff_list_lens[[cndex]])
                
                #perform a sanity check, i.e. exclude cases like  a|b|c in one column
                #and c|d in another
                if(any(aff_list_lens[[cndex]][to_rep_rows]>1)){
                    stop("split_cols_lengthen_df: some of your column '"
                         ,colsToSplit[cndex]
                         ,"' values have fewer elements than in other columns, but not one.")
                }
                #repeat rows which are to be repeated
                repd_rows<-
                    to_rep_rows %>% 
                    rep(.
                        ,aff_list_lens_max[.])
                
                #cook up replicated part of the dataframe
                part1<-
                    tempcol %>% 
                    .[repd_rows,]
                
                #now get those rows, which will be replicated
                #to accommodate properly split values from the list
                #take the other part of the dataframe,
                #and fill it with unlisted split values minus those values 
                #which got replicated in the previous part
                if(length(to_rep_rows)>0){
                    #if previously detected rows were non-zero, then
                    #exclude them from consideration
                    to_rep_rows<-
                        1:nrow(tempcol) %>% 
                        .[-to_rep_rows]
                } else{
                    #else simply take all rows of the temp dataframe in question
                    to_rep_rows<-
                        1:nrow(tempcol)
                }
                
                repd_rows<-
                    to_rep_rows %>% 
                    rep(.
                        ,aff_list_lens_max[.])
                #make this second part of the df by replicating the corresponding rows
                #and replacing the original values with the split data from the list
                part2<-
                    tempcol %>% 
                    .[repd_rows,]
                part2[,colsToSplit[cndex]]<-
                    aff_list[[cndex]][to_rep_rows] %>% 
                    unlist
                
                #merge and arrange according to original order
                #fill dataframe with new info while duplicating all relevant rows
                dFrame_aff[,colsToSplit[cndex]]<-
                    rbind(part1
                          ,part2) %>% 
                    arrange(orig_order) %>% 
                    .[,colsToSplit[cndex]]
            }
            #bind affected and unaffected parts of the df together
            #and arrange according to original order
            dFrame_proc<-
                rbind(dFrame_aff
                      ,dFrame_nonaff) %>% 
                arrange(orig_order) %>% 
                dplyr::select(-orig_order)
            
        } else {
            #if it is to be applied iteratively
            #i.e. one column after the other
            for(index in 1:length(colsToSplit)){
                dFrame_proc<-
                    split_cols_lengthen_df(dFrame
                                           ,colsToSplit=colsToSplit[index]
                                           ,patternToSplit=patternToSplit
                                           ,at_once=TRUE)
            }
        }
        
        dFrame_proc[dFrame_proc=="NA"]<-
            NA
        return(dFrame_proc)
    }
