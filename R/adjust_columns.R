adjust_columns <-
  function(df_to_adjust=NULL
           ,df_template=NULL){
    
    #' @export
    #' @title Add Columns from Another DataFrame
    #' @description Add columns to one dataframe based on another (template)
    #' such that the first would keep its original ones 
    #' and incorporate all from the second.
    #' @param df_to_adjust (dataframe) Dataframe to which new columns will be added.
    #' @param df_template (dataframe) Dataframe, whose column names will be
    #' compared to \code{df_to_adjust}, and then added to the latter.
    #' 
    #' @author Ivan Grishagin
    
    if (is.null(df_to_adjust) |
        (is.null(df_template))){
      stop("Not all mandatory parameters provided.")
    }
    
    unique_colnames<-
      unique(c(colnames(df_to_adjust)
               ,colnames(df_template)))
    
    df_to_adjust[,unique_colnames[!(unique_colnames %in% colnames(df_to_adjust))]]<-
      NA
    return(df_to_adjust)
  }
