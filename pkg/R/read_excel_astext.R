read_excel_astext<-
    function(path
             ,sheet = 1
             ,col_names = TRUE
             ,na = ""
             ,skip = 0){
        #'@title 
        #'Read Excel Files as Text
        #'@description 
        #'A wrapper for \code{read_excel} function from \code{readxl} package. 
        #'Reads data from an Excel file (*.xls or *.xlsx) into a dataframe. 
        #'Key difference -- reads all columns as text, i.e. does not need the type to be specified for every column.
        #'@param path Path to the xls/xlsx file.
        #'@param sheet	Sheet to read. Either a string (the name of a sheet), or an integer (the position of the sheet). Defaults to the first sheet.
        #'@param col_names Either TRUE to use the first row as column names, 
        #'FALSE to number columns sequentially from X1 to Xn, or a character vector giving a name for each column.
        #'@param na Missing value. 
        #'By default, readxl converts blank cells to missing data. 
        #'Set this value if you have used a sentinel value for missing values.
        #'@param skip Number of rows to skip before reading any data.
        #'
        #'@author 
        #'Ivan Grishagin
        
        require(readxl)
        #first, read without specifying the type
        tempDF<-
            readxl::read_excel(path = path
                               ,sheet = sheet
                               ,col_names = col_names
                               ,col_types = NULL
                               ,na = na
                               ,skip = skip)
        #count number of columns
        colnum<-
            ncol(tempDF)
        rm(tempDF)
        
        if(colnum<1){
            stop("read_excel_astext: No columns were detected in the input Excel file!")
        }
        
        #read the file properly, having specified column type
        resultDF<-
            readxl::read_excel(path = path
                               ,sheet = sheet
                               ,col_names = col_names
                               ,col_types = rep("text",colnum)
                               ,na = na
                               ,skip = skip)
        return(resultDF)
    }