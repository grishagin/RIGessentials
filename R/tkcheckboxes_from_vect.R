tkcheckboxes_from_vect<-
  function(vect
           ,window_title="Pick one or more options:"){
    #' @export
    #' @title
    #' Tk Checkbox Element From Vector
    #' @description 
    #' Takes in a vector and produces a form with TkCheckboxes with those labels as options. 
    #' @param vect Vector of strings to use as checkbox labels.
    #' @param window_title Title of the window with radio buttons.
    #' 
    #' @author Ivan Grishagin
    
    library(tcltk)
    #initialize window
    window <- tktoplevel()
    tkwm.title(window,"Selector")
    #choose default option
    chb_value_list<- 
      vect %>% 
      lapply(tclVar)
    #add title
    title_font<-
      tkfont.create(family = "Arial"
                    ,size = 12
                    ,weight = "bold")
    tkgrid(tklabel(window
                   ,text = window_title
                   ,font=title_font)
           ,columnspan = 2
           ,padx = 10
           ,pady = c(15, 5))
    
    #add options/controls for every vector element
    for (index in 1:length(vect)){
      rbindex<-paste0("rb",index)
      window$env[[rbindex]] <- tkcheckbutton(window)
      tkconfigure(window$env[[rbindex]]
                  ,variable = chb_value_list[[index]])
      tkgrid(tklabel(window
                     ,text = vect[index])
             ,window$env[[rbindex]]
             ,padx = 10
             ,pady = c(0, 5))
    }
    
    #what happens when ok button is clicked
    onOK<- 
      function() {
        chb_val<- 
          chb_value_list %>% 
          lapply(tclvalue) %>% 
          map_lgl(function(x){
            suppressWarnings(as.logical(as.integer(x)))
          }) %>% 
          vect[.] %>% 
          .[!is.na(.)]
        
        tkdestroy(window)
        env<-parent.env(environment())
        env$val<-chb_val
      }
    
    #add OK button function
    window$env$butOK<- 
      tkbutton(window, text = "OK", width = -6, command = onOK)
    tkgrid(window$env$butOK
           ,columnspan = 2
           ,padx = 10
           ,pady = c(5, 15))
    tkfocus(window)
    
    #wait for window response
    tkwait.window(window)
    #return selected value
    return(val)
  }