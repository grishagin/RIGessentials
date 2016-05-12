tkradio_from_vect<-
    function(vect
             ,window_title="Choose an option."
             ){
        require(tcltk)
        #initialize window
        window <- tktoplevel()
        tkwm.title(window,"Selector")
        #choose default option
        rbValue <- tclVar(vect[1])
        #add title
        tkgrid(tklabel(window
                       ,text = window_title)
               ,columnspan = 2
               ,padx = 10
               ,pady = c(15, 5))
        
        #add options/controls for every vector element
        for (index in 1:length(vect)){
            rbindex<-paste0("rb",index)
            window$env[[rbindex]] <- tkradiobutton(window)
            tkconfigure(window$env[[rbindex]], variable = rbValue, value = vect[index])
            tkgrid(tklabel(window
                           ,text = vect[index])
                   ,window$env[[rbindex]]
                   ,padx = 10
                   ,pady = c(0, 5))
        }
        
        #what happens when ok button is clicked
        onOK<- 
            function() {
            rbVal<- 
                as.character(tclvalue(rbValue))
            tkdestroy(window)
            env<-parent.env(environment())
            env$val<-rbVal
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