update_RIGessentials<-
    function(){
        unloadNamespace(ns = "RIGessentials")
        devtools::install_github("grishagin/RIGessentials",subdir = "pkg")
    }
