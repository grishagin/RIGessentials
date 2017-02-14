update_RIGessentials<-
    function(){
        #unloadNamespace(ns = "RIGessentials")
        devtools::unload(pkg = inst("RIGessentials"))
        devtools::install_github("grishagin/RIGessentials",subdir = "pkg")
    }
