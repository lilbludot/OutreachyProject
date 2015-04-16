
uniqueServerFinder <- function(mainDF){
        library(dplyr)
        mainDF$date <- as.character(mainDF$date) 
        serverCountries <- unique(mainDF$serverCountry)
        serverCountries
        serverListPerCountry <- list()
        for (i in serverCountries){
            dummy <- filter(mainDF, serverCountry == i)
            x <- unique(dummy$serverIP)
            serverListPerCountry[[i]]<-x
            print(i)
            print(x)
        
        }
       return(serverListPerCountry) 
}
