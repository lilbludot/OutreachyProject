#This function takes as input a data frame with the schema provided below and 
#it plots the Roundtrip Time v Date and the mean Roundtrip time 

#Schema: "logTime"              - integer,  UTC time
#        "RTT"                  - integer, roundtrip time
#        "clientContinent"      - character, continent code of the client (optional)
#        "clientCountry"        - character, country of client (optional)
#        "clientCity"           - character, city of client (optional)
#        "serverContinent"      - character, continent code of server (optional)
#        "serverCountry"        - character, country of server (optional)
#        "serverCity"           - character, city of server (optional)
#        "Date"                 - POSIXlt, format "y-m-d h:min:sec"
library(stringr)
plotter <- function(df){
        meanRTT<- mean(df$RTT)
        plot(df$date, df$RTT, type="l", col="darkblue", ylab="", xlab="")
        abline(h=meanRTT, col="violetred4", lwd=2)
        yr<- str_sub(as.character(df$moDayYear[1]), start=-4)
        title(main=paste("Year:", yr), xlab="Date", ylab="Roundtrip Time",
        col.main="deeppink4", font.main = 4, cex.main=1.1, col.sub = "deeppink4",
        col.lab="deeppink4", cex.lab=1)
        legend('topright', c("RTT",paste("Mean Roundtrip Time",round(meanRTT, digits=2))), 
               lty=c(1,1), lwd=c(2.5,2.5),col=c("darkblue","violetred4")) 
}