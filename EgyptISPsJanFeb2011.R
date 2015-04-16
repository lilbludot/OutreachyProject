
source("uniqueServerFinderFunction.R")
source("dataConverterFunction.R")
dfJan <- read.csv("Egypt2011JanRTTComplete.csv", colClasses="character") 
dfFeb <- read.csv("Egypt2011FebRTTComplete.csv", colClasses="character") 
mainJanDF <- dataConverter(dfJan)
mainFebDF <- dataConverter(dfFeb)
JanServerList <- uniqueServerFinder(mainJanDF)
FebServerList <- uniqueServerFinder(mainFebDF)

###Getting the UK servers for Egypt during Jan and Feb 2011 
### and their Netherlands and Egypt clients


EgyptNethJan2011DF<-read.csv("UKServers.csv", colClasses="character")
Jan2011DF <- dataConverter(EgyptNethJan2011DF)
Jan2011DF$date <- as.character(Jan2011DF$date)
head(Jan2011DF)
EgyptJan2011DF <- filter(Jan2011DF, clientCountry=="Egypt")
NethJan2011DF <-filter(Jan2011DF, clientCountry =="Netherlands")
nrow(EgyptJan2011DF)
nrow(NethJan2011DF)
boxplot(EgyptJan2011DF$RTT , NethJan2011DF$RTT,  main="RTT's for the UK Servers of Egypt during January 2011", 
        xlab="", ylab="RTT", names=c("Egypt","Netherlands"),col=c("thistle","wheat"), outline=FALSE)
boxplot(EgyptJan2011DF$RTT , main="Egypt's RTT for its UK Servers in January 2011", 
        xlab="Index", ylab="RTT", col="thistle")
median(EgyptJan2011DF$RTT)
mean(EgyptJan2011DF$RTT)
median(NethJan2011DF$RTT)
mean(NethJan2011DF$RTT)
