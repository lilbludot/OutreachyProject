source("dataConverterFunction.R")
source("ByDayFunction.R")
source("plotterFunction.R")
library(dplyr); library(ggplot2)
dfList<-list()
dfList[[1]]<-read.csv("EgyptNov2009RTTComplete.csv", colClasses="character")
NOv2009DF<-dataConverter(Nov2009Char)
meanRTTNov2009 <- mean(Nov2009DF$RTT)
plotter(Nov2009DF)
dfList[[2]]<-read.csv("Egypt2009DecRTTComplete.csv", colClasses="character")
Dec2009DF<-plotRTTData(Dec2009Char)
meanRTTDec2009 <- mean(Dec2009DF$RTT)
dfList[[3]]<-read.csv("Egypt2010JanRTTComplete.csv", colClasses="character")
Jan2010DF<-plotRTTData(Jan2010Char)
meanRTTJan2010 <- mean(Jan2010DF$RTT)
dfList[[4]]<-read.csv("Egypt2010FebRTTComplete.csv", colClasses="character")
Feb2010DF<-plotRTTData(Feb2010Char)
meanRTTFeb2010 <- mean(Feb2010DF$RTT)

dfList[[5]]<-read.csv("Egypt2010MarRTTComplete", colClasses="character")
Mar2010DF<-plotRTTData(Mar2010Char)
meanRTTMar2010 <- mean(Mar2010DF$RTT)
dfList[[6]]<-read.csv("Egypt2010AprRTTComplete.csv", colClasses="character")
Apr2010DF<-plotRTTData(Apr2010Char)
meanRTTApr2010 <- mean(Apr2010DF$RTT)
dfList[[7]]<-read.csv("Egypt2010MayRTTComplete.csv", colClasses="character")
May2010DF<-plotRTTData(May2010Char)
meanRTTMay2010 <- mean(May2010DF$RTT)
dfList[[8]]<-read.csv("June2010.csv", colClasses="character")
June2010DF<-plotRTTData(June2010Char)
meanRTTJune2010 <- mean(June2010DF$RTT)
dfList[[9]]<-read.csv("Egypt2010JulyRTTComplete.csv", colClasses="character")
Jul2010DF<-plotRTTData(Jul2010Char)
meanRTTJul2010 <- mean(Jul2010DF$RTT)

dfList[[10]]<-read.csv("Egypt2010AugRTTComplete.csv", colClasses="character")
Aug2010DF<-plotRTTData(Aug2010Char)
meanRTTAug2010 <- mean(Aug2010DF$RTT)
dfList[[11]]<-read.csv("Egypt2010SeptRTComplete.csv", colClasses="character")
Sep2010DF<-plotRTTData(Sep2010Char)
meanRTTSep2010<-mean(Sep2010DF$RTT)
dfList[[12]]<-read.csv("Egypt2010OctRTComplete.csv", colClasses="character")
Oct2010DF<-plotRTTData(Oct2010Char)
meanRTTOct2010<-mean(Oct2010DF$RTT)
dfList[[13]]<-read.csv("Egypt2010NovRTTComplete.csv", colClasses="character")
Nov2010DF <-  plotRTTData(Nov2010Char)
meanRTTNov2010 <- mean(Nov2010DF$RTT)
dfList[[14]] <- read.csv("Egypt2010DecRTTComplete.csv", colClasses="character") 
Dec2010DF <- plotRTTData(Dec2010Char)
meanRTTDec2010<-mean(Dec2010DF$RTT)

dfList[[15]] <- read.csv("Egypt2011JanRTTComplete.csv", colClasses="character") 
Jan2011DF <-  plotRTTData(Jan2011Char)
meanRTTJan2011<-mean(Jan2011DF$RTT)
dfList[[16]] <- read.csv("Egypt2011FebRTTComplete.csv", colClasses="character") 
Feb2011DF <-  plotRTTData(Feb2011Char)
meanRTTFeb2011 <-mean(Feb2011DF$RTT)
dfList[[17]]<- read.csv("Egypt2011MarRTTComplete.csv", colClasses="character") 
Mar2011DF <-  plotRTTData(Mar2011Char)
meanRTTMar2011<-mean(Mar2011DF$RTT)
dfList[[18]] <- read.csv("Egypt2011AprRTTComplete.csv", colClasses="character") 
Apr2011DF <-  plotRTTData(Apr2011Char)
meanRTTApr2011 <- mean(Apr2011DF$RTT)
by_day_Nov2009 <- by_day(Nov2009DF)
by_day_Dec2009 <- by_day(Dec2009DF)
by_day_Jan2010 <- by_day(Jan2010DF)
by_day_Feb2010 <- by_day(Feb2010DF)
by_day_Mar2010 <- by_day(Mar2010DF)
by_day_Apr2010 <- by_day(Apr2010DF)
by_day_May2010 <- by_day(May2010DF)
by_day_June2010 <- by_day(June2010DF)
by_day_Jul2010 <- by_day(Jul2010DF)
by_day_Aug2010 <- by_day(Aug2010DF)
by_day_Sep2010 <- by_day(Sep2010DF)
by_day_Oct2010 <- by_day(Oct2010DF)
by_day_Nov2010 <- by_day(Nov2010DF)
by_day_Dec2010 <- by_day(Dec2010DF)
by_day_Jan2011 <- by_day(Jan2011DF)
by_day_Feb2011 <- by_day(Feb2011DF)
by_day_Mar2011 <- by_day(Mar2011DF)
by_day_Apr2011 <- by_day(Apr2011DF)

df_vect <- c(by_day_Nov2009, by_day_Dec2009, by_day_Jan2010, by_day_Feb2010, 
             by_day_Mar2010,
             by_day_Apr2010, by_day_May2010, by_day_June2010, by_day_Jul2010, 
             by_day_Aug2010, 
             by_day_Sep2010, by_day_Oct2010,  by_day_Nov2010, by_day_Dec2010, 
             by_day_Jan2011, 
             by_day_Feb2011, by_day_Mar2011, by_day_Apr2011 )



by_day_all <- rbind(by_day_Nov2009, by_day_Dec2009, by_day_Jan2010, by_day_Feb2010)

by_day_all <- rbind(by_day_all, by_day_Mar2010,
                    by_day_Apr2010, by_day_May2010, by_day_June2010, by_day_Jul2010)

by_day_all <- rbind(by_day_all, by_day_Aug2010, 
                    by_day_Sep2010, by_day_Oct2010,  by_day_Nov2010, by_day_Dec2010, 
                    by_day_Jan2011, 
                    by_day_Feb2011, by_day_Mar2011, by_day_Apr2011)     



daily_ts <- ts(by_day_all$avgRTT, freq =365, start=c(2009, 305))

plot(daily_ts)

daily_ts_may2010_apr2011 <- window(daily_ts, start=c(2010,121))
plot(daily_ts_may2010_apr2011)
abline(v=2011+30/365, col="red")
abline(h=mean(daily_ts_may2010_apr2011), col="blue")


avg_by_mo <- c(meanRTTNov2009, meanRTTDec2009, meanRTTJan2010, meanRTTFeb2010,
               meanRTTMar2010, meanRTTApr2010, meanRTTMay2010, meanRTTJune2010, 
               meanRTTJul2010, meanRTTAug2010, meanRTTSep2010, meanRTTOct2010,
               meanRTTNov2010, meanRTTDec2010, meanRTTJan2011, meanRTTFeb2011,
               meanRTTMar2011, meanRTTApr2011)
mo_names <- c("Nov2009", "Dec2009", "Jan2010", "Feb2010", "Mar2010", "Apr2010",
              "May2010", "June2010", "July2010", "Aug2010", "Sept2010", "Oct2010",
              "Nov2010", "Dec2010", "Jan2011", "Feb2011", "Mar2011", "Apr2011")
monthly_ts <- ts(avg_by_mo, freq=12, start=c(2009, 11))
monthly_ts
plot(monthly_ts)
abline(v=2010+3/12, col='red')
abline(v=2011+1/12, col="blue")
y_range<-seq(180,320, by =20)
x_range <-seq(1, 18, by=1)
x_ticks <-c("11-2009", "12-2009", "01-2010", "02-2010", "03-2010", "04-2011")
plot(avg_by_mo, pch = 22, cex = 1, lty = "solid", lwd = 3, col="deeppink3", yaxt="n", xaxt="n",
      xlab="", ylab="", ylim = c(180,320), xlim=c(1, 18))
axis(2, at=y_range,labels=y_range, col.axis="violetred4", las=2)
axis(1, at=x_range,labels=x_range, col.axis="violetred4", las=1)
pos_vector <- rep(1, length(avg_by_mo))
pos_vector[11]=3
text(avg_by_mo, labels=mo_names, cex= 0.7, pos=pos_vector)
title(main="Monthly Mean RTT in Egypt",
      xlab="Months", ylab="Mean RTT", col.main="deeppink4", font.main = 4, cex.main=1.5)

