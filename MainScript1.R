source("dataConverterFunction.R")
source("ByDayFunction.R")
source("plotterFunction.R")
source("ipAddressLocatorFunction.R")
library(ggplot2);library(dplyr); library(ggmap)
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}
dfList<-list()
dfList[[1]]<-read.csv("Egypt2009NovRTTComplete.csv", colClasses="character")
dfList[[2]]<-read.csv("Egypt2009DecRTTComplete.csv", colClasses="character")
dfList[[3]]<-read.csv("Egypt2010JanRTTComplete.csv", colClasses="character")
dfList[[4]]<-read.csv("Egypt2010FebRTTComplete.csv", colClasses="character")
dfList[[5]]<-read.csv("Egypt2010MarRTTComplete.csv", colClasses="character")
dfList[[6]]<-read.csv("Egypt2010AprRTTComplete.csv", colClasses="character")
dfList[[7]]<-read.csv("Egypt2010MayRTTComplete.csv", colClasses="character")
dfList[[8]]<-read.csv("Egypt2010JuneRTTComplete.csv", colClasses="character")
dfList[[9]]<-read.csv("Egypt2010JulyRTTComplete.csv", colClasses="character")
dfList[[10]]<-read.csv("Egypt2010AugRTTComplete.csv", colClasses="character")
dfList[[11]]<-read.csv("Egypt2010SeptRTTComplete.csv", colClasses="character")
dfList[[12]]<-read.csv("Egypt2010OctRTTComplete.csv", colClasses="character")
dfList[[13]]<-read.csv("Egypt2010NovRTTComplete.csv", colClasses="character")
dfList[[14]] <- read.csv("Egypt2010DecRTTComplete.csv", colClasses="character") 
dfList[[15]] <- read.csv("Egypt2011JanRTTComplete.csv", colClasses="character") 
dfList[[16]] <- read.csv("Egypt2011FebRTTComplete.csv", colClasses="character") 
dfList[[17]]<- read.csv("Egypt2011MarRTTComplete.csv", colClasses="character") 
dfList[[18]] <- read.csv("Egypt2011AprRTTComplete.csv", colClasses="character")
dfList[[19]]<-read.csv("Egypt2011MayRTTComplete.csv", colClasses="character")
dfList[[20]]<-read.csv("Egypt2011JuneRTTComplete.csv", colClasses="character")
dfList[[21]]<-read.csv("Egypt2011JulyRTTComplete.csv", colClasses="character")
dfList[[22]]<-read.csv("Egypt2011AugRTTComplete.csv", colClasses="character")
dfList[[23]]<-read.csv("Egypt2011SeptRTTComplete.csv", colClasses="character")
workingDFList <- list()
for (i in (1:23)){
        workingDFList[[i]]<-  dataConverter(dfList[[i]])
}
dfCount <- length(dfList)

#Creating aggregate data frame
aggregateDF <- data.frame()

for (i in (1:dfCount)){
        aggregateDF <-rbind(aggregateDF, workingDFList[[i]])
}
aggregateDF$month <- format(aggregateDF$date, format="%b")
aggregateDF$year <- as.POSIXlt(aggregateDF$date)$year + 1900
aggregateDF$year_month <- paste(aggregateDF$year, aggregateDF$month)
ggplot(aggregateDF) + geom_boxplot(aes(x=reorder(year_month, logTime), y=RTT, fill=logTime)) +
        coord_trans(y = "log10")+
        coord_cartesian(ylim = quantile(aggregateDF$RTT, c(0.1, 0.9)))

        #scale_y_continuous(limits = quantile(aggregateDF$RTT, c(0.1, 0.9)))+
        #ylim1 = boxplot.stats(aggregateDF$RTT)$stats[c(1, 5)]
#p1 = p0 + coord_cartesian(ylim = ylim1*1.05)

#finding monthly averages and medians
avg_by_mo <-rep(0,dfCount)

for (i in (1:dfCount)){
        avg_by_mo[i] <- mean(workingDFList[[i]]$RTT) 
        
}
med_by_mo <-rep(0,dfCount)

for (i in (1:dfCount)){
        med_by_mo[i] <- median(workingDFList[[i]]$RTT) 
        
}

#Graphing monthly average and mean
mo_names <- c("Nov2009", "Dec2009", "Jan2010", "Feb2010", "Mar2010", "Apr2010",
              "May2010", "June2010", "July2010", "Aug2010", "Sept2010", "Oct2010",
              "Nov2010", "Dec2010", "Jan2011", "Feb2011", "Mar2011", "Apr2011", 
              "May2011", "June2011", "July2011", "Aug2011", "Sept2011")
y_range<-seq(180,300, by =20)
x_range <-seq(1, dfCount, by=1)

plot(avg_by_mo, pch = 22, cex = 1, lty = "solid", lwd = 3, col="deeppink3", yaxt="n", xaxt="n",
     xlab="", ylab="", ylim = c(180,302), xlim=c(1, dfCount))
lines(med_by_mo, pch = 22, cex = 1, lty = "solid", lwd = 3, col="darkgreen", yaxt="n", xaxt="n",
     xlab="", ylab="", ylim = c(180,302), xlim=c(1, dfCount))
axis(2, at=y_range,labels=y_range, col.axis="violetred4", las=2)
axis(1, at=x_range,labels=x_range, col.axis="violetred4", las=1)
pos_vector <- rep(1, length(avg_by_mo))
pos_vector[11]=3
text(avg_by_mo, labels=mo_names, cex= 0.7, pos=pos_vector)
title(main="Monthly Mean RTT in Egypt",
      xlab="Months", ylab="Mean RTT", col.main="deeppink4", font.main = 4, cex.main=1.5)

#finding 5 number summary and the top outlier boundary for each month 
#and saving the top outlier to data frames stored in a list
#fiveNumbersList<-list()
topOutlierBndry <-rep(0,dfCount)
outlierDFList<-list()
normalDFList <- list()
for (i in (1:dfCount)){
        x<-fivenum(workingDFList[[i]]$RTT)
        iqr<-IQR(workingDFList[[i]]$RTT)
        topOutlierBndry[i]<- x[4] + 1.5*iqr
        the_condition <- workingDFList[[i]]$RTT >= topOutlierBndry[i]
        outlierDFList[[i]]<- workingDFList[[i]][the_condition, ]
        normalDFList[[i]]<-workingDFList[[i]][-the_condition, ]
        
        normalDFList[[i]]<-subset(normalDFList[[i]], select = - c(date))
        outlierDFList[[i]]<-subset(outlierDFList[[i]], select = - c(date))
}

#grouping the outlier data frames by serverIP and summarizing the count, mean and median
# of each group
byServerIPSummaryList<-list()
byServerNormalIPSummaryList <- list()
for (i in (1:dfCount)){
        x <- group_by(outlierDFList[[i]], serverIP)
        byServerIPSummaryList[[i]]<- data.frame(summarise(x, count=n(), meanRTT = mean(RTT),
                                               medianRTT = median(RTT)))
        y <- group_by(normalDFList[[i]], serverIP)
        byServerNormalIPSummaryList[[i]]<-data.frame(summarise(y, count=n(),
                                meanRTT=mean(RTT), medianRTT=median(RTT)))
}

#ip address locator function from 
#https://heuristically.wordpress.com/2013/05/20/geolocate-ip-addresses-in-r/
freegeoip <- function(ip, format = ifelse(length(ip)==1,'list','dataframe'))
{
        if (1 == length(ip))
        {
                # a single IP address
                require(rjson)
                url <- paste(c("http://freegeoip.net/json/", ip), collapse='')
                ret <- fromJSON(readLines(url, warn=FALSE))
                if (format == 'dataframe')
                        ret <- data.frame(t(unlist(ret)))
                return(ret)
        } else {
                ret <- data.frame()
                for (i in 1:length(ip))
                {
                        r <- freegeoip(ip[i], format="dataframe")
                        ret <- rbind(ret, r)
                }
                return(ret)
        }
}   

##Using the address locator function on the distinct serverIP's from 
##
outlierServerIPLocationList <- list()
normalServerIPLocationList <- list()
for (i in (1:dfCount)){
        x <- unique(byServerIPSummaryList[[i]]$serverIP)
        outlierServerIPLocationList[[i]]<- freegeoip(x)

        
}
for (i in (1:dfCount)){
        y <- unique(byServerNormalIPSummaryList[[i]]$serverIP)
        normalServerIPLocationList[[i]]<-freegeoip(y)
        
}

      
aggregateOutlierServerLocationDF <- data.frame()
aggregateNormalServerLocationDF <- data.frame()
for (i in (1:dfCount)){
        aggregateOutlierServerLocationDF <- rbind(aggregateOutlierServerLocationDF,
                                                 outlierServerIPLocationList[[i]] )
        aggregateNormalServerLocationDF <- rbind(aggregateNormalServerLocationDF, 
                                                 normalServerIPLocationList[[i]])
}
aggregateOutlierServerLocationDF$longLat <- paste(aggregateOutlierServerLocationDF$longitude,
                                                  aggregateOutlierServerLocationDF$latitude)
aggregateNormalServerLocationDF$longLat <- paste(aggregateNormalServerLocationDF$longitude,
                                                 aggregateNormalServerLocationDF$latitude)
temp <- group_by(aggregateOutlierServerLocationDF, longitude, latitude)
byLongLatOutlierServerDF <- data.frame(summarise(temp, count=n()))
temp <- group_by(aggregateNormalServerLocationDF, longitude, latitude)
byLongLatNormalServerDF <- data.frame(summarise(temp, count=n()))

############
#centering map on Europe
B_lat<- 47.5172007  #Budapest latitude
B_lon<- 19.07226563  #Budapest longitude
Eu_lat <- 48.69096039 #generic Europe latitude
Eu_lon <- 9.66796875  #generic Europe longitude
googMap <- get_googlemap(center = c(lon = B_lon , lat = B_lat ), zoom =4, 
                         maptype='hybrid', scale=2)

#finding unique longitudes and latitudes  in Europe
outlierEuropeCondition <- as.numeric.factor(byLongLatOutlierServerDF$longitude) <= 30 &
        as.numeric.factor(byLongLatOutlierServerDF$longitude)> -50
normalEuropeCondition <- as.numeric.factor(byLongLatNormalServerDF$longitude) <= 30 &
        as.numeric.factor(byLongLatNormalServerDF$longitude)> -50

EuropeOutlierDF <- byLongLatOutlierServerDF[outlierEuropeCondition,]

EuropeNormalDF <- byLongLatNormalServerDF [normalEuropeCondition,]

#plotting servers form Europe
ggmap(googMap)+geom_point(
        aes(x =as.numeric.factor(longitude), y =as.numeric.factor(latitude),
            size = count),pch=8, data=EuropeOutlierDF , colour= "red")

ggmap(googMap) +geom_point(
        aes(x =as.numeric.factor(longitude), y =as.numeric.factor(latitude),
            size = count),pch=8, data=EuropeNormalDF , colour= "green")

calc_zoom(as.numeric.factor(longitude), as.numeric.factor(latitude), data=Eur_df,  f = 0.05)

                
#centering map on US
googMapUS <- get_googlemap(center=c(lon=-100.1953125, lat=37.16031655), zoom=4,
                           maptype="hybrid", scale=2)

#finding unique longitudes and latitudes in Us
USOutlierDF <- byLongLatOutlierServerDF[!outlierEuropeCondition,]
USNormalDF <- byLongLatNormalServerDF [!normalEuropeCondition,]

#plotting server locations from the US
ggmap(googMapUS)+geom_point(
        aes(x =as.numeric.factor(longitude), y =as.numeric.factor(latitude), size=count), 
        pch=8, data=USOutlierDF , colour="red")

ggmap(googMapUS) +geom_point(
        aes(x =as.numeric.factor(longitude), y =as.numeric.factor(latitude),
            size = count),pch=8, data=USNormalDF , colour= "green")