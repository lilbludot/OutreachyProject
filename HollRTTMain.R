source("dataConverterFunction.R")
source("ByDayFunction.R")
source("plotterFunction.R")
library(ggplot2);library(dplyr); library(stringr)

#########NETHERLANDS######################
file_list = list.files(pattern="Holland")

monthCount <- length(file_list)
dutchDFList <- list()

for (i in file_list){
        x <- substring(i, 1, nchar(i)-15)
        dummy<- read.csv(i, colClasses="character")
        dutchDFList[[x]] <- dataConverter(dummy) 
}
months <- list("Jan"=1, "Feb"=2, "Mar"=3, "Apr"=4, "May"=5, "June"=6, "July"=7,
               "Aug"=8, "Sept"=9, "Oct"=10, "Nov"=11, Dec=12)
namesToNumbers <- list()
numbersToNames <- list()
numbers <- c()
for (i in file_list){
        x <- substring(i, 1, nchar(i)-15)
        print(x)
        year <- substring(x,8,11 )
        mo <- substring(x, 12, nchar(x))
        month <- months[[mo]]
        number <- as.integer(year)*100+as.integer(month)
        print(number)
        namesToNumbers[[x]]=number
        
        numbersToNames[[as.character(number)]]=x
        print(numbersToNames[[as.character(number)]])
        numbers <- c(numbers, number)     
}

numbers<-sort(numbers)

#finding monthly averages and medians
avg_by_mo <-rep(0,monthCount-2)
counter<-0

for (i in numbers[3:length(numbers)]){
        counter<-counter+1
        dummy<-numbersToNames[[as.character(i)]]
        avg_by_mo[counter] <- mean(dutchDFList[[dummy]]$RTT) 
        
}
med_by_mo <-rep(0,monthCount-2)
counter<-0
for (i in numbers[3:length(numbers)]){
        counter<-counter+1
        dummy<-numbersToNames[[as.character(i)]]
        med_by_mo[counter] <- median(dutchDFList[[dummy]]$RTT) 
        
}

####################EGYPT##############################
file_list_Egypt <- list.files(pattern="Egypt.*RTTComplete.*csv")
monthCount_Egypt <- length(file_list_Egypt)
egyptDFList <- list()
for (i in file_list_Egypt){
        x <- substring(i, 1, nchar(i)-15)
        dummy<- read.csv(i, colClasses="character")
        egyptDFList[[x]] <- dataConverter(dummy) 
}

namesToNumbers_Egypt <- list()
numbersToNames_Egypt <- list()
numbers_Egypt <- c()
for (i in file_list_Egypt){
        x <- substring(i, 1, nchar(i)-15)
        year <- substring(x,6,9)
        mo <- substring(x, 10, nchar(x))
        month <- months[[mo]]
        number <- as.integer(year)*100+as.integer(month)
        print(number)
        namesToNumbers_Egypt[[x]]=number
        numbersToNames_Egypt[[number]]=x
        numbers_Egypt <- c(numbers_Egypt, number)     
}

numbers_Egypt<-sort(numbers_Egypt)

#finding monthly averages and medians
avg_by_mo_Egypt <-rep(0,monthCount_Egypt)
counter<-0

for (i in numbers_Egypt){
        counter<-counter+1
        dummy<-numbersToNames_Egypt[[i]]
        avg_by_mo_Egypt[counter] <- mean(egyptDFList[[dummy]]$RTT) 
        
}
med_by_mo_Egypt <-rep(0,monthCount_Egypt)
counter<-0
for (i in numbers_Egypt){
        counter<-counter+1
        dummy<-numbersToNames_Egypt[[i]]
        med_by_mo_Egypt[counter] <- median(egyptDFList[[dummy]]$RTT) 
        
}
mo_names <- c("Nov2009", "Dec2009", "Jan2010", "Feb2010", "Mar2010", "Apr2010",
              "May2010", "June2010", "July2010", "Aug2010", "Sept2010", "Oct2010",
              "Nov2010", "Dec2010", "Jan2011", "Feb2011", "Mar2011", "Apr2011", 
              "May2011", "June2011", "July2011", "Aug2011", "Sept2011")
half_mo_names <- c("Nov2009", "", "Jan2010", "", "Mar2010","",
                   "May2010", "", "July2010","", "Sept2010", "",
                   "Nov2010", "", "Jan2011", "", "Mar2011", "",
                   "May2011", "", "July2011", "", "Sept2011")
month_labels <- c("11/09","12/09", "1/10","2/10", "3/10", "4/10", "5/10", "6/10",
                  "7/10", "8/10", "9/10", "10/10", "11/10", "12/10", "1/11", "2/11",
                  "3/11", "4/11", "5/11", "6/11", "7/11", "8/11", "9/11" )
x_ticks <- rep(0, monthCount_Egypt)
y_range<-seq(-4,300, by =20)
x_range <-seq(1, monthCount_Egypt, by=1)
xx_cable <- c(5,6,6,5)
yy_cable <- c(avg_by_mo_Egypt[5], avg_by_mo_Egypt[6],0,0)
xx_revolt <- c(15,16,17,17,16,15)
yy_revolt <- c(avg_by_mo_Egypt[15],avg_by_mo_Egypt[16], avg_by_mo_Egypt[17], 0, 0, 0 )
#yy_cable <- c(302,302,0,0)
color1 <- c(255, 218, 155)
color1_transparent <- adjustcolor(color1, alpha.f = 0.5) 
color2 <- c(176, 224, 230)
color2_transparent <- adjustcolor(color2, alpha.f = 0.5) 
plot(avg_by_mo_Egypt, pch = 22, cex = 1, lty = "solid", lwd = 3, col="deeppink3", yaxt="n", xaxt="n",
     xlab="", ylab="", ylim = c(-4,302), xlim=c(1, monthCount_Egypt+1), frame=FALSE)

points(avg_by_mo, pch = 22, cex = 1, lty = "solid", lwd = 3, col="darkorange", yaxt="n", xaxt="n",
     xlab="", ylab="")
points(x_ticks, pch=3, yaxt="n", xaxt="n", xlab="", ylab="")
#points(5, 198, pch = "-",  yaxt="n", xaxt="n", xlab="", ylab="", col="violetred4")
#points(5, 150, pch = "-",  yaxt="n", xaxt="n", xlab="", ylab="", col="violetred4")
#points(15, 188, pch = "-",  yaxt="n", xaxt="n", xlab="", ylab="", col="violetred4")
#points(15, 136, pch = "-",  yaxt="n", xaxt="n", xlab="", ylab="", col="violetred4")
#points(16, 160, pch = 20,  yaxt="n", xaxt="n", xlab="", ylab="", col="transparent")
polygon(xx_cable,yy_cable,col=color2_transparent, border=color2_transparent)
polygon(xx_revolt, yy_revolt, col=color2_transparent, border=color2_transparent)
abline(h=0)
#polygon(c(0,24,24,0),c(302,302,0,0),col="transparent")
axis(2, at=y_range,labels=y_range, col.axis="violetred4", las=2, cex.axis=0.6)
#axis(1, at=x_range,labels=mo_names, col.axis="violetred4", las=1)
pos_vector <- rep(1, 12)
#pos_vector[11]=3
text(x_ticks, labels=half_mo_names, cex= 0.6, pos=pos_vector, col="violetred4")
text(5, 198, labels="- 24 March 2010: \n  SEACOM* experiences problems", pos=c(4), cex=0.7,
     col="violetred4")
text(5, 150, labels="- 14 April 2010: \n  SEA-ME-WE-4 undersea cable is cut",
     pos=c(4), cex=0.7, col="violetred4")
text(15, 188, labels="- January & February 2011: \n  Beginnings of the Arab Spring ",
     pos=c(4), cex=0.7,col="violetred4")
text(15, 136, labels="- 12 February 2011: \n  Mubarak cedes power", pos=c(4), cex=0.7,
     col="violetred4")
title(main="Monthly Mean RTT in Egypt and the Netherlands \n with Highlighted Time Periods of Interest",
      ,xlab="", ylab="Mean RTT", col.main="violetred4", font.main = 4, cex.main=1.2, 
      col.lab = "violetred4")

legend("topright",c("Egypt", "Netherlands"), col =c("deeppink3", "darkorange"), 
       pch = c(22,22), bty="n", text.col = c("deeppink3", "darkorange"), cex=1)
mtext("*SEACOM is the owner and operator of a network of 
      undersea and land high-speed fiber-optic cables in Africa", side=1,
      line=2, cex=0.6, adj=0)     

#################PLOTS####################################################
y_range<-seq(0,160, by =20)
x_range <-seq(1, monthCount-2, by=1)

plot(avg_by_mo, pch = 22, cex = 1, lty = "solid", lwd = 3, col="deeppink3", yaxt="n", xaxt="n",
     xlab="", ylab="", ylim = c(0,160), xlim=c(1, monthCount-2))
lines(med_by_mo, pch = 22, cex = 1, lty = "solid", lwd = 3, col="darkgreen", yaxt="n", xaxt="n",
      xlab="", ylab="", ylim = c(0,160), xlim=c(1, monthCount-2))
axis(2, at=y_range,labels=y_range, col.axis="violetred4", las=2)
axis(1, at=x_range,labels=x_range, col.axis="violetred4", las=1)
