#STAT 8230 FINAL PROJECT
#CONNOR ARMSTRONG
math <- read.csv("C:/Users/conno/OneDrive/Desktop/STAT 8230 - Applied Multivariate Data Analysis/Final Project/math_test.csv")
summary(math)

unique(math$Grade)
math_2 <- subset(math, Grade!="All Grades")
unique(math_2$Grade)

summary(math_2)

num <- math_2[,c("Num.Level.1","Num.Level.2","Num.Level.3","Num.Level.4")]
pct <- math_2[,c("Pct.Level.1","Pct.Level.2","Pct.Level.3","Pct.Level.4")]

pairs(num)

pairs(pct)

#import python clustering results
ward <- read.csv("C:/Users/conno/OneDrive/Desktop/STAT 8230 - Applied Multivariate Data Analysis/Final Project/ward.csv")
complete <- read.csv("C:/Users/conno/OneDrive/Desktop/STAT 8230 - Applied Multivariate Data Analysis/Final Project/complete.csv")
average <- read.csv("C:/Users/conno/OneDrive/Desktop/STAT 8230 - Applied Multivariate Data Analysis/Final Project/average.csv")
math_2$ward <- ward$X0
math_2$complete <- complete$X0
math_2$average <- average$X0

#compare clusterings

#ward and complete have a lot of similarity 0=0 and 1=1
as.data.frame(table(math_2$ward,math_2$complete))
#1's are Not SWD and 0's are SWD

head(average)
#need to reverse average 
math_2$average <- 1 - average$X0
math_2$average

#initialize colors
aimcol <- character(nrow(math_2))
aimcol[] <- "red"
aimcol[math_2$Demographic == "SWD"] <- "black"
wardcol <- character(nrow(math_2))
wardcol[] <- "red"
wardcol[math_2$ward == 0] <- "black"
completecol <- character(nrow(math_2))
completecol[] <- "red"
completecol[math_2$complete == 0] <- "black"
averagecol <- character(nrow(math_2))
averagecol[] <- "red"
averagecol[math_2$average == 0] <- "black"


pairs(num,col=aimcol,pch = 20, cex=0.25)
pairs(num,col=wardcol,pch = 20, cex=0.25)
pairs(num,col=completecol,pch = 20, cex=0.25)
pairs(num,col=averagecol,pch = 20, cex=0.25)

summary(num)


SWD <- math_2 %>% dplyr::filter(Demographic == "SWD")

summary(SWD)


SWOD <- math_2 %>% dplyr::filter(Demographic == "Not SWD")

summary(SWOD)

#hotellings t^2 for identifying the differences between SWD and SWOD
hot <- num
hot$swd <- as.factor(math_2$Demographic)

SWDnum <- SWD[,c("Num.Level.1","Num.Level.2","Num.Level.3","Num.Level.4")]
SWODnum <- SWOD[,c("Num.Level.1","Num.Level.2","Num.Level.3","Num.Level.4")]

install.packages("ICSNP")
library(ICSNP)
HotellingsT2(SWDnum,SWODnum)

as.data.frame(table(math_2$average, math_2$Demographic))
as.data.frame(table(math_2$complete, math_2$Demographic))
as.data.frame(table(math_2$ward, math_2$Demographic))
