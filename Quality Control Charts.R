#STAT 8110 HW1
library(tidyverse)
library(readxl)
library(qcc)

q6dot11 <- readxl::read_xlsx("C:/Users/conno/OneDrive/Desktop/STAT 8110 - Quality Control and Process Improvement/Homework/Homework 1/q6dot11.xlsx")
#q6dot11
## x-bar Chart ##
qcc(q6dot11[,-1],type="xbar",plot=TRUE)
## S Chart ##
qcc(q6dot11[,-1],type="S",plot=TRUE)
## R Chart ##
qcc(q6dot11[,-1],type="R",plot=TRUE)

ssquared.p1 <- function(dat,alpha){
  
  sbar2 <- mean(apply(dat,1,var))#the 1 means rows#
  UCL <- sbar2*qchisq(alpha/2,df=ncol(dat)-1,lower.tail=F)/(ncol(dat)-1)
  LCL <- sbar2*qchisq(alpha/2,df=ncol(dat)-1,lower.tail=T)/(ncol(dat)-1)
  s2 <- apply(dat,1,var)
  
  p <- ggplot() + geom_line(aes(x=seq(1,nrow(dat),by=1),y=UCL),color="red") +
    geom_line(aes(x=seq(1,nrow(dat),by=1),y=LCL),color="red") +
    geom_line(aes(x=seq(1,nrow(dat),by=1),y=s2),color="black") +
    geom_point(aes(x=seq(1,nrow(dat),by=1),y=s2),color="black") +
    labs(x="Time Points",y="Value of Measurement") +
    theme_classic() +
    ggtitle("S-Squared Control Chart") +
    theme(plot.title=element_text(hjust=0.5))
  
  print(p)
  
}

library(ggplot2)

ssquared.p1(q6dot11[,-1],alpha=0.0027)#ALPHA 0.0027 IS SIX SIGMA

q6dot22 <- readxl::read_xlsx("C:/Users/conno/OneDrive/Desktop/STAT 8110 - Quality Control and Process Improvement/Homework/Homework 1/q6dot22.xlsx")
## x-bar Chart ##
qcc(q6dot22[,-1],type="xbar",plot=TRUE)
## S Chart ##
qcc(q6dot22[,-1],type="S",plot=TRUE)

q6dot22b <- readxl::read_xlsx("C:/Users/conno/OneDrive/Desktop/STAT 8110 - Quality Control and Process Improvement/Homework/Homework 1/q6dot22b.xlsx")
## x-bar Chart ##
qcc(q6dot22b[,-1],type="xbar",plot=TRUE)
## S Chart ##
qcc(q6dot22b[,-1],type="S",plot=TRUE)

## 6.22 for 6.21.a R Chart ##
qcc(q6dot22[,-1],type="R",plot=TRUE)
## 6.22 for 6.21.b R Chart ##
qcc(q6dot22b[,-1],type="R",plot=TRUE)

q6dot34 <- readxl::read_xlsx("C:/Users/conno/OneDrive/Desktop/STAT 8110 - Quality Control and Process Improvement/Homework/Homework 1/q6dot34.xlsx")
## x-bar Chart ##
qcc(q6dot34[,-1],type="xbar",plot=TRUE)
## R Chart ##
qcc(q6dot34[,-1],type="R",plot=TRUE)

#removing OOC observations

#pulling out xbar control limits
q6dot34qcc <- qcc(q6dot34[,-1],type="xbar")

#new dataframe in case this messes up
q6dot34a2 <- q6dot34

#create mean column
q6dot34a2$Mean <- rowMeans(q6dot34a2[,2:5])

#filter by control limits
q6dot34a3 <- q6dot34a2 %>%
  dplyr::filter((Mean > q6dot34qcc$limits[1])&(Mean < q6dot34qcc$limits[2]))

## new x-bar Chart ##
qcc(q6dot34a3[,-1],type="xbar",plot=TRUE)
## new R Chart ##
qcc(q6dot34a3[,-1],type="R",plot=TRUE)


install.packages('nortest')
library(nortest)
##AD Normality Test
ad.test(c(q6dot34$x1,q6dot34$x2,q6dot34$x3,q6dot34$x4))

qqnorm(c(q6dot34$x1,q6dot34$x2,q6dot34$x3,q6dot34$x4), plot.it=TRUE, pch = 1)
qqline(c(q6dot34$x1,q6dot34$x2,q6dot34$x3,q6dot34$x4), col = "steelblue", lwd = 2)


##Calculating FNC
## Saving Phase I Information ##

p1 <- qcc(flow[,-1],type="xbar",plot=FALSE)

##6.41
1-pnorm(2.064,mean=0,sd=1)
