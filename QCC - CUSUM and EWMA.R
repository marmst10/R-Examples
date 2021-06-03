# STAT 8110 HW3

library(tidyverse)
library(IQCC)
library(ggplot2)

table9e1 <- readxl::read_xlsx(
  "C:/Users/conno/OneDrive/Desktop/STAT 8110 - Quality Control and Process Improvement/Data/Table 9E1.xlsx")

## Problem 9.1

## Plot CUSUM Using QCC ##

cusum_plot <- qcc::cusum(table9e1$x,sizes=1,center=1050,std.dev=25,decision.interval = 5,
                         head.start = 0.50,plot=T)

## Determine Which Points Went OOC ##

cusum_plot$violations$lower
cusum_plot$violations$upper

sd(table9e1$x)

## Problem 9.2

table9e2 <- readxl::read_xlsx(
  "C:/Users/conno/OneDrive/Desktop/STAT 8110 - Quality Control and Process Improvement/Data/Table 9E2.xlsx")

## Plot CUSUM Using QCC ##

cusum2 <- qcc::cusum(table9e2$x,sizes=1,center=8.02,std.dev=0.05,decision.interval = 4.77,
                         head.start = 0.50,plot=T)

## Determine Which Points Went OOC ##

cusum2$violations$lower
cusum2$violations$upper

sd(table9e2$x)

## Problem 9.25

## Create EWMA Control Chart ##

ewma1 <- qcc::ewma(table9e1$x,
                        sizes=1,
                        center=1050,
                        std.dev=25,
                        lambda=0.10,
                        nsigmas=2.7,
                        plot=T)

## Determining which points are OOC ##

ewma1$violations

## Problem 9.27

## Create EWMA Control Chart ##

ewma2 <- qcc::ewma(table9e2$x,
                   sizes=1,
                   center=8.02,
                   std.dev=0.05,
                   lambda=0.20,
                   nsigmas=3,
                   plot=T)

## Determining which points are OOC ##

ewma2$violations

## PROBLEM 11.1
table11e1 <- readxl::read_xlsx(
  "C:/Users/conno/OneDrive/Desktop/STAT 8110 - Quality Control and Process Improvement/Data/Table 11E1.xlsx")

## inputting the mean values##
mean_vector <- matrix(c(55, 30),ncol=2)
mean_vector
#1x2 matrix

## inputting the V-C matrix##
vc_matrix <- matrix(c(200, 130, 130, 120),nrow=2,ncol=2)
vc_matrix
#2x2 matrix

## Obtain T^2 Values ##
xbarz <- t(as.matrix(table11e1[,-1]))
#2x15 matrix

T2 <- c()

for(i in 1:15){
  T2[i] <- 25*(xbarz[,i]-mean_vector) %*% solve(vc_matrix) %*% t(xbarz[,i] - mean_vector)
}


UCL <- (2*(50+1)*(25-1))/(50*25-50-2+1)*qf(0.001,2,50*25-50-2+1,lower.tail=F)



ggplot() + geom_point(aes(x=seq(1,15),y=T2),color="red") +
  geom_line(aes(x=seq(1,15),y=T2),color="red") +
  geom_line(aes(x=seq(1,15),y=UCL),color="black") + theme_classic()

## PROBLEM 11.2
table11e2 <- readxl::read_xlsx(
  "C:/Users/conno/OneDrive/Desktop/STAT 8110 - Quality Control and Process Improvement/Data/Table 11E2.xlsx")

## inputting the mean values##
mean_vector <- matrix(c(3.0,3.5,2.8),ncol=3)
mean_vector

## inputting the V-C matrix##
vc_matrix <- matrix(c(1.4,1.02,1.05,1.02,1.35,.98,1.05,.98,1.2),nrow=3,ncol=3)
vc_matrix

## Obtain T^2 Values ##
xbarz <- t(as.matrix(table11e2[,-1]))

T2 <- c()

for(i in 1:15){
  T2[i] <- 10*(xbarz[,i]-mean_vector) %*% solve(vc_matrix) %*% t(xbarz[,i] - mean_vector)
}

UCL <- (3*(30-1)*(10-1))/(30*10-30-3+1)*qf(0.001,3,30*10-50-3+1,lower.tail=F)

ggplot() + geom_point(aes(x=seq(1,15),y=T2),color="red") +
  geom_line(aes(x=seq(1,15),y=T2),color="red") +
  geom_line(aes(x=seq(1,15),y=UCL),color="black") + theme_classic()
