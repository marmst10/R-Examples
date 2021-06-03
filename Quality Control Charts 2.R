#STAT 8110 HW2 R CODE

#7.3a
Q73 <- readxl::read_xlsx(
  "C:/Users/conno/OneDrive/Desktop/STAT 8110 - Quality Control and Process Improvement/Homework/Homework 2/7 1.xlsx")

#P chart
Q73_chart <- qcc::qcc(Q73$`Number Nonconforming`,
                         type="p",sizes=50,
                         plot=TRUE)
#7.3b
ooc_points <- Q73_chart$violations$beyond.limits

Q73_1 <- Q73[-ooc_points,]

Q73_chart1 <- qcc::qcc(Q73_1$`Number Nonconforming`,
                       type="p",sizes=50,
                       plot=TRUE)

#7.8
Q78_chart <- qcc::qcc(Q73$`Number Nonconforming`,
                      type="np",sizes=50,
                      plot=TRUE)

Q78_chart1 <- qcc::qcc(Q73_1$`Number Nonconforming`,
                       type="np",sizes=50,
                       plot=TRUE)

#7.49
Q749 <- readxl::read_xlsx(
  "C:/Users/conno/OneDrive/Desktop/STAT 8110 - Quality Control and Process Improvement/Homework/Homework 2/7 13.xlsx")
#P chart
Q749_chart <- qcc::qcc(Q749$`Total Number of Imperfections`,
                      type="p",sizes=Q749$`Number of Rolls Produced`,
                      plot=TRUE)

#7.50
#calculating average sample size
Q750avg <- mean(Q749$`Number of Rolls Produced`)
#u chart with size set to average
Q750_chart <- qcc::qcc(Q749$`Total Number of Imperfections`,
                       type="u",sizes=Q750avg,
                       plot=TRUE)

#7.64
Q764 <- readxl::read_xlsx(
  "C:/Users/conno/OneDrive/Desktop/STAT 8110 - Quality Control and Process Improvement/Homework/Homework 2/7 18.xlsx")

Q764_chart <- qcc::qcc(Q764$`Number of Posting Errors`,
                          type="c")


alpha <- 0.0027

UCL <- qpois(alpha/2,lambda=Q764_chart$center,lower.tail=FALSE)
LCL <- qpois(alpha/2,lambda=Q764_chart$center,lower.tail=TRUE)

## Compare to 3 Sigma Limits ##

c(UCL,Q764_chart$limits[2])

c(LCL,Q764_chart$limits[1])

#check whether data follows a poisson distribution
y=rpois(1000000,Q764_chart$center)
n=length(y)
(x=table(y))
?rpois
k=as.numeric(names(x))
plot(k,log(x)+lfactorial(k))
lines(k,log(k)+lfactorial(k), col="red",lty=2)

n=length(Q764$`Number of Posting Errors`)
x=table(Q764$`Number of Posting Errors`)
k=as.numeric(names(x))
plot(k,log(x)+lfactorial(k))
plot(k, log(X)+lfactorial(k), col="red")

set.seed(12111978)
vec <- rpois(1000, Q764_chart$center)
y <- 0:max(vec)
x <- dpois(0:max(vec), mean(vec))
lo <- loess(y~x)
lines(predict(lo), col='red', lwd=2)
hist(Q764$`Number of Posting Errors`, prob=TRUE, ylim = c(0, .7)) # may need to tweak the y axis.
lines(0:max(vec), dpois(0:max(vec), mean(vec)), col = 'red')

x <- 1:10
y <- c(2,4,6,8,7,12,14,16,18,20)
lo <- loess(y~x)
plot(x,y)
lines(predict(lo), col='red', lwd=2)
lines(qplot(x,y, geom='smooth', span =0.5), col='red', lwd=2)
smoothingSpline = smooth.spline(x, y, spar=0.23)
plot(x,y)
lines(smoothingSpline)

scatter.smooth(x, y)
library(ggplot2)
qplot(x,y, geom='smooth', span =0.5)
q764x <- 1:6
(q764x2 = table(6))
ggplot(Q764$`Number of Posting Errors`, aes(x=q764x)) + geom_histogram()
