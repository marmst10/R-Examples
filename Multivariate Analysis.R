# STAT 8230 HW1

# Question 1
A = matrix(
  c(0.801 , 121.41 , 70.42,
    0.824 , 127.70 , 72.47,
    0.841 , 129.20 , 78.20,
    0.816 , 131.80 , 74.89,
    0.840 , 135.10 , 71.21,
    0.842 , 131.50 , 78.39,
    0.820 , 126.70 , 69.02,
    0.802 , 115.10 , 73.10,
    0.828 , 130.80 , 79.28,
    0.819 , 124.60 , 76.48,
    0.826 , 118.31 , 70.25,
    0.802 , 114.20 , 72.88,
    0.810 , 120.30 , 68.23,
    0.802 , 115.70 , 68.12,
    0.832 , 117.51 , 71.62,
    0.796 , 109.81 , 53.10,
    0.759 , 109.10 , 50.85,
    0.770 , 115.10 , 51.68,
    0.759 , 118.31 , 50.60,
    0.772 , 112.60 , 53.51,
    0.806 , 116.20 , 56.53,
    0.803 , 118.00 , 70.70,
    0.845 , 131.00 , 74.35,
    0.822 , 125.70 , 68.29,
    0.971 , 126.10 , 72.10,
    0.816 , 125.80 , 70.64,
    0.836 , 125.50 , 76.33,
    0.815 , 127.80 , 76.75,
    0.822 , 130.50 , 80.33,
    0.822 , 127.90 , 75.68,
    0.843 , 123.90 , 78.54,
    0.824 , 124.10 , 71.91,
    0.788 , 120.80 , 68.22,
    0.782 , 107.40 , 54.42,
    0.795 , 120.70 , 70.41,
    0.805 , 121.91 , 73.68,
    0.836 , 122.31 , 74.93,
    0.788 , 110.60 , 53.52,
    0.772 , 103.51 , 48.93,
    0.776 , 110.71 , 53.67,
    0.758 , 113.80 , 52.42), 
  nrow = 41, byrow = T)

# Question 1.a
# Report summary statistics (means and variances) of the three variables 
# and present a scatter plot matrix.

# summary statistics of all 3 variables
summary(A)

# mean and variance of x1
mean(A[,1])
var(A[,1])

# mean and variance of x2
mean(A[,2])
var(A[,2])

# mean and variance of x3
mean(A[,3])
var(A[,3])

# scatterplot matrix
pairs(A)

# Question 1.b
# Identify an outlier.

# Using method described in:
# https://www.r-bloggers.com/2019/01/a-new-way-to-handle-multivariate-outliers/

library(psych)

#outlier function from psych package plots QQ plot for outliers using 
# Mahalanobis Distance (MD)
outlier(A)


md <- mahalanobis(A, center = colMeans(A), cov = cov(A))
alpha <- .001
cutoff <- (qchisq(p = 1 - alpha, df = ncol(A)))
names_outliers_MH <- which(md > cutoff)
excluded_mh <- names_outliers_MH

# print MD for all observations
md

# print observation which flagged as outlier - observation 25
excluded_mh

# observation 25 detected as outlier with >99.9% confidence with alpha = .001


# Question 2
# Consider a bivariate normal distribution for X = (X1; X2)', with E(X1) = 3, E(X2) = 5, 
# Var(X1) = 10, Var(X2) = 4, Corr(X1; X2) = 0.9.

# From http://www.stat.ucl.ac.be/ISpersonnel/lecoutre/stats/fichiers/_gallery.pdf
# 3-D plots
mu1<-3 # setting the expected value of x1
mu2<-5 # setting the expected value of x2
s11<-10 # setting the variance of x1
s12<-36 # setting the covariance between x1 and x2
s22<-4 # setting the variance of x2
rho<-0.9 # setting the correlation coefficient between x1 and x2
x1<-seq(-7,13,length=41) # generating the vector series x1
x2<-seq(-1,11,length=41) # generating the vector series x2
f<-function(x1,x2)
{
  term1<-1/(2*pi*sqrt(s11*s22*(1-rho^2)))
  term2<--1/(2*(1-rho^2))
  term3<-(x1-mu1)^2/s11
  term4<-(x2-mu2)^2/s22
  term5<--2*rho*((x1-mu1)*(x2-mu2))/(sqrt(s11)*sqrt(s22))
  term1*exp(term2*(term3+term4-term5))
} # setting up the function of the multivariate normal density
z<-outer(x1,x2,f) # calculating the density values
# Question 2.a
# Sketch a contour of the density.
persp(x1, x2, z,
      main="Two dimensional Normal Distribution",
      sub=expression(italic(f)~(bold(x))==frac(1,2~pi~sqrt(sigma[11]~sigma[22]
        ~(1-rho^2)))~phantom(0)^bold(.)~exp~bgroup("{", list(-frac(1,2(1-rho^2)),
        bgroup("[", frac((x[1]~-~mu[1])^2, sigma[11])~-~2~rho~frac(x[1]~-~mu[1],
        sqrt(sigma[11]))~ frac(x[2]~-~mu[2],sqrt(sigma[22]))~+~
        frac((x[2]~-~mu[2])^2, sigma[22]),"]")),"}")),
      col="lightgreen",
      theta=10, phi=30,
      r=5.0,
      d=0.1,
      expand=0.5,
      ltheta=90, lphi=180,
      shade=0.75,
      ticktype="detailed",
      nticks=5) # produces the 3-D plot
mtext(expression(list(mu[1]==3,mu[2]==5,sigma[11]==10,sigma[22]==4,sigma[12
]==36,rho==0.9)), side=3) # adding a text line to the graph

# question 2.b (b) Generate n = 100 random observations from this distribution 
# and draw a scatterplot of the observations.

N = 100 # number of random samples
mu = c(mu1, mu2) # mean vector
sigma = matrix(c(s11^2, s11*s22*rho, s11*s22*rho, s22^2), 2) # var covar matrix

library(MASS)
bvn1 <- mvrnorm(N, mu = mu, Sigma = sigma ) # from MASS package
colnames(bvn1) <- c("X1","X2")
bvn1
plot(bvn1)

#2c
mu_bar <- c(mean(bvn1[,1]),mean(bvn1[,2]))
percent_dif <- c(100*(mu_bar[1]-mu[1])/mu[1], 100*(mu_bar[2]-mu[2])/mu[2])

#2d
s11_bar <- sqrt(var(bvn1[,1]))
s22_bar <- sqrt(var(bvn1[,2]))
s22_bar
cov(bvn1)

#3a
A = matrix(c(2, 4, 1, 3), 2, byrow = T)
B = matrix(c(-2, 1, 0, 4), 2, byrow = T)
C = matrix(c(3, 1, 2, 1), 2, byrow = T)

identical((A + B) + C, A + (B + C))
(A + B) + C
A + (B + C)
#3b
identical((A%*%B)%*%C, A%*%(B%*%C))
(A%*%B)%*%C
A%*%(B%*%C)
#3c
identical(A%*%(B + C), A%*%B + A%*%C)
A%*%(B + C)
A%*%B + A%*%C