# STAT 8230 Homework 3
library(CCA)

# Question 1.b Perform PCA for this dataset and report the result by 
# 1) scatterplot matrix of principal components and 
# 2) scree plot.

pendigit3 <- read.csv("C:/Users/conno/OneDrive/Desktop/STAT 8230 - Applied Multivariate Data Analysis/Homework/Homework 3/pendigit3.txt", header = FALSE)

names(pendigit3)[1] <- "x1"
names(pendigit3)[2] <- "y1"
names(pendigit3)[3] <- "x2"
names(pendigit3)[4] <- "y2"
names(pendigit3)[5] <- "x3"
names(pendigit3)[6] <- "y3"
names(pendigit3)[7] <- "x4"
names(pendigit3)[8] <- "y4"
names(pendigit3)[9] <- "x5"
names(pendigit3)[10] <- "y5"
names(pendigit3)[11] <- "x6"
names(pendigit3)[12] <- "y6"
names(pendigit3)[13] <- "x7"
names(pendigit3)[14] <- "y7"
names(pendigit3)[15] <- "x8"
names(pendigit3)[16] <- "y8"
names(pendigit3)[17] <- "class"

colnames(pendigit3)

x = pendigit3[,-17]

pca <- prcomp(x,
              center = TRUE,
              scale. = TRUE) 

pca$x

print(pca)

plot(pca, type = "l")

summary(pca)

predict(pca, 
        newdata=tail(x, 2))

pairs(pca$x[,1:6])

# Question 1c Do the data look as if they are from a MVN distribution?

# Identify an outlier.

# Using method described in:
# https://www.r-bloggers.com/2019/01/a-new-way-to-handle-multivariate-outliers/

library(psych)

# outlier function from psych package plots QQ plot for outliers using 
# Mahalanobis Distance (MD)
outlier(pendigit3[,-17])


md <- mahalanobis(pendigit3[,-17], center = colMeans(pendigit3[,-17]), cov = cov(pendigit3[,-17]))
alpha <- .05
cutoff <- (qchisq(p = 1 - alpha, df = ncol(pendigit3[,-17])))
names_outliers_MH <- which(md > cutoff)
excluded_mh <- names_outliers_MH

# print MD for all observations
md

# print observations which were flagged as outliers
excluded_mh

# 37 total outliers out of 1055 observations at alpha = 0.001
# 105 total outliers out of 1055 observations at alpha = 0.05
length(excluded_mh)

# normality test using MVN package
# https://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=&ved=2ahUKEwj4_OPg3-3vAhWIX80KHSvqCdcQFjAAegQIAxAD&url=https%3A%2F%2Fcran.r-project.org%2Fweb%2Fpackages%2FMVN%2Fvignettes%2FMVN.pdf&usg=AOvVaw3Y93Fy-o6qSjO6KDxKDFwx
library(MVN)

# Mardia's MVN test
pendigit3mardia <- mvn(data = pendigit3[,-17], mvnTest = "mardia")
pendigit3mardia$multivariateNormality
# p-value: 0   result: no

# Henze-Zirkler's MVN test
pendigit3hz <- mvn(data = pendigit3[,-17], mvnTest = "hz")
pendigit3hz$multivariateNormality
# p-value: 0   result: no

# Royston's MVN test
pendigit3royston <- mvn(data = pendigit3[,-17], mvnTest = "royston")
pendigit3royston$multivariateNormality
# p-value: 4.11e-275   result: no

# Doornik-Hansen's MVN test
pendigit3dh <- mvn(data = pendigit3[,-17], mvnTest = "dh")
pendigit3dh$multivariateNormality$MVN
pendigit3dh$multivariateNormality$`p value`
# p-value: 0   result: no

# what about x/y combinations
library(tidyverse)

mvn_p_values <- vector("double", 8) 
mvn_results <- vector("double", 8) 

for (i in 1:8) {         
  temp <- mvn(data = pendigit3[,c(-1+2*i, 2*i)], mvnTest = "dh")
  
  mvn_p_values[[i]] <- temp$multivariateNormality$MVN
  mvn_results[[i]] <- temp$multivariateNormality$`p value`
}

mvn_p_values 
mvn_results

summary(pendigit3)

par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
colors=c("blue","red","orange","black","dark red","green","coral1","deeppink")
for (i in 1:8) { 
  if (i == 1) {
    plot(pendigit3[,c(-1+2*i, 2*i)], 
         xlim=c(0,100), ylim=c(0,100), 
         col=colors[i], pch=20, 
         xlab="X", ylab="Y")
  } else {
    points(pendigit3[,-1+2*i], pendigit3[,2*i], col=colors[i], pch=20)
  }
}
legend("topright", 
       inset=c(-0.2,0),
       legend=c(1:8), 
       col=c("blue","red","orange","black","dark red","green","coral1","deeppink"),
       pch=20,
       title="Step"
      )

#univariate normality tests
library(nortest)

univnorm_results <- vector("double", 16) 
for (i in 1:16) {         
  temp <- ad.test(pendigit3[,i])
  univnorm_results[[i]] <- temp$p.value
}
univnorm_results

univnorm_pca <- vector("double", 16) 
univnorm_pca_result <- vector("double", 16) 
for (i in 1:16) {         
  temp <- ad.test(pca$x[,i])
  univnorm_pca[[i]] <- temp$p.value
  if(temp$p.value < 0.05){
    univnorm_pca_result[[i]] <- "No"
  } else {
    univnorm_pca_result[[i]] <- "Yes"
  }
}
univnorm_pca
univnorm_pca_result

# 1e
pairs(pca$x[,1:4])


# 1f
pendigit8 <- read.csv("C:/Users/conno/OneDrive/Desktop/STAT 8230 - Applied Multivariate Data Analysis/Homework/Homework 3/pendigit8.txt", header = FALSE)

names(pendigit8)[1] <- "x1"
names(pendigit8)[2] <- "y1"
names(pendigit8)[3] <- "x2"
names(pendigit8)[4] <- "y2"
names(pendigit8)[5] <- "x3"
names(pendigit8)[6] <- "y3"
names(pendigit8)[7] <- "x4"
names(pendigit8)[8] <- "y4"
names(pendigit8)[9] <- "x5"
names(pendigit8)[10] <- "y5"
names(pendigit8)[11] <- "x6"
names(pendigit8)[12] <- "y6"
names(pendigit8)[13] <- "x7"
names(pendigit8)[14] <- "y7"
names(pendigit8)[15] <- "x8"
names(pendigit8)[16] <- "y8"
names(pendigit8)[17] <- "class"

colnames(pendigit8)

y = pendigit8[,-17]

pca <- prcomp(y,
              center = TRUE,
              scale. = TRUE) 

pca$x
library(dplyr)
pendigit38 <- dplyr::union_all(pendigit3, pendigit8)

pca38 <- prcomp(pendigit38[,-17],
              center = TRUE,
              scale. = TRUE) 

plot(pca38)

pairs(pca38$x[,1:6],pendigit38$class)

pca38$x
pendigit38[,-17]

plot(pca38$x[,1],pendigit38$class)

temp <- as.data.frame(pca38$x[,1:6])
temp$class <- pendigit38$class

cols <- character(nrow(temp))

#initialize colors
cols[] <- "red"
cols[temp$class == 3] <- "black"

#collate so all 8's aren't on top of the 3's
temp$collate <- 0

#numbers has mod operator
install.packages("numbers")
library(numbers)
for (i in 1:nrow(temp)) {
  if (mod(i,2)==1) {
    temp$collate[i] <- 1
  }
}
temp <- temp[order(temp$collate),] 

#scatterplot matrix of PC's for pendigit38
temp$class <- as.factor(temp$class)

pairs(temp[,1:4],col=cols,pch = 20, cex=0.25, oma=c(3,3,3,15))
par(xpd = TRUE)

