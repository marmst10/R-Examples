# STAT 8210 Homework 2
# Connor Armstrong

#############################################
#' Collinearity diagnostics
#'
#' @description
#' Variance inflation factor, tolerance, eigenvalues and condition indices.
#'
#' @param model An object of class \code{lm}.
#'
#' @details
#' Collinearity implies two variables are near perfect linear combinations of
#' one another. Multicollinearity involves more than two variables. In the
#' presence of multicollinearity, regression estimates are unstable and have
#' high standard errors.
#'
#' \emph{Tolerance}
#'
#' Percent of variance in the predictor that cannot be accounted for by other predictors.
#'
#' Steps to calculate tolerance:
#'
#' \itemize{
#'   \item Regress the kth predictor on rest of the predictors in the model.
#'   \item Compute \eqn{R^2} - the coefficient of determination from the regression in the above step.
#'   \item \eqn{Tolerance = 1 - R^2}
#' }
#'
#' \emph{Variance Inflation Factor}
#'
#' Variance inflation factors measure the inflation in the variances of the parameter estimates due to
#' collinearities that exist among the predictors. It is a measure of how much the variance of the estimated
#' regression coefficient \eqn{\beta_k}  is inflated by the existence of correlation among the predictor variables
#' in the model. A VIF of 1 means that there is no correlation among the kth predictor and the remaining predictor
#' variables, and hence the variance of \eqn{\beta_k} is not inflated at all. The general rule of thumb is that VIFs
#' exceeding 4 warrant further investigation, while VIFs exceeding 10 are signs of serious multicollinearity
#' requiring correction.
#'
#' Steps to calculate VIF:
#'
#' \itemize{
#'   \item Regress the kth predictor on rest of the predictors in the model.
#'   \item Compute \eqn{R^2} - the coefficient of determination from the regression in the above step.
#'   \item \eqn{Tolerance = 1 / 1 - R^2 = 1 / Tolerance}
#' }
#'
#' \emph{Condition Index}
#'
#' Most multivariate statistical approaches involve decomposing a correlation matrix into linear
#' combinations of variables. The linear combinations are chosen so that the first combination has
#' the largest possible variance (subject to some restrictions), the second combination
#' has the next largest variance, subject to being uncorrelated with the first, the third has the largest
#' possible variance, subject to being uncorrelated with the first and second, and so forth. The variance
#' of each of these linear combinations is called an eigenvalue. Collinearity is spotted by finding 2 or
#' more variables that have large proportions of variance (.50 or more) that correspond to large condition
#' indices. A rule of thumb is to label as large those condition indices in the range of 30 or larger.
#'
#'
#' @return \code{ols_coll_diag} returns an object of class \code{"ols_coll_diag"}.
#' An object of class \code{"ols_coll_diag"} is a list containing the
#' following components:
#'
#' \item{vif_t}{tolerance and variance inflation factors}
#' \item{eig_cindex}{eigen values and condition index}
#'
#' @references
#' Belsley, D. A., Kuh, E., and Welsch, R. E. (1980). Regression Diagnostics: Identifying Influential Data and
#' Sources of Collinearity. New York: John Wiley & Sons.
#'
#' @examples
#' # model
#' model <- lm(mpg ~ disp + hp + wt + drat, data = mtcars)
#'
#' # vif and tolerance
#' ols_vif_tol(model)
#'
#' # eigenvalues and condition indices
#' ols_eigen_cindex(model)
#'
#' # collinearity diagnostics
#' ols_coll_diag(model)
#'
#' @export
#'
ols_coll_diag <- function(model) UseMethod("ols_coll_diag")

#' @export
#'
ols_coll_diag.default <- function(model) {
  
  check_model(model)
  
  vift    <- ols_vif_tol(model)
  eig_ind <- ols_eigen_cindex(model)
  result  <- list(vif_t = vift, eig_cindex = eig_ind)
  
  class(result) <- "ols_coll_diag"
  return(result)
  
}

#' @export
#'
print.ols_coll_diag <- function(x, ...) {
  cat("Tolerance and Variance Inflation Factor\n")
  cat("---------------------------------------\n")
  print(x$vif_t)
  cat("\n\n")
  cat("Eigenvalue and Condition Index\n")
  cat("------------------------------\n")
  print(x$eig_cindex)
}

#' @rdname ols_coll_diag
#' @export
#'
ols_vif_tol <- function(model) {
  
  check_model(model)
  vt <- viftol(model)
  data.frame(Variables = vt$nam, Tolerance = vt$tol, VIF = vt$vifs)
  
}

#' @rdname ols_coll_diag
#' @export
#'
ols_eigen_cindex <- function(model) {
  
  check_model(model)
  
  pvdata <- NULL
  x      <- as.data.frame(model.matrix(model))
  e      <- evalue(x)$e
  cindex <- cindx(e)
  pv     <- pveindex(evalue(x)$pvdata)
  out    <- data.frame(Eigenvalue = cbind(e, cindex, pv))
  
  colnames(out) <- c("Eigenvalue", "Condition Index", colnames(evalue(x)$pvdata))
  return(out)
  
}


evalue <- function(x) {
  
  values         <- NULL
  y              <- x
  colnames(y)[1] <- "intercept"
  z              <- scale(y, scale = T, center = F)
  tu             <- t(z) %*% z
  
  e <- eigen(tu / diag(tu))$values
  list(e = e, pvdata = z)
  
}


cindx <- function(e) {
  sqrt(e[1] / e) 
}

pveindex <- function(z) {
  
  d     <- NULL
  v     <- NULL
  svdx  <- svd(z)
  svdxd <- svdx$d
  
  phi_diag <- diag(1 / svdxd)
  phi      <- svdx$v %*% phi_diag
  ph       <- t(phi ^ 2)
  diag_sum <- diag(rowSums(ph, dims = 1))
  
  prop.table(ph %*% diag_sum, margin = 2)
  
}


fmrsq <- function(nam, data, i) {
  
  r.squared <- NULL
  fm        <- as.formula(paste0("`", nam[i], "` ", "~ ."))
  m1        <- summary(lm(fm, data = data))$r.squared
  
  1 - m1
  
}


#' @description Computes vif and tolerance
#'
#' @noRd
#'
viftol <- function(model) {
  
  m   <- as.data.frame(model.matrix(model))[, -1]
  nam <- names(m)
  p   <- length(model$coefficients) - 1
  tol <- c()
  
  for (i in seq_len(p)) {
    tol[i] <- fmrsq(nam, m, i)
  }
  
  vifs <- 1 / tol
  list(nam = names(m), tol = tol, vifs = vifs)
  
}















































































# LOAD DATASETS PACKAGES ###################################

library(datasets)  # Load/unload base packages manually
library(olsrr)
library(coala)
library(sjmisc)
library(mvtnorm)
library(sjPlot)
library(car)
# Installs pacman ("package manager") if needed
if (!require("pacman")) install.packages("pacman")
install.packages("olsrr")
install.packages("coala")
install.packages("sjmisc")
install.packages("sjPlot")
install.packages("strengejacke")


# Use pacman to load add-on packages as desired
pacman::p_load(pacman, rio) 

# HomeworK 3 ###############################################

# Import Data
q1 <- import("/Users/conno/OneDrive/Desktop/STAT 8210 - Applied Regression Analysis/Homework 3/anscombe1.csv")
head(q1)

# Question 1: Build Models
fit1 <- lm(y1 ~ x1, data=q1)
fit2 <- lm(y2 ~ x2, data=q1)
fit3 <- lm(y3 ~ x3, data=q1)
fit4 <- lm(y4 ~ x4, data=q1)

# Question 1: Display Coefficients and R^2
summary(fit1)
summary(fit2)
summary(fit3)
summary(fit4)

# Question 1: Provides some Diagnostics Tables. Vague
influence(fit1)

# Question 1: Diagnostic Plots
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(fit1)
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(fit2)
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(fit3)
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(fit4)

# Question 2: Manually input data
# create a data frame from scratch
temp <- c(24.9, 35, 44.9, 55.1, 65.2, 75.2, 85.2, 95.2)
visc <- c(1.1330, 0.9772, 0.8532, 0.7550, 0.6723, 0.6021, 0.5420, 0.5074)
temp2 <- temp*temp
q2 <- data.frame(temp, temp2, visc) 
head(q2)


# Question 2: Fit the MLR Model
fit5 <- lm(visc ~ temp+temp2, data=q2)
summary(fit5)

# Question 3: Import Data
q3 <- import("/Users/conno/OneDrive/Desktop/STAT 8210 - Applied Regression Analysis/Homework 3/Property_Value_Table_B_4.csv")
head(q3)

# Question 3: Fit Model
fit6 <- lm(y ~ x1+x2+x3+x4+x5+x6+x7+x8+x9, data=q3)
summary(fit6)

# Question 3 Multicollinearity Check
anova(fit6)
vif(fit6)
tol(fit6)
ols_vif_tol(fit6)
ols_coll_diag(fit6) #
ols_eigen_cindex(fit6)

# Question 3: Partial F-test
fit6mod <- lm(y ~ x1+x2+x3+x4+x7+x8, data=q3)
summary(fit6mod)
anova(fit6mod, fit6)

# Question 4: Import Data
q4 <- import("/Users/conno/OneDrive/Desktop/STAT 8210 - Applied Regression Analysis/Homework 3/NFL_table_B1.csv")
head(q4)

# Question 4: Build Model and Check Diagnostics
fit7 <-lm(y~x2+x7+x8, data=q4)
summary(fit7)
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(fit7)

# Question 4: Plot variables vs Residuals
layout(matrix(c(1),1,1))
plot(q4$x2, fit7$residuals)
plot(q4$x7, fit7$residuals)
plot(q4$x8, fit7$residuals)

# Question 4: Partial Regression Plots
avPlots(fit7)

# Question 4: Studentized Residuals and RStudent
library(MASS)
studres(fit7)
rstudent(fit7)

# Reference Code for Plotting and LR
plot(mtcars$wt, mtcars$mpg)

# 2)	Fit the simple linear regression model with wt as explanatory variable, 
#     mpg as response variable.  Report the estimated regression line.

cor(mtcars$wt, mtcars$mpg)  # calculate correlation between weight and mpg 
linearMod <- lm(mpg ~ wt, data=mtcars)  # build linear regression model on full data
print(linearMod)

# 3)	Construct and interpret a 95% confidence interval for:
#   a.	??0

attach(mtcars)     # attach the data frame 
mpg.lm = lm(mpg ~ wt)
newdata = data.frame(wt = 3) #predicting intercept preditcion and confidence intervals
predict(eruption.lm, newdata, interval="predict") #prediction interval
predict(eruption.lm, newdata, interval="confidence") #confidence interval




# Clear packages
detach("package:datasets", unload = TRUE)  # For base

# Clear plots
dev.off()  # But only if there IS a plot

# Clear console
cat("/014")  # ctrl+L

# Clear mind :)
