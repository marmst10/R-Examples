# STAT 8210 Homework 1
# Connor Armstrong

# LOAD DATASETS PACKAGES ###################################

library(datasets)  # Load/unload base packages manually
                   # Includes mtcars

# 1)	Plot mpg on y axis vs. wt [vehicle weight in half-tons] on x-axis.  
#     Include this plot.  Does the pattern look linear?

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

#   b.	??1 


# CLEAN UP #################################################

# Clear packages
detach("package:datasets", unload = TRUE)  # For base

# Clear plots
dev.off()  # But only if there IS a plot

# Clear console
cat("\014")  # ctrl+L

# Clear mind :)
