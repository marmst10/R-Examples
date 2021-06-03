# STAT 8210 Homework 2
# Connor Armstrong

# LOAD DATASETS PACKAGES ###################################

library(datasets)  # Load/unload base packages manually

# Installs pacman ("package manager") if needed
if (!require("pacman")) install.packages("pacman")

# Use pacman to load add-on packages as desired
pacman::p_load(pacman, rio) 

# Homework #################################################
# 1)	make a scatterplot of the data

ozone <- import("/Users/conno/OneDrive/Desktop/STAT 8210 - Applied Regression Analysis/Homework 2/HW2 Q1.csv")
head(ozone)

plot(ozone$Index, ozone$Days,
  main = "Figure 1: Scatterplot of Days where Ozone Levels\nExceeded 0.20 ppm vs. Seasonal Meteorological Index",
  xlab = "Seasonal Meteorological Index",
  ylab = "# Days/yr above 0.20 ppm")

# 2)  estimate the prediction equation

cor(ozone$Index, ozone$Days)  # calculate correlation between weight and mpg 
linearMod <- lm(Days ~ Index, data=ozone)  # build linear regression model on full data
print(linearMod)
# CLEAN UP #################################################

# Clear packages
detach("package:datasets", unload = TRUE)  # For base

# Clear plots
dev.off()  # But only if there IS a plot

# Clear console
cat("\014")  # ctrl+L

# Clear mind :)
