## STAT 8030 PROJECT CODE

## DOWNLOAD COVID DATA TO ENVIRONMENT
library(tidyverse)#DATA MANIPULATION
library(ggplot2)#PLOTS
library(dplyr)#DATA MANIPULATION
library(ggpubr)#PLOTS
library(car)#ANOVA
library(nortest)#NORMALITY TESTS
library(moments)#SKEWNESS
library(geoR)#BOXCOXTRANSFORMATIONS
library(gamlss)#checking fit of distributions
library(gamlss.dist)#checking fit of distributions
library(gamlss.add)#checking fit of distributions
library(lubridate)#for week aggregating
library(Hmisc)#lag function
library(caret)#RMSE and R2 functions
library(ggpmisc)#show equation and r2 on graph
covid <- read.csv("C:/Users/conno/OneDrive/Desktop/STAT 8030 - R Programming/Project/potential datasets/owid-covid-data 11 22 20.csv")
summary(covid$iso_code)
summary(covid$continent)
summary(covid$location)
summary(covid$date)
summary(covid$total_cases)
summary(covid$new_cases)
summary(covid$new_cases_smoothed)
summary(covid$total_deaths)
summary(covid$new_deaths)
summary(covid$new_deaths_smoothed)
summary(covid$total_cases_per_million)
summary(covid$new_cases_per_million)
summary(covid$new_cases_smoothed_per_million)
summary(covid$total_deaths_per_million)
summary(covid$new_deaths_per_million)
summary(covid$new_deaths_smoothed_per_million)
summary(covid$reproduction_rate)
summary(covid$icu_patients)
summary(covid$icu_patients_per_million)
summary(covid$hosp_patients)
summary(covid$hosp_patients_per_million)
summary(covid$weekly_icu_admissions)
summary(covid$weekly_icu_admissions_per_million)
summary(covid$weekly_hosp_admissions)
summary(covid$weekly_hosp_admissions_per_million)
summary(covid$total_tests)
summary(covid$new_tests)
summary(covid$new_tests_smoothed)
summary(covid$total_tests_per_thousand)
summary(covid$new_tests_per_thousand)
summary(covid$new_tests_smoothed_per_thousand)
summary(covid$tests_per_case)
summary(covid$positive_rate)
summary(covid$tests_units)
summary(covid$stringency_index)
summary(covid$population)
summary(covid$population_density)
summary(covid$median_age)
summary(covid$aged_65_older)
summary(covid$aged_70_older)
summary(covid$gdp_per_capita)
summary(covid$extreme_poverty)
summary(covid$cardiovasc_death_rate)
summary(covid$diabetes_prevalence)
summary(covid$female_smokers)
summary(covid$male_smokers)
summary(covid$handwashing_facilities)
summary(covid$hospital_beds_per_thousand)
summary(covid$life_expectancy)
summary(covid$human_development_index)

##Are cases reported more frequently on certain days of the week?
covid$weekday <- weekdays(as.Date(covid$date))
head(covid$weekday)

#need to remove world totals
covid2 <- subset(covid, continent!="")
covid2$weekday <- weekdays(as.Date(covid2$date))
weekday <- covid2 %>% 
  group_by(weekday) %>% 
  summarise(Frequency = sum(new_cases,na.rm=T))

#need to sort by day of week
weekday$weekday <- ordered(weekday$weekday, levels= c("Sunday", "Monday", 
                                         "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

weekday[order(weekday$weekday), ]

## Bar Plot
options(scipen=10)
p <- ggplot(data = weekday, aes(x=weekday,y=Frequency)) + geom_bar(stat="identity")
p

# no visible significant difference in reported cases by day of week

# will perform an anova to validate this conclusion


group_by(covid2, weekday) %>%
  summarise(
    count = n(),
    mean = mean(new_cases, na.rm = TRUE),
    sd = sd(new_cases, na.rm = TRUE)
  )

#maybe put a table here?


#box plots with log base 10 y axis
ggboxplot(covid2, x = 'weekday', y = 'new_cases', 
          order = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"),
          ylab = "exp(new_cases)", xlab = "weekday") + scale_y_continuous(trans='exp')

#One way ANOVA
res.aov <- aov(new_cases ~ weekday, data = covid2)
# Summary of the analysis
summary(res.aov)

#With a p value of >0.05, we can conclude that there are no significant differences between
#the factor levels of weekday.

#now check anova assumptions

#

#check hov

leveneTest(new_cases ~ weekday, data = covid)
#with a p value >0.05, we can conclude that there is no evidence to suggest variance across
#groups is statistically different.

# check normality assumption

##AD Normality Test
ad.test(covid$new_cases, na.rm = TRUE)
#The data do not follow a normal distribution. We may need to perform a transformation on this data

#check skewness

skewness(covid$new_cases, na.rm = TRUE)
#skewness is +19.95. Highly positively skewed. will use log(10) transformation

covid$lognewcases <- log10(covid$new_cases)
#zeros return -inf. not useful!
#paper on how to handle zero values in log transformations https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3444996

#how to handle zero values? different approaches

#delete zeros. The effectiveness of this approach depends on the proportion of zeros
#in the dataset. How many zeros are there, exactly?

#Well, there are both N/A's and zeros. It would probably be best to remove N/A's and
#keep the zeros. An alternative approach would be to treat the N/A's as zeros. For
#completeness, I will check both.

length(which(covid$new_cases==0)) #18301 zeros out of 58470 observations

summary(covid$new_cases) #931 NA's out of 58470 observations. will turn these to zeros.
covid$new_cases[is.na(covid$new_cases)] <- 0
summary(covid$new_cases) #NA's are now 0
length(which(covid$new_cases==0))#total number of zeros is now 19232 out of 58470 observations
#thus, approximately one third of the data is zero. removing these is not appropriate.
#interestingly, there are negative values. Maybe these are corrections? They could be errors.
#the most negative value is -8261
#applying a shift of 8262 so all numbers are >0 for the analysis. Should not impact 
#shape of distribution or affect normality.
covid$shift<-covid$new_cases+8262
summary(covid$shift)
#perform log 10 transform on shifted data, no more 0's or negative values.
covid$lognewcases <- log10(covid$shift)

#is it normal? dont need na.rm = TRUE NA's have been removed!
ad.test(covid$lognewcases)
#log transform is not ideal. Does not normalize data

#still no. What does the density plot look like?
d <- density(covid2$new_cases, na.rm = T)
plot(d)
#density plot of log transformed data
d <- density(covid$lognewcases)
plot(d)

#The data appears exponentially distributed. There are some negative values, which are
#not allowed in exponential distributions. There are relatively few of these observations,
#so they will be temporarily removed.

#18 out of ~58470 observations are negative. These will be removed temporarily for the
#exp distribution tests.
length(which(covid2$new_cases<0))
covid2$new_cases[which(covid2$new_cases<0)] <- NA
summary(covid2$new_cases)

#Is the data exponentially distributed?
covexptest <- na.omit(covid2[,6])
#First, estimate the parameters
fit1 <- fitdistr(covexptest, "exponential") 
#now check goodness of fit
ks.test(covexptest, "pexp", fit1$estimate) # p-value > 0.05 -> distribution not refused

#plot a graph
hist(covexptest, freq = FALSE, breaks = 700, xlim = c(0, quantile(covexptest, 0.99)))
curve(dexp(x, rate = fit1$estimate), from = 0, col = "red", add = TRUE)

#not exactly an exponential distribution, but maybe close enough for government work.

#Although ANOVA is robust to departures from normality, a transformation should be 
#attempted. Dr Brown recommended a box cox power transformation or kruskal wallis

#attempting box cox transformation
covid8 <- read.csv("C:/Users/conno/OneDrive/Desktop/STAT 8030 - R Programming/Project/potential datasets/owid-covid-data 11 22 20.csv")
covid8$new_cases[is.na(covid8$new_cases)] <- 0
covid8$new_cases[which(covid8$new_cases<0)] <- NA
covid9<-covid8 %>% drop_na(new_cases)
covid9$weekday <- weekdays(as.Date(covid9$date))

#do we need a shift??
covid9$shift2<-covid9$new_cases+1

#generate linear model
covidmodel <- lm(shift2 ~ weekday, data = covid9)
plot(covidmodel)

#boxcox transformation on model
covidbc <- boxcox(covidmodel)

#extract best lambda
best.lam = covidbc$x[which(covidbc$y==max(covidbc$y))]

#generating model on +1 shifted response with optimized lambda
covbcmod <- lm((shift2)^best.lam~weekday, data = covid9)
plot(covbcmod)

#qq plot still showing very non-normal behavior. probably not feasible.

#try kruskal wallis test, good to use when ANOVA assumptions are not valid.
order = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")

covid9 <- subset(covid9, continent!="")

group_by(covid9, weekday) %>%
  summarise(
    count = n(),
    mean = mean(new_cases, na.rm = TRUE),
    sd = sd(new_cases, na.rm = TRUE),
    median = median(new_cases, na.rm = TRUE),
    IQR = IQR(new_cases, na.rm = TRUE)
  )

#run the kw test
kruskal.test(new_cases ~ weekday, data = covid9)
#The results indicate that there are significant differences between reporting by weekday.
#p value 2.886e-08

#which days are different? wilcox test p adjust method BH recommended per http://www.sthda.com/english/wiki/kruskal-wallis-test-in-r
pairwise.wilcox.test(covid9$new_cases, covid9$weekday,
                     p.adjust.method = "BH")
#combinations with p < 0.05 are significantly different.
#Mon Fri p 1.1e-06
#Fri Sun p 0.0016
#Fri Tue p 0.0012
#Fri Wed p 0.042
#Mon Sat p 6.3e-05
#Mon Thu p 1.7e-05
#Mon Wed p 0.0053
#Sat Sun p 0.0226
#Sat Tue p 0.0176
#Sun Thu p 0 0096
#Thu Tue 0.007

#Conclusion - while difficult to interpret given the structure of the response data,
#reporting of covid cases by day of week is not statistically similar. Therefore, these 
#apparent differences in reporting by weekday will be mitigated by aggregating data on
#a weekly basis.

#Plot of all daily reportings in the covid dataset with time on x axis and cases on y

plot(as.Date(covid2$date,"%Y-%m-%d"),covid2$new_cases,main="Scatterplot of Reported Covid-19 Cases versus Time",
     xlab="Date",ylab="Daily Number of Reported Cases")

#The number of daily reported cases over time shows some very interesting trends.
#Firstly, there are distinct groupings of case reports. The factors which differentiate these
#groupings should be investigated. 
#Secondly, a vast majority of the reported cases appear to cluster near 0. (compared to the
#largest values, on the scale of hundreds of thousands)

#ggplot2 allows for an easy method of color coding of points by group. 
qplot(as.Date(covid$date,"%Y-%m-%d"),#xaxis
      covid$new_cases,#yaxis
      colour = covid$continent,
      main="Scatterplot of Reported Covid-19 Cases versus Time",#title
      xlab="Date",
      ylab="Daily Number of Reported Cases")

#color coding daily reportings based on continent appears to cleanly divide the 
#apparent groupings.

#This is problematic. The world daily totals are included! These should be removed. 

covid2 <- subset(covid, continent!="")

#plotting regional daily reportings only, without world totals
qplot(as.Date(covid2$date,"%Y-%m-%d"),#xaxis
      covid2$new_cases,#yaxis
      colour = covid2$continent,
      main="Scatterplot of Reported Covid-19 Cases versus Time",#title
      xlab="Date",
      ylab="Daily Number of Reported Cases")+ labs(colour = 'Continent')

#What about cases per million? This is a useful statistic already calculated in the 
#dataset.
covid2$new_cases_per_million[which(covid2$new_cases_per_million<0)] <- NA
qplot(as.Date(covid2$date,"%Y-%m-%d"),#xaxis
      covid2$new_cases_per_million,#yaxis
      colour = covid2$continent,
      main="Scatterplot of Reported Covid-19 Cases per Million versus Time",#title
      xlab="Date",
      ylab="Daily Number of Reported Cases")+ labs(colour = 'Continent')
#This is not quite as easy to interpret. This might indicate that the apparent differences
#shown in the daily totals reported is largely due to population differences.

#try to answer this question "Did increased testing correlate with decreases in death rates?"

#need to calculate increases in testing. aggregating by week will help to smooth
#out the numbers. Weeks will be numbered, for simplicity.

covid2$week <- week(covid2$date)

#because we have multiple years in the dataset, both year and week must be considered.

covid2$year <- year(covid2$date)

#making NA's 0's
covid2$new_cases[is.na(covid2$new_cases)] <- 0
covid2$new_deaths[is.na(covid2$new_deaths)] <- 0
covid2$new_tests[is.na(covid2$new_tests)] <- 0

#summing new_tests, new_deaths, new_cases by continent, week, and year. 
covid3 <- covid2 %>%
  group_by(week, year, continent) %>%
  summarise(sum_new_tests = sum(new_tests),
            sum_new_deaths = sum(new_deaths),
            sum_new_cases = sum(new_cases),
            sum_new_cpm = sum(new_cases_per_million, na.rm = T))

#redesignate weeks. Only the last week in 2019 was reported, so it will be designated as week 0.
covid3$week[which(covid3$year==2019)] <- 0

#plot weekly totals of new_tests, new_deaths, new_cases by continent
qplot(covid3$week,#xaxis
      covid3$sum_new_cases,#yaxis
      geom=c("point", "line"),
      colour = covid3$continent,
      main="Scatterplot of Weekly Reported Covid-19 Cases versus Time",#title
      xlab="Date",
      ylab="Weekly Number of Reported Cases") + labs(colour = 'Continent')
#easier to interpret than daily totals for all countries (locations)

qplot(covid3$week,#xaxis
      covid3$sum_new_cpm,#yaxis
      geom=c("point", "line"),
      colour = covid3$continent,
      main="Scatterplot of Weekly Reported Covid-19 Cases per Million versus Time",#title
      xlab="Date",
      ylab="Weekly Number of Reported Cases")+ labs(colour = 'Continent')
#looking at the data on a per million basis shows that most regions cluster together while 
#europe has two large spikes both between weeks 10-20 and 40+. This spike appears to be on a 
#downward trend, but might be partially attributed to this data not containing all days in
#the final reporting week.


qplot(covid3$week,#xaxis
      covid3$sum_new_tests,#yaxis
      colour = covid3$continent,
      geom=c("point", "line"),
      main="Scatterplot of Weekly Reported Covid-19 Tests versus Time",#title
      xlab="Date",
      ylab="Weekly Number of Reported Tests")+ labs(colour = 'Continent')
#low numbers at most recent week probably due to taking data for part of week, should maybe 
#be removed so it is not misleading.

qplot(covid3$week,#xaxis
      covid3$sum_new_deaths,#yaxis
      geom=c("point", "line"),
      colour = covid3$continent,
      main="Scatterplot of Weekly Reported Covid-19 Deaths versus Time",#title
      xlab="Date",
      ylab="Weekly Number of Reported Deaths")+ labs(colour = 'Continent')
#large spike in deaths in Europe and North America between weeks 10 and 20.
#this is interesting, and may warrant further analysis.

#need to calculate weekly increases/decreases for each of the 3 parameters.

#sorting data by continent and week
covid4 <- arrange(covid3, continent, week)

covid5 <- covid4 %>%
  group_by(continent) %>%
  mutate(difcase = sum_new_cases - lag(sum_new_cases),
         diftest = sum_new_tests - lag(sum_new_tests),
         difdeath = sum_new_deaths - lag(sum_new_deaths),
         difcase2 = sum_new_cases - lag(sum_new_cases, k=2),
         diftest2 = sum_new_tests - lag(sum_new_tests, k=2),
         difdeath2 = sum_new_deaths - lag(sum_new_deaths, k=2),
         difcase3 = sum_new_cases - lag(sum_new_cases, k=3),
         diftest3 = sum_new_tests - lag(sum_new_tests, k=3),
         difdeath3 = sum_new_deaths - lag(sum_new_deaths, k=3))

#Is there any correlation between testing, deaths, and cases by continent
#when totals are aggregated by week?

#need to go back and add back in negative reportings of cases.
#These are likely corrections and could impact the results.
covid <- read.csv("C:/Users/conno/OneDrive/Desktop/STAT 8030 - R Programming/Project/potential datasets/owid-covid-data 11 22 20.csv")
covid2 <- subset(covid, continent!="")
covid2$week <- week(covid2$date)
covid2$year <- year(covid2$date)
covid2$new_cases[is.na(covid2$new_cases)] <- 0
covid2$new_deaths[is.na(covid2$new_deaths)] <- 0
covid2$new_tests[is.na(covid2$new_tests)] <- 0
covid3 <- covid2 %>%
  group_by(week, year, continent) %>%
  summarise(sum_new_tests = sum(new_tests),
            sum_new_deaths = sum(new_deaths),
            sum_new_cases = sum(new_cases))
covid3$week[which(covid3$year==2019)] <- 0
covid4 <- arrange(covid3, continent, week)



covid5 <- covid4 %>%
  group_by(continent) %>%
  mutate(difcase = sum_new_cases - Lag(sum_new_cases, shift = 1),
         diftest = sum_new_tests - Lag(sum_new_tests, shift = 1),
         difdeath = sum_new_deaths - Lag(sum_new_deaths, shift = 1),
         difcase2 = sum_new_cases - Lag(sum_new_cases, shift = 2),
         diftest2 = sum_new_tests - Lag(sum_new_tests, shift = 2),
         difdeath2 = sum_new_deaths - Lag(sum_new_deaths, shift = 2),
         difcase3 = sum_new_cases - Lag(sum_new_cases, shift = 3),
         diftest3 = sum_new_tests - Lag(sum_new_tests, shift = 3),
         difdeath3 = sum_new_deaths - Lag(sum_new_deaths, shift = 3))

#remove NA rows (first week, week=0) and last week (47) since it is not a full week.
covid6 <- subset(covid5, week!=0 & week!=47)

#removing columns not inspected in matrix
covid7 = subset(covid6, select = c(sum_new_tests, week, difcase,diftest,difdeath) )
covid13 = subset(covid6, select = c(sum_new_tests, week, difcase2,diftest,difdeath2) )
covid14 = subset(covid6, select = c(sum_new_tests, week, difcase3,diftest,difdeath3) )

#scatterplot matrix of week, difcases, diftests, and diftests
pairs(covid7[,2:5])
#no clear patterns

#scatterplot matrix lagged 2 weeks
pairs(covid13[,2:5])

#scatterplot matrix lagged 3 weeks
pairs(covid14[,2:5])

#what do distributions look like?
plot(density(covid7$difcase))
plot(density(covid7$diftest))
plot(density(covid7$difdeath))
#all highly peaked about zero with significant noise

#check normality
ad.test(covid7$difcase)#fails with p value of <2.2e-16
ad.test(covid7$diftest)#fails with p value of <2.2e-16
ad.test(covid7$difdeath)#fails with p value of <2.2e-16

#build linear model for all 3 combinations
linearMod1 <- lm(difcase ~ diftest, data=covid7)  # build linear regression model on full data
print(linearMod1)
linearMod2 <- lm(difcase ~ difdeath, data=covid7)  # build linear regression model on full data
print(linearMod2)
linearMod3 <- lm(diftest ~ difdeath, data=covid7)  # build linear regression model on full data
print(linearMod3)

#check diagnostics of simple linear regressions
summary(linearMod1)# r^2 ~ 0.15, p value ~2.6e-11
summary(linearMod2)# r^2 ~ 0.15, p value ~9.0e-12
summary(linearMod3)# r^2 ~ 0.0034, p value ~0.33

#no significant results of linear regression between these variables.
#all 3 variables failed normality test. transformation may mitigate this.

#a better approach may be to check correlations of previous testing to future rates/deaths

#try spline and linear regression. going to check the following relationships:
#1. weekly changes in testing rates with weekly changes in deaths 
#2. weekly changes in testing rates with 1 week lagged changes in deaths
#3. weekly changes in testing rates with 2 week lagged changes in deaths
#4. weekly changes in testing rates with weekly changes in cases 
#5. weekly changes in testing rates with 1 week lagged changes in cases
#6. weekly changes in testing rates with 2 week lagged changes in cases
#7. testing rates with weekly changes in deaths 
#8. testing rates with 1 week lagged changes in deaths
#9. testing rates with 2 week lagged changes in deaths
#10. testing rates with weekly changes in cases 
#11. testing  rates with 1 week lagged changes in cases
#12. testing  rates with 2 week lagged changes in cases

#1. weekly changes in testing rates with weekly changes in deaths 
knots1 <- quantile(covid7$diftest, p = c(0.25, 0.5, 0.75))
spline1 <- lm (difdeath ~ bs(diftest, knots = knots1), data = covid7)
linear1 <- lm(difdeath ~ diftest, data=covid7)
#spline plot
ggplot(covid7, aes(diftest, difdeath) ) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ splines::bs(x, df = 3))
#linear plot
ggplot(covid7, aes(diftest, difdeath) ) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ x)
#model performance
splpred1 <- spline1 %>% predict(covid7)
linpred1 <- linear1 %>% predict(covid7)
data.frame(
  Spline_RMSE = RMSE(splpred1, covid7$difdeath),
  Linear_RMSE = RMSE(linpred1, covid7$difdeath),
  Spline_R2 = R2(splpred1, covid7$difdeath),
  Linear_R2 = R2(linpred1, covid7$difdeath)
)#spline is a bit better r2, but still not good.


#2. weekly changes in testing rates with 1 week lagged changes in deaths
knots2 <- quantile(covid13$diftest, p = c(0.25, 0.5, 0.75))
spline2 <- lm (difdeath2 ~ bs(diftest, knots = knots2), data = covid13)
linear2 <- lm(difdeath2 ~ diftest, data=covid13)
#spline plot
ggplot(covid13, aes(diftest, difdeath2) ) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ splines::bs(x, df = 3))
#linear plot
ggplot(covid13, aes(diftest, difdeath2) ) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ x)
#model performance
splpred2 <- spline2 %>% predict(covid13)
linpred2 <- linear2 %>% predict(covid13)
data.frame(
  Spline_RMSE = RMSE(splpred2, covid13$difdeath2, na.rm = T),
  Linear_RMSE = RMSE(linpred2, covid13$difdeath2, na.rm = T),
  Spline_R2 = R2(splpred2, covid13$difdeath2, na.rm = T),
  Linear_R2 = R2(linpred2, covid13$difdeath2, na.rm = T)
)#spline is a bit better RMSE and R2, but still not good.

#3. weekly changes in testing rates with 2 week lagged changes in deaths
knots3 <- quantile(covid14$diftest, p = c(0.25, 0.5, 0.75))
spline3 <- lm (difdeath3 ~ bs(diftest, knots = knots2), data = covid14)
linear3 <- lm(difdeath3 ~ diftest, data=covid14)
#spline plot
ggplot(covid14, aes(diftest, difdeath3) ) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ splines::bs(x, df = 3))
#linear plot
ggplot(covid14, aes(diftest, difdeath3) ) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ x)
#model performance
splpred3 <- spline3 %>% predict(covid14)
linpred3 <- linear3 %>% predict(covid14)
data.frame(
  Spline_RMSE = RMSE(splpred3, covid14$difdeath3, na.rm = T),
  Linear_RMSE = RMSE(linpred3, covid14$difdeath3, na.rm = T),
  Spline_R2 = R2(splpred3, covid14$difdeath3, na.rm = T),
  Linear_R2 = R2(linpred3, covid14$difdeath3, na.rm = T)
)#spline is a bit better RMSE and R2, but still not good.

#4. weekly changes in testing rates with weekly changes in cases 
spline4 <- lm (difcase ~ bs(diftest, knots = knots1), data = covid7)
linear4 <- lm(difcase ~ diftest, data=covid7)
#spline plot
ggplot(covid7, aes(diftest, difcase) ) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ splines::bs(x, df = 3))
#linear plot
ggplot(covid7, aes(diftest, difcase) ) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ x)
#model performance
splpred4 <- spline4 %>% predict(covid7)
linpred4 <- linear4 %>% predict(covid7)
data.frame(
  Spline_RMSE = RMSE(splpred4, covid7$difcase),
  Linear_RMSE = RMSE(linpred4, covid7$difcase),
  Spline_R2 = R2(splpred4, covid7$difcase),
  Linear_R2 = R2(linpred4, covid7$difcase)
)#spline is a bit better RMSE and R2, but still not good.

#5. weekly changes in testing rates with 1 week lagged changes in cases
#6. weekly changes in testing rates with 2 week lagged changes in cases
#7. testing rates with weekly changes in deaths 
knots7 <- quantile(covid7$sum_new_tests, p = c(0.25, 0.5, 0.75))
spline7 <- lm (difdeath ~ bs(sum_new_tests, knots = knots7), data = covid7)
linear7 <- lm(difdeath ~ sum_new_tests, data=covid7)
#spline plot
ggplot(covid7, aes(sum_new_tests, difdeath) ) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ splines::bs(x, df = 3))
#linear plot
ggplot(covid7, aes(sum_new_tests, difdeath) ) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ x)
#model performance
splpred7 <- spline7 %>% predict(covid7)
linpred7 <- linear7 %>% predict(covid7)
data.frame(
  Spline_RMSE = RMSE(splpred7, covid7$difdeath),
  Linear_RMSE = RMSE(linpred7, covid7$difdeath),
  Spline_R2 = R2(splpred7, covid7$difdeath),
  Linear_R2 = R2(linpred7, covid7$difdeath)
)#spline is a bit better RMSE and R2, but still not good.


#8. testing rates with 1 week lagged changes in deaths
knots8 <- quantile(covid13$sum_new_tests, p = c(0.25, 0.5, 0.75))
spline8 <- lm (difdeath2 ~ bs(sum_new_tests, knots = knots8), data = covid13)
linear8 <- lm(difdeath2 ~ sum_new_tests, data=covid13)
#spline plot
ggplot(covid13, aes(sum_new_tests, difdeath2) ) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ splines::bs(x, df = 3))
#linear plot
ggplot(covid13, aes(sum_new_tests, difdeath2) ) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ x)
#model performance
splpred8 <- spline8 %>% predict(covid13)
linpred8 <- linear8 %>% predict(covid13)
data.frame(
  Spline_RMSE = RMSE(splpred8, covid13$difdeath2, na.rm = T),
  Linear_RMSE = RMSE(linpred8, covid13$difdeath2, na.rm = T),
  Spline_R2 = R2(splpred8, covid13$difdeath2, na.rm = T),
  Linear_R2 = R2(linpred8, covid13$difdeath2, na.rm = T)
)#spline is a bit better RMSE and R2, but still not good.

#9. testing rates with 2 week lagged changes in deaths
knots9 <- quantile(covid14$sum_new_tests, p = c(0.25, 0.5, 0.75))
spline9 <- lm (difdeath3 ~ bs(sum_new_tests, knots = knots9), data = covid14)
linear9 <- lm(difdeath3 ~ sum_new_tests, data=covid14)
#spline plot
ggplot(covid14, aes(sum_new_tests, difdeath3) ) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ splines::bs(x, df = 3))
#linear plot
ggplot(covid14, aes(sum_new_tests, difdeath3) ) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ x)
#model performance
splpred9 <- spline9 %>% predict(covid14)
linpred9 <- linear9 %>% predict(covid14)
data.frame(
  Spline_RMSE = RMSE(splpred9, covid14$difdeath3, na.rm = T),
  Linear_RMSE = RMSE(linpred9, covid14$difdeath3, na.rm = T),
  Spline_R2 = R2(splpred9, covid14$difdeath3, na.rm = T),
  Linear_R2 = R2(linpred9, covid14$difdeath3, na.rm = T)
)#spline is a bit better RMSE and R2, but still not good.

#10. testing rates with weekly changes in cases 
knots10 <- quantile(covid7$sum_new_tests, p = c(0.25, 0.5, 0.75))
spline10 <- lm (difcase ~ bs(sum_new_tests, knots = knots10), data = covid7)
linear10 <- lm(difcase ~ sum_new_tests, data=covid7)
#spline plot
ggplot(covid7, aes(sum_new_tests, difcase) ) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ splines::bs(x, df = 3))
#linear plot
ggplot(covid7, aes(sum_new_tests, difcase) ) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ x)
#model performance
splpred10 <- spline10 %>% predict(covid7)
linpred10 <- linear10 %>% predict(covid7)
data.frame(
  Spline_RMSE = RMSE(splpred10, covid7$difcase, na.rm = T),
  Linear_RMSE = RMSE(linpred10, covid7$difcase, na.rm = T),
  Spline_R2 = R2(splpred10, covid7$difcase, na.rm = T),
  Linear_R2 = R2(linpred10, covid7$difcase, na.rm = T)
)#spline is a bit better RMSE and R2, but still not good.


#11. testing  rates with 1 week lagged changes in cases
knots11 <- quantile(covid13$sum_new_tests, p = c(0.25, 0.5, 0.75))
spline11 <- lm (difcase2 ~ bs(sum_new_tests, knots = knots11), data = covid13)
linear11 <- lm(difcase2 ~ sum_new_tests, data=covid13)
#spline plot
ggplot(covid13, aes(sum_new_tests, difcase2) ) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ splines::bs(x, df = 3))
#linear plot
ggplot(covid13, aes(sum_new_tests, difcase2) ) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ x)
#model performance
splpred11 <- spline11 %>% predict(covid13)
linpred11 <- linear11 %>% predict(covid13)
data.frame(
  Spline_RMSE = RMSE(splpred11, covid13$difcase2, na.rm = T),
  Linear_RMSE = RMSE(linpred11, covid13$difcase2, na.rm = T),
  Spline_R2 = R2(splpred11, covid13$difcase2, na.rm = T),
  Linear_R2 = R2(linpred11, covid13$difcase2, na.rm = T)
)#spline is a bit better RMSE and R2, but still not good.


#12. testing  rates with 2 week lagged changes in cases


#realizing that it would make more sense to look at whether previous testing affected
#later case and death rates. going to reperform the data manipulation.
cov <- covid4 %>%
  group_by(continent) %>%
  mutate(pretest1 = Lag(sum_new_tests, shift = 1),
         pretest2 = Lag(sum_new_tests, shift = 2),
         pretest3 = Lag(sum_new_tests, shift = 3))

#remove NA rows (first week, week=0) and last week (47) since it is not a full week.
cov <- subset(cov, week!=0 & week!=47)

#take a look at correlation matrix
nolag <- c(4:6)
lag1 <- c(5:7)
lag2 <- c(5:6,8)
lag3 <- c(5:6,9)
pairs(cov[,nolag])
pairs(cov[,lag1])
pairs(cov[,lag2])
pairs(cov[,lag3])

#1. Linear model between total cases and total tests
ggplot(cov, aes(sum_new_tests, sum_new_cases) ) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ x) +
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  labs(title="Weekly Totals (by Continent) of Covid 19 cases vs. tests",
       x ="Weekly Testing Totals", 
       y = "Weekly Case Totals")

#2. Linear model between total cases and 1 week lagged testing
ggplot(cov, aes(pretest1, sum_new_cases) ) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ x) +
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  labs(title="Weekly Totals (by Continent) of Covid 19 cases vs. 1 Week Prior Testing Totals",
       x ="1 Week prior Weekly Testing Totals", 
       y = "Weekly Case Totals")

#3. Linear model between total cases and 2 week lagged testing
ggplot(cov, aes(pretest2, sum_new_cases) ) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ x) +
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  labs(title="Weekly Totals (by Continent) of Covid 19 cases vs. 2 Week Prior Testing Totals",
       x ="2 Week prior Weekly Testing Totals", 
       y = "Weekly Case Totals")

#4. Linear model between total cases and 3 week lagged testing
ggplot(cov, aes(pretest3, sum_new_cases) ) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ x) +
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  labs(title="Weekly Totals (by Continent) of Covid 19 cases vs. 3 Week Prior Testing Totals",
       x ="3 Week prior Weekly Testing Totals", 
       y = "Weekly Case Totals")

#5. Linear model between total deaths and total tests
ggplot(cov, aes(sum_new_tests, sum_new_deaths) ) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ x) +
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  labs(title="Weekly Totals (by Continent) of Covid 19 deaths vs. tests",
       x ="Weekly Testing Totals", 
       y = "Weekly death Totals")

#6. Linear model between total deaths and 1 week lagged testing
ggplot(cov, aes(pretest1, sum_new_deaths) ) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ x) +
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  labs(title="Weekly Totals (by Continent) of Covid 19 deaths vs. 1 Week Prior Testing Totals",
       x ="1 Week prior Weekly Testing Totals", 
       y = "Weekly death Totals")

#7. Linear model between total deaths and 2 week lagged testing
ggplot(cov, aes(pretest2, sum_new_deaths) ) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ x) +
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  labs(title="Weekly Totals (by Continent) of Covid 19 deaths vs. 2 Week Prior Testing Totals",
       x ="2 Week prior Weekly Testing Totals", 
       y = "Weekly death Totals")

#8. Linear model between total deaths and 3 week lagged testing
ggplot(cov, aes(pretest3, sum_new_deaths) ) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ x) +
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  labs(title="Weekly Totals (by Continent) of Covid 19 deaths vs. 3 Week Prior Testing Totals",
       x ="3 Week prior Weekly Testing Totals", 
       y = "Weekly death Totals")

#compare linear and spline
knots12 <- quantile(cov$sum_new_tests, p = c(0.25, 0.5, 0.75))
spline12 <- lm (sum_new_cases ~ bs(sum_new_tests, knots = knots12), data = cov)
linear12 <- lm(sum_new_cases ~ sum_new_tests, data=cov)
#spline plot
ggplot(cov, aes(sum_new_tests, sum_new_cases) ) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ splines::bs(x, df = 3))+
  labs(title="Weekly Totals (by Continent) of Covid 19 Cases vs. Testing Totals",
       x ="Weekly Testing Totals", 
       y = "Weekly Case Totals")
#linear plot
ggplot(cov, aes(sum_new_tests, sum_new_cases) ) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ x)
#model performance
splpred12 <- spline12 %>% predict(cov)
linpred12 <- linear12 %>% predict(cov)
data.frame(
  Spline_RMSE = RMSE(splpred12, cov$sum_new_cases, na.rm = T),
  Linear_RMSE = RMSE(linpred12, cov$sum_new_cases, na.rm = T),
  Spline_R2 = R2(splpred12, cov$sum_new_cases, na.rm = T),
  Linear_R2 = R2(linpred12, cov$sum_new_cases, na.rm = T)
)#spline is a bit better RMSE and R2, but still not good.

#compare linear and spline
knots13 <- quantile(cov$sum_new_tests, p = c(0.25, 0.5, 0.75))
spline13 <- lm (sum_new_deaths ~ bs(sum_new_tests, knots = knots13), data = cov)
linear13 <- lm(sum_new_deaths ~ sum_new_tests, data=cov)
#spline plot
ggplot(cov, aes(sum_new_tests, sum_new_deaths) ) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ splines::bs(x, df = 3))+
  labs(title="Weekly Totals (by Continent) of Covid 19 Deaths vs. Testing Totals",
       x ="Weekly Testing Totals", 
       y = "Weekly Death Totals")
#linear plot
ggplot(cov, aes(sum_new_tests, sum_new_deaths) ) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ x)
#model performance
splpred13 <- spline13 %>% predict(cov)
linpred13 <- linear13 %>% predict(cov)
data.frame(
  Spline_RMSE = RMSE(splpred13, cov$sum_new_deaths, na.rm = T),
  Linear_RMSE = RMSE(linpred13, cov$sum_new_deaths, na.rm = T),
  Spline_R2 = R2(splpred13, cov$sum_new_deaths, na.rm = T),
  Linear_R2 = R2(linpred13, cov$sum_new_deaths, na.rm = T)
)#spline is a bit better RMSE and R2, but still not good.


