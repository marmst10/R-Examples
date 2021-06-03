# STAT 8030 Homework 1 #

library(tidyverse)
library(readxl)
library(dplyr)
setwd("C:/Users/conno/OneDrive/Desktop/STAT 8030 - R Programming/Homework 1")

# Problem 1
bat15 <- readxl::read_xlsx("bat15.xlsx")
bat16 <- readxl::read_xlsx("bat16.xlsx")
bat17 <- readxl::read_xlsx("bat17.xlsx")
bat18 <- readxl::read_xlsx("bat18.xlsx")
bat19 <- readxl::read_xlsx("bat19.xlsx")
pitch15 <- readxl::read_xlsx("pitch15.xlsx")
pitch16 <- readxl::read_xlsx("pitch16.xlsx")
pitch17 <- readxl::read_xlsx("pitch17.xlsx")
pitch18 <- readxl::read_xlsx("pitch18.xlsx")
pitch19 <- readxl::read_xlsx("pitch19.xlsx")

# Problem 2
bat <- rbind(bat15,bat16,bat17,bat18,bat19)

# 3. For each team for each year, calculate the mean, 75th percentile, and standard
# deviation of homeruns hit and save these summary statistics along with the team 
# name and year in a new dataframe/tibble called bat_dat.

##what are columns?##
names(bat)

bat_dat <- bat %>%
  dplyr::group_by(yearID,teamID) %>%
  dplyr::summarize(mean_hr = mean(HR,na.rm=T),
                   q75_hr = quantile(HR,probs=0.75),
                   sd_hr = sd(HR))

# 4. Vertically join the pitching datasets together using whatever means you like.
pitch <- rbind(pitch15,pitch16,pitch17,pitch18,pitch19)

# 5. Perform the same operation from (3) here on the full pitching dataframe, except
# name the resulting dataframe/tibble pitch_dat. HINT: Be mindful to have different 
# column names for your summary statistics than what you had in (3).
names(pitch)

pitch_dat <- pitch %>%
  dplyr::group_by(yearID,teamID) %>%
  dplyr::summarize(mean_hr2 = mean(HR,na.rm=T),
                   q75_hr2 = quantile(HR,probs=0.75),
                   sd_hr2 = sd(HR))

# 6. Using whichever method you'd like, horizontally join bat_dat and pitch_dat 
# together in a new dataframe/tibble called hr_dat. There are a few different ways 
# you can go about this but each row in the resulting dataframe/tibble should 
# contain the mean, 75th percentile, and standard deviation for homeruns hit and 
# homeruns given up by pitchers for a particular team in a particular season. It 
# should also contain 150 rows and 8 columns.
hr_dat <- merge(bat_dat, pitch_dat)

# 7. Using whichever method you'd like, along with the yearID and teamID columns, 
# select only the mean homeruns hit and mean homeruns allowed columns.
hr_dat2 <- hr_dat %>%
  dplyr::select(yearID,
                teamID,
                mean_hr,
                mean_hr2)

# 8. For each year, determine which team had the minimum and maximum mean homeruns
# hit. HINT: You don't have to use the dataframe/tibble created in (7) if you don't 
# want to, but if you do, consider using a horizontal join to aid in solving this 
# problem.
?dplyr
hr_datmin <- hr_dat2 %>%
  dplyr::group_by(yearID) %>%
  dplyr::summarize(mean_hr = min(mean_hr)) %>%
  dplyr::mutate(desc = paste("Minimum Homeruns in", yearID))

hr_datmax <- hr_dat2 %>%
  dplyr::group_by(yearID) %>%
  dplyr::summarize(mean_hr = max(mean_hr)) %>%
  dplyr::mutate(desc = paste("Maximum Homeruns in", yearID))

hr_dat3 <- dplyr::union_all(hr_datmin, hr_datmax) 

hr_dat4 <- dplyr::inner_join(hr_dat2, hr_dat3, by = c("mean_hr", "yearID")) %>%
           dplyr::relocate(desc, .before = teamID) %>% 
           dplyr::relocate(mean_hr, .before = teamID) %>% 
           dplyr::arrange(yearID, desc) %>% 
           dplyr::select(-c("yearID", "mean_hr2")) 

# 9. Perform the same operation in (8) except determine which team had the minimum 
# and maximum mean homeruns allowed.
hr_datmin2 <- hr_dat2 %>%
  dplyr::group_by(yearID) %>%
  dplyr::summarize(mean_hr2 = min(mean_hr2)) %>%
  dplyr::mutate(desc = paste("Minimum Homeruns Allowed in", yearID))

hr_datmax2 <- hr_dat2 %>%
  dplyr::group_by(yearID) %>%
  dplyr::summarize(mean_hr2 = max(mean_hr2)) %>%
  dplyr::mutate(desc = paste("Maximum Homeruns Allowed in", yearID))

hr_dat5 <- dplyr::union_all(hr_datmin2, hr_datmax2) 

hr_dat6 <- dplyr::inner_join(hr_dat2, hr_dat5, by = c("mean_hr2", "yearID")) %>%
  dplyr::relocate(desc, .before = teamID) %>% 
  dplyr::relocate(mean_hr2, .before = teamID) %>%
  dplyr::arrange(yearID, desc) %>% 
  dplyr::select(-c("yearID", "mean_hr")) 
