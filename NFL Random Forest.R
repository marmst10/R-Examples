# Restarting fresh 5/4/21
library(tidyverse)
library(dplyr)
library(TTR)#EWMA
library(writexl)
library(rsample)      # data splitting 
library(randomForest) # basic implementation
library(ranger)#more efficient than randomForest
library(h2o)

# import data
seasons <- 2015:2020
all <- map_df(seasons, function(x) {
  readRDS(
    url(
      paste0("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_",x,".rds")
    )
  )
})

# only regular season
all <- all[ which(all$season_type == 'REG'), ]

# set run gap na's to guard
all$run_gap = ifelse(is.na(all$run_gap)&all$play_type=="run", "guard", all$run_gap)

# bring in weather keys and append weather_type variable
weatherkeys <- read.csv("C:/Users/conno/OneDrive/Desktop/STAT 8940 - Applied Analysis Project/Data/weather/weather description keys.csv")
weatherkeys <- weatherkeys[,c("weather","weather_type")]

# append weather keys
all <- all %>%
  dplyr::left_join(weatherkeys, by = "weather")

# set indoor games to indoor weather
all$weather_type = ifelse(all$roof=="dome"|all$roof=="closed", "Indoor", all$weather_type)

# missing weather - obtained via python
missingweatherkeys5 <- read.csv("C:/Users/conno/OneDrive/Desktop/STAT 8940 - Applied Analysis Project/Data/missingweatherkeys_corrected_041821.csv")
missingweatherkeys <- missingweatherkeys5[,c("joinkey", "weather_type5")]

# format date
all$game_date <- as.Date(all$game_date,format = "%Y-%m-%d")

# create key for joining missing weather
all$joinkey <- paste(all$game_date,all$stadium_id)

# append missing weather data
all <- all %>%
  dplyr::left_join(missingweatherkeys, by = "joinkey")

# missing weather replace
all$weather_type <- ifelse(all$weather_type=="MISSING",all$weather_type5,all$weather_type)

# 0's were determined to be cloudy
all$weather_type <- ifelse(all$weather_type==0,"Cloudy",all$weather_type)

# scoring defense by pass and rush plays
def_week_rush <- all %>%
  dplyr::filter(play_type == 'run') %>%
  dplyr::group_by(season, week, defteam) %>%
  dplyr::summarize(rush_td = sum(rush_touchdown), 
                   rush_yd = sum(yards_gained))

def_week_pass <- all %>%
  dplyr::filter(play_type == 'pass') %>%
  dplyr::group_by(season, week, defteam) %>%
  dplyr::summarize(rec_td = sum(pass_touchdown), 
                   rec_yd = sum(yards_gained))

# creating EMA
# http://finzi.psych.upenn.edu/library/TTR/html/MovingAverages.html
# mathematical definition
# https://www.fmlabs.com/reference/default.htm?url=ExpMA.htm

# using 3 weeks here

# def ema scores
def_week_rush <- def_week_rush %>%
  dplyr::arrange(desc(defteam))%>% 
  dplyr::group_by(defteam, season) %>%
  dplyr::mutate(def_rush_td_ema = EMA(rush_td, 3), 
                def_rush_yd_ema = EMA(rush_yd, 3)) %>%
  dplyr::ungroup()

def_week_pass <- def_week_pass %>%
  dplyr::arrange(desc(defteam))%>% 
  dplyr::group_by(defteam, season) %>%
  dplyr::mutate(def_rec_td_ema = EMA(rec_td, 3), 
                def_rec_yd_ema = EMA(rec_yd, 3)) %>%
  dplyr::ungroup()

# set na's to 0
def_week_pass$def_rec_td_ema = ifelse(is.na(def_week_pass$def_rec_td_ema), 0, def_week_pass$def_rec_td_ema)
def_week_pass$def_rec_yd_ema = ifelse(is.na(def_week_pass$def_rec_yd_ema), 0, def_week_pass$def_rec_yd_ema)
def_week_rush$def_rush_td_ema = ifelse(is.na(def_week_rush$def_rush_td_ema), 0, def_week_rush$def_rush_td_ema)
def_week_rush$def_rush_yd_ema = ifelse(is.na(def_week_rush$def_rush_yd_ema), 0, def_week_rush$def_rush_yd_ema)

# keep only ema, team, season, week
def_week_rush <- def_week_rush[,c("defteam", "week", "season", "def_rush_td_ema", "def_rush_yd_ema")]

def_week_pass <- def_week_pass[,c("defteam", "week", "season", "def_rec_td_ema", "def_rec_yd_ema")]

# can now append def scores
all <- all %>%
  dplyr::left_join(def_week_rush, by = c("defteam", "week", "season"))

all <- all %>%
  dplyr::left_join(def_week_pass, by = c("defteam", "week", "season"))

# windangle - used excel for text decomposition
#weather <- all$weather
#write_xlsx(as.data.frame(weather),"C:/Users/conno/OneDrive/Desktop/STAT 8940 - Applied Analysis Project/Data/weather.xlsx")
windangle <- read.csv("C:/Users/conno/OneDrive/Desktop/STAT 8940 - Applied Analysis Project/Data/weather/weather_eval_csv.csv")
all$windangle <- as.numeric(windangle[,2])

# bring in stadium orientation
stadorien <- read.csv("C:/Users/conno/OneDrive/Desktop/STAT 8940 - Applied Analysis Project/Data/Stadiums/stadiums_orientation_final_final.csv")
stadorien2 <- stadorien[,c("stadium_id", "orientation")]
all <- all %>% dplyr::left_join(stadorien2, by = "stadium_id")
all <- all %>%
  mutate(toward_home_wind = ifelse(is.na(windangle) | !roof %in% c("open","outdoors"), 0, wind*sin((as.numeric(windangle)-orientation)*pi/180)),
         crosswind = ifelse(is.na(windangle) | !roof %in% c("open","outdoors"), 0, wind*cos((as.numeric(windangle)-orientation)*pi/180)))

rushtemp <- all %>% 
  dplyr::filter(play_type == 'pass')
summary(rushtemp$down)

# split by pass/rush and calculate score

# old code
# rush <- all %>% 
#   dplyr::filter(play_type == 'run') %>%
#   dplyr::mutate(score = 6 * touchdown + (yards_gained - (yards_gained)%%10)/10 + -2 * fumble,
#                 temp = ifelse(is.na(temp), median(temp, na.rm = T), temp),
#                 crosswind = ifelse(is.na(crosswind), 0, abs(crosswind)),
#                 toward_home_wind = ifelse(is.na(toward_home_wind), 0, abs(toward_home_wind)))%>%
#   dplyr::filter(!is.na(drive)&!is.na(down)) #removing na's of drive and down, very small percentage
# changed | to & in last filter, I think that's what I should have done before.
# pass <- all %>%
#   dplyr::filter(play_type == 'pass') %>%
#   dplyr::mutate(score = 6 * touchdown + (yards_gained - (yards_gained)%%10)/10 + -2 * fumble,
#                 air_yards = ifelse(is.na(air_yards), 0, air_yards),
#                 temp = ifelse(is.na(temp), median(temp, na.rm = T), temp),
#                 crosswind = ifelse(is.na(crosswind), 0, abs(crosswind)),
#                 toward_home_wind = ifelse(is.na(toward_home_wind), 0, abs(toward_home_wind)))%>%
#   dplyr::filter(!is.na(drive)&!is.na(down)) #removing na's of drive and down, very small percentage

# new code - counts all yards not just increments of 10
rush <- all %>% 
  dplyr::filter(play_type == 'run') %>%
  dplyr::mutate(score = 6 * touchdown + yards_gained/10 + -2 * fumble,
                temp = ifelse(is.na(temp), median(temp, na.rm = T), temp),
                crosswind = ifelse(is.na(crosswind), 0, abs(crosswind)),
                toward_home_wind = ifelse(is.na(toward_home_wind), 0, abs(toward_home_wind)))%>%
  dplyr::filter(!is.na(drive)&!is.na(down)) 


pass <- all %>%
  dplyr::filter(play_type == 'pass') %>%
  dplyr::mutate(score = 6 * touchdown + yards_gained/10 + -2 * fumble,
                air_yards = ifelse(is.na(air_yards), 0, air_yards),
                temp = ifelse(is.na(temp), median(temp, na.rm = T), temp),
                crosswind = ifelse(is.na(crosswind), 0, abs(crosswind)),
                toward_home_wind = ifelse(is.na(toward_home_wind), 0, abs(toward_home_wind)))%>%
  dplyr::filter(!is.na(drive)&!is.na(down))

# keep only predictors and score
rush <- rush[,c("week", "yardline_100", "game_seconds_remaining", "drive", "qtr", "down", 
                "goal_to_go", "ydstogo", "shotgun", "no_huddle", "run_gap", "posteam_score",
                "defteam_score", "weather_type", "temp", "crosswind", "toward_home_wind", "score", "season",
                "def_rush_td_ema", "def_rush_yd_ema")]# removed "air_yards"

pass <- pass[,c("week", "yardline_100", "game_seconds_remaining", "drive", "qtr", "down", 
                "goal_to_go", "ydstogo", "shotgun", "no_huddle", "air_yards", "posteam_score", #evaluate air_yards
                "defteam_score", "weather_type", "temp", "crosswind", "toward_home_wind","score", "season",
                "def_rec_td_ema", "def_rec_yd_ema")]# removed "run_gap"

# designate weather_type and run_gap as factors

rush$run_gap <- as.factor(rush$run_gap)
rush$weather_type <- as.factor(rush$weather_type)
pass$weather_type <- as.factor(pass$weather_type)

summary(rush)
##################
#split between train and test here by 2015-2019 and 2020
##################

rush_train_1 <- rush[rush$season<2020,]
rush_train_1 <- subset(rush_train_1, select=-season)
rush_test_1 <- rush[rush$season==2020,]
rush_test_1 <- subset(rush_test_1, select=-season)
pass_train_1 <- pass[pass$season<2020,]
pass_train_1 <- subset(pass_train_1, select=-season)
pass_test_1 <- pass[pass$season==2020,]
pass_test_1 <- subset(pass_test_1, select=-season)


# create training and validation data 
# # validation is different from test
# # validation is used for short term checking during
# # model optimization phase
rush_valid_split_1 <- initial_split(rush_train_1, .8)
pass_valid_split_1 <- initial_split(pass_train_1, .8)

# training data
rush_train_2 <- analysis(rush_valid_split_1)
pass_train_2 <- analysis(pass_valid_split_1)
# validation data
rush_valid <- assessment(rush_valid_split_1)
pass_valid <- assessment(pass_valid_split_1)
rush_x_test <- rush_valid[setdiff(names(rush_valid), "score")]
rush_y_test <- rush_valid$score
pass_x_test <- pass_valid[setdiff(names(pass_valid), "score")]
pass_y_test <- pass_valid$score


# ranger rf models
# library(ranger)
rushmodel2 <- ranger(
  x = rush_train_2[setdiff(names(rush_train_2), "score")],
  y = rush_train_2$score,
  num.trees = 200
)

passmodel2 <- ranger(
  x = pass_train_2[setdiff(names(pass_train_2), "score")],
  y = pass_train_2$score,
  num.trees = 200
)

# generic ranger prediction error
rushmodel2$prediction.error#1.554758
passmodel2$prediction.error#2.612927

#rm(list= ls()[!(ls() %in% c('all','otherstuffhere'))])

# start up h2o 
h2o.init(max_mem_size = "5g")

# clear h2o files
h2o.rm
# restart h2o
h2o.shutdown()
h2o.init(max_mem_size = "5g")


# create feature names
y <- "score"
rush_x <- setdiff(names(rush_train_2), y)
pass_x <- setdiff(names(pass_train_2), y)

# turn training set into h2o object
rush_train.h2o <- as.h2o(rush_train_2)
pass_train.h2o <- as.h2o(pass_train_2)
rush_valid.h2o <- as.h2o(rush_valid)
pass_valid.h2o <- as.h2o(pass_valid)


# hyperparameter grid
old_hyper_grid.h2o <- list(
  ntrees      = seq(200, 500, by = 150),
  mtries      = seq(2, 12, by = 1),
  max_depth   = seq(5, 30, by = 5),
  min_rows    = seq(2, 20, by = 2),
  nbins       = seq(10, 30, by = 5)
)

hyper_grid.h2o <- list(
  ntrees      = seq(200, 500, by = 150),
  mtries      = seq(5, 15, by = 1),
  max_depth   = seq(5, 50, by = 5),
  min_rows    = seq(12, 40, by = 2),
  nbins       = seq(10, 50, by = 5)
)

# random grid search criteria
search_criteria <- list(
  strategy = "RandomDiscrete",
  stopping_metric = "mse",
  stopping_tolerance = 0.002,#decreased from 0.005 to 0.002
  stopping_rounds = 10,
  max_runtime_secs = 20*60#20 minutes
)


# build grid search 
rush_random_grid <- h2o.grid(
  algorithm = "randomForest",
  grid_id = "rush_grid",
  x = rush_x, 
  y = y, 
  training_frame = rush_train.h2o,
  hyper_params = hyper_grid.h2o,
  search_criteria = search_criteria
)
rush_random_grid

pass_random_grid <- h2o.grid(
  algorithm = "randomForest",
  grid_id = "pass_grid",
  x = pass_x, 
  y = y, 
  training_frame = pass_train.h2o,
  hyper_params = hyper_grid.h2o,
  search_criteria = search_criteria
)
pass_random_grid


# collect the results and sort by our model performance metric of choice
rush_grid_perf2 <- h2o.getGrid(
  grid_id = "rush_grid", 
  sort_by = "mse", 
  decreasing = FALSE
)
print(rush_grid_perf2)

pass_grid_perf2 <- h2o.getGrid(
  grid_id = "pass_grid", 
  sort_by = "mse", 
  decreasing = FALSE
)
print(pass_grid_perf2)

# rm(model)
# how consistent is the pass model?

pass_consistency <- data.frame(mse = rep(NA, 50))

for(i in 1:50) {
  i
  model <- h2o.randomForest(
    x = pass_x, 
    y = y, 
    training_frame = pass_train.h2o,
    mtries = 10,
    ntrees = 500,
    max_depth = 10,
    min_rows = 34,
    nbins = 20
  )
  predict <- as.data.frame(h2o.predict(model, pass_valid.h2o))
  pass_consistency$mse[i] <- mean((predict$predict-pass_valid$score)**2)
}

mean(pass_consistency$mse)#2.366661
sd(pass_consistency$mse)#0.0009239712
max(pass_consistency$mse)-min(pass_consistency$mse)#0.003961422

hist(pass_consistency$mse)

# how consistent is the rush model?
rm(model)
rush_consistency <- data.frame(mse = rep(NA, 50))

for(i in 1:50) {
  i
  model <- h2o.randomForest(
    x = rush_x, 
    y = y, 
    training_frame = rush_train.h2o,
    mtries = 12,
    ntrees = 350,
    max_depth = 5,
    min_rows = 18,
    nbins = 30
  )
  predict <- as.data.frame(h2o.predict(model, rush_valid.h2o))
  rush_consistency$mse[i] <- mean((predict$predict-rush_valid$score)**2)
}

mean(rush_consistency$mse)
sd(rush_consistency$mse)
max(rush_consistency$mse)-min(rush_consistency$mse)

hist(rush_consistency$mse)

# single models

# RUSH
rush_model <- h2o.randomForest(
  x = rush_x, 
  y = y, 
  training_frame = rush_train.h2o,
  mtries = 10,
  ntrees = 500,
  max_depth = 10,
  min_rows = 34,
  nbins = 20
)

# RUSH VARIABLE IMPORTANCE
h2o.varimp_plot(rush_model)
h2o.varimp(rush_model)

# RUSH PREDICTED MSE
rush_predict <- as.data.frame(h2o.predict(rush_model, rush_valid.h2o))
mean((rush_predict$predict-rush_valid$score)**2)

# PASS
pass_model <- h2o.randomForest(
  x = pass_x, 
  y = y, 
  training_frame = pass_train.h2o,
  mtries = 13,
  ntrees = 350,
  max_depth = 15,
  min_rows = 36,
  nbins = 10
)

# variable importance
h2o.varimp_plot(pass_model)
h2o.varimp(pass_model)

# Prediction MSE
pass_predict <- as.data.frame(h2o.predict(pass_model, pass_valid.h2o))
mean((pass_predict$predict-pass_valid$score)**2)

# predicting test data
rush_test.h2o <- as.h2o(rush_test_1)
pass_test.h2o <- as.h2o(pass_test_1)

pass_test_predict <- as.data.frame(h2o.predict(pass_model, pass_test.h2o))
mean((pass_test_predict$predict-pass_test_1$score)**2)
rush_test_predict <- as.data.frame(h2o.predict(rush_model,rush_test.h2o))
mean((rush_test_predict$predict-rush_test_1$score)**2)

# RMSE of best model
h2o.mse(rush_h20_best_perf) %>% sqrt()
h2o.mse(pass_h20_best_perf) %>% sqrt()

pass_test_1$predict <- pass_test_predict$predict
rush_test_1$predict <- rush_test_predict$predict

# export results of test
write_xlsx(pass_test_1,"C:/Users/conno/OneDrive/Desktop/STAT 8940 - Applied Analysis Project/Data/pass_test_results.xlsx")
write_xlsx(rush_test_1,"C:/Users/conno/OneDrive/Desktop/STAT 8940 - Applied Analysis Project/Data/rush_test_results.xlsx")

# MODEL TEST PREDICTION ANALYSIS

# RUSH
# create rush test dataset with all columns for analysis
rush_2020 <- all %>% 
  dplyr::filter(play_type == 'run' & season == 2020) %>%
  dplyr::mutate(score = 6 * touchdown + yards_gained/10 + -2 * fumble,
                temp = ifelse(is.na(temp), median(temp, na.rm = T), temp),
                crosswind = ifelse(is.na(crosswind), 0, abs(crosswind)),
                toward_home_wind = ifelse(is.na(toward_home_wind), 0, abs(toward_home_wind)))%>%
  dplyr::filter(!is.na(drive)&!is.na(down)) 

# test prediction dataset
rush_2020_predict <- rush_2020[,c("week", "yardline_100", "game_seconds_remaining", "drive", "qtr", "down", 
                                  "goal_to_go", "ydstogo", "shotgun", "no_huddle", "run_gap", "posteam_score",
                                  "defteam_score", "weather_type", "temp", "crosswind", "toward_home_wind", "score", "season",
                                  "def_rush_td_ema", "def_rush_yd_ema")]

rush_2020_predict.h2o <- as.h2o(rush_2020_predict)

# predict test data
rush_test_predict2 <- as.data.frame(h2o.predict(rush_model,rush_2020_predict.h2o))
mean((rush_test_predict2$predict-rush_2020_predict$score)**2)

# append predictions and calculate residuals
rush_2020$predict <- rush_test_predict2$predict
rush_2020$residual <- rush_2020$score - rush_2020$predict

## testing analysis framework
# test <- rush_2020 %>% dplyr::filter(game_id == "2020_01_ARI_SF")
# test2 <- test %>% 
#   dplyr::select(posteam, rusher_player_id, score, predict, rusher_player_name, residual) %>%
#   dplyr::group_by(posteam, rusher_player_id, rusher_player_name) %>%
#   dplyr::summarise(sum_actual = sum(score),
#                    sum_predict = sum(predict), 
#                    sum_residual = sum(residual))
# test2

# read in 2020 roster
roster_2020 <- read_csv("C:/Users/conno/OneDrive/Desktop/STAT 8940 - Applied Analysis Project/Data/roster_2020.csv")
# roster_2020

# export rush to excel, ## NOT NEEDED, CAN LEFT JOIN ON "gris_id" AND "position" ##
write_xlsx(rush_2020,"C:/Users/conno/OneDrive/Desktop/STAT 8940 - Applied Analysis Project/Data/rush_test_results_05_07_21.xlsx")
rush_2020_2 <- read_csv("C:/Users/conno/OneDrive/Desktop/STAT 8940 - Applied Analysis Project/Data/rush_test_results_05_07_21_position.csv")

# append position and exclude all but running backs
rush_2020$rusher_position <- rush_2020_2$rusher_position
rush_2020_3 <- rush_2020 %>% dplyr::filter(rusher_position == "RB")

# Test MSE predicted after removal of non RB
mean(abs(rush_2020_3$residual)**2)
plot(density(rush_2020_3$residual))
library(nortest)
ad.test(rush_2020_3$residual)#not normal
library(ggpubr)
ggqqplot(rush_2020_3$residual)#qqplot very non-linear

# aggregate scores/predicted scores/residuals by player and week, sort by week and team
rush_2020_4 <- rush_2020_3 %>% 
  dplyr::select(posteam, rusher_player_id, score, predict, rusher_player_name, residual, week, def_rush_yd_ema, def_rush_td_ema, weather_type) %>%
  dplyr::group_by(posteam, rusher_player_id, rusher_player_name, week, def_rush_yd_ema, def_rush_td_ema, weather_type) %>%
  dplyr::summarise(sum_actual = sum(score),
                   sum_predict = sum(predict), 
                   sum_residual = sum(residual)) %>%
  dplyr::arrange(week, posteam)

rush_2020_4

sum(rush_2020_4$sum_actual)#total sum score
sum(abs(rush_2020_4$sum_residual))# total absolute error by team


# distribution of residuals
summary(rush_2020_4$sum_residual)
plot(density(rush_2020_4$sum_residual))

ad.test(rush_2020_4$sum_residual)#not normal

ggqqplot(rush_2020_4$sum_residual)#qqplot very non-linear

#best and worst weekly player performances by residuals
rush_2020_4_2 <- rush_2020_4 %>% dplyr::arrange(sum_residual)
rush_2020_4_2
tail(rush_2020_4_2)

weeklymse <- rush_2020_3 %>% dplyr::group_by(week) %>% dplyr::summarise(MSE = mean(residual**2))
barplot(weeklymse$MSE, by = weeklymse$week)

# TEAM SEASON AGGREGATION
rush_2020_5 <- rush_2020_4 %>% 
  dplyr::select(posteam, sum_actual, sum_predict, sum_residual) %>%
  dplyr::group_by(posteam) %>%
  dplyr::summarise(total_sum_actual = sum(sum_actual),
                   total_sum_predict = sum(sum_predict), 
                   total_sum_residual = sum(sum_residual)) %>%
  dplyr::arrange(total_sum_residual)

rush_2020_5#worst teams
tail(rush_2020_5)#best teams
sum(rush_2020_5$total_sum_actual)#total sum score
sum(abs(rush_2020_5$total_sum_residual))# total absolute error by team

# PLAYER SEASON AGGREGATION
rush_2020_6 <- rush_2020_4 %>% 
  dplyr::select(rusher_player_id, rusher_player_name, sum_actual, sum_predict, sum_residual) %>%
  dplyr::group_by(rusher_player_id, rusher_player_name) %>%
  dplyr::summarise(total_sum_actual = sum(sum_actual),
                   total_sum_predict = sum(sum_predict), 
                   total_sum_residual = sum(sum_residual)) %>%
  dplyr::arrange(total_sum_residual)

rush_2020_6#worst players
tail(rush_2020_6)#best players
sum(rush_2020_6$total_sum_actual)#total sum score
sum(abs(rush_2020_6$total_sum_residual))#total absolute error by player


# PASS
# create pass test dataset with all columns for analysis
pass_2020 <- all %>% 
  dplyr::filter(play_type == 'pass' & season == 2020) %>%
  dplyr::mutate(score = 6 * touchdown + yards_gained/10 + -2 * fumble,
                temp = ifelse(is.na(temp), median(temp, na.rm = T), temp),
                crosswind = ifelse(is.na(crosswind), 0, abs(crosswind)),
                toward_home_wind = ifelse(is.na(toward_home_wind), 0, abs(toward_home_wind)))%>%
  dplyr::filter(!is.na(drive)&!is.na(down)) 

# test prediction dataset
pass_2020_predict <- pass_2020[,c("week", "yardline_100", "game_seconds_remaining", "drive", "qtr", "down", 
                                  "goal_to_go", "ydstogo", "shotgun", "no_huddle", "air_yards", "posteam_score", 
                                  "defteam_score", "weather_type", "temp", "crosswind", "toward_home_wind","score", "season",
                                  "def_rec_td_ema", "def_rec_yd_ema")]

pass_2020_predict.h2o <- as.h2o(pass_2020_predict)

# predict test data
pass_test_predict2 <- as.data.frame(h2o.predict(pass_model,pass_2020_predict.h2o))
mean((pass_test_predict2$predict-pass_2020_predict$score)**2)

# append predictions and calculate residuals
pass_2020$predict <- pass_test_predict2$predict
pass_2020$residual <- pass_2020$score - pass_2020$predict

#embarrassing code for bringing over position
roster_2020_2 <- roster_2020[,c("position", "gsis_id")]
roster_2020_receiver <- roster_2020_2
roster_2020_receiver$receiver_player_id <- roster_2020_2$gsis_id
roster_2020_receiver <- roster_2020_receiver %>%
  dplyr::select(receiver_player_id, position)

pass_2020_2 <- pass_2020 %>%
  dplyr::left_join(roster_2020_receiver) %>%
  dplyr::filter(!is.na(receiver_player_id)) %>%
  dplyr::rename(receiver_position = position)

#exclude all but wide receivers
pass_2020_3 <- pass_2020_2 %>% dplyr::filter(receiver_position == "WR")
as.data.frame(table(pass_2020_2$receiver_position))

# Test MSE predicted after removal of non WR
mean(pass_2020_3$residual**2)

plot(density(pass_2020_3$residual))

passdensity <- density(pass_2020_3$residual)
#density is bimodal!

passdensity$x[which.max(passdensity$y)]

passdensity$x

x <- as.data.frame(passdensity$x)
y <- as.data.frame(passdensity$y)
xy <- x %>% dplyr::bind_cols(y)
xy
negxy <- xy[which(xy$`passdensity$x`< -0.4),] 
negxy$`passdensity$x`[which.max(negxy$`passdensity$y`)]
ggqqplot(pass_2020_3$residual)


# aggregate scores/predicted scores/residuals by player and week, sort by week and team
pass_2020_4 <- pass_2020_3 %>% 
  dplyr::select(posteam, receiver_player_id, score, predict, receiver_player_name, residual, week, def_rec_yd_ema, def_rec_td_ema, weather_type) %>%
  dplyr::group_by(posteam, receiver_player_id, receiver_player_name, week, def_rec_yd_ema, def_rec_td_ema, weather_type) %>%
  dplyr::summarise(sum_actual = sum(score),
                   sum_predict = sum(predict), 
                   sum_residual = sum(residual)) %>%
  dplyr::arrange(week, posteam)

pass_2020_4

# distribution of residuals
summary(pass_2020_4$sum_residual)
plot(density(pass_2020_4$sum_residual))
library(nortest)
ad.test(pass_2020_4$sum_residual)#not normal
library(ggpubr)
ggqqplot(pass_2020_4$sum_residual)#qqplot very non-linear

pass_2020_4_2 <- pass_2020_4 %>% dplyr::arrange(sum_residual)
pass_2020_4_2
tail(pass_2020_4_2)

sum(pass_2020_4$sum_actual)#total sum score
sum(abs(pass_2020_4$sum_residual))# total absolute error by team

# TEAM SEASON AGGREGATION
pass_2020_5 <- pass_2020_4 %>% 
  dplyr::select(posteam, sum_actual, sum_predict, sum_residual) %>%
  dplyr::group_by(posteam) %>%
  dplyr::summarise(total_sum_actual = sum(sum_actual),
                   total_sum_predict = sum(sum_predict), 
                   total_sum_residual = sum(sum_residual)) %>%
  dplyr::arrange(total_sum_residual)

pass_2020_5#worst teams
tail(pass_2020_5)#best teams
sum(pass_2020_5$total_sum_actual)#total sum score
sum(abs(pass_2020_5$total_sum_residual))# total absolute error by team

# PLAYER SEASON AGGREGATION
pass_2020_6 <- pass_2020_4 %>% 
  dplyr::select(receiver_player_id, receiver_player_name, sum_actual, sum_predict, sum_residual) %>%
  dplyr::group_by(receiver_player_id, receiver_player_name) %>%
  dplyr::summarise(total_sum_actual = sum(sum_actual),
                   total_sum_predict = sum(sum_predict), 
                   total_sum_residual = sum(sum_residual)) %>%
  dplyr::arrange(total_sum_residual)

pass_2020_6#worst players
tail(pass_2020_6)#best players
sum(pass_2020_6$total_sum_actual)#total sum score
sum(abs(pass_2020_6$total_sum_residual))#total absolute error by player

#COMPUTATION TIME COMPARISON

#create smaller training data to demonstrate randomForest, it is taking way too long.
pass_train_3_split <- initial_split(pass_train_2, .1)
pass_train_3 <- analysis(pass_train_3_split)

  
# randomForest
start_time <- Sys.time()
RFMODEL <- randomForest(
  x = pass_train_3[,pass_x],
  y = pass_train_3$score
)
end_time <- Sys.time()
end_time - start_time

# randomForest memory
(object.size(RFMODEL)/1024)/1024

# randomForest optimum mse
min(RFMODEL$mse)

# ranger
start_time <- Sys.time()
RANGERMODEL <- ranger(
  x = pass_train_2[,pass_x],
  y = pass_train_2$score
)
end_time <- Sys.time()
end_time - start_time

# ranger memory
(object.size(RANGERMODEL)/1024)/1024

# ranger prediction mse
RANGERMODEL$prediction.error

# h2o

h2o.shutdown()
h2o.init(max_mem_size = "5g")
pass_train_2.h2o <- as.h2o(pass_train_2)

start_time <- Sys.time()
H2OMODEL <- h2o.randomForest(
  x = pass_x, 
  y = y, 
  training_frame = pass_train_2.h2o,
)
end_time <- Sys.time()
end_time - start_time

H2OMODEL_2 <- h2o.getModel("DRF_model_R_1620428620974_3")
h2o.performance(model = H2OMODEL_2)

rm()
object.size(ls)




















