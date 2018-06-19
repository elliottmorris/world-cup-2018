# By G. Elliott Morris
# Simulate every game in the group stage before happens to assess accuracy
#
# setup 
rm(list=ls())
source("~/setup_elliott.R")
source("functions.R")
library(magrittr)
library(purrr)

# data ---------
# read in historical data
train_data <- read.csv("data/matches_history.csv",stringsAsFactors = F)

# need a var that is elo margin
train_data <- train_data %>%
  group_by(match_id) %>%
  mutate(elo.opp = rev(elo)) %>%
  ungroup() %>%
  mutate(elo.adv = elo - elo.opp)

# read in data for 2018 team_data
team_data <- read.csv("data/team_data_2018.csv",stringsAsFactors = F)
group_stage_2018 <- read.csv("data/group_stage_2018.csv",stringsAsFactors = F)

# update data for 2018 starting point (prev file made on May 23)
elo_start <- read.csv("data/elo_clean/elo_2018_start_clean.csv",stringsAsFactors = F) %>% 
  dplyr::rename("name" = team) %>% 
  arrange(name)

team_data <- team_data %>% 
  arrange(name) %>% 
  select(-elo) %>%
  left_join(elo_start)

# bind group letters to group stage data
group_stage_2018 <- group_stage_2018 %>% 
  left_join(team_data %>% select(name,group) %>% dplyr::rename(team1=name))

# games already been played?
#source("")

# update elo again
##

# train the poisson model and save lambdas to team_data --------
# training data

# train model
mod <- glm(goals ~ elo, family = poisson(link = "log"), data = train_data)

summary(mod)
loocv(mod)

# spit out predicted lamba for 2018 cup
team_data$lambda <- NA
team_data$lambda <- predict(mod, 
                            newdata = select(team_data,elo), 
                            type = "response")

head(arrange(team_data,desc(lambda)))


# matches ------------
match <- get_matchup_stats(team_data,
                  teama_in = "Sweden",
                  teamb_in = "South Korea",
                  nmatches_in = 500,
                  musthavewinner_in = F) 

for(row in 1:nrow(group_stage_2018)){
  temp <- group_stage_2018[row,]
  
  get_matchup_stats(team_data,
                    teama_in=temp$team1,
                    teamb_in=temp$team2,
                    nmatches_in=1000,
                    musthavewinner_in = F) 
}
