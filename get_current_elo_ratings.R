# file to get current elo of all teams after matches played
#
# NOTE: You have to go by this file line-by-line, a source won't work if no teams have played in a given stage of the group round
#

# By G. Elliott Morris
# Simulate the 2018 FIFA World Cup 
#
# setup 
rm(list=ls())
source("~/setup_elliott.R")
source("functions.R")
library(magrittr)
library(purrr)

clean_elo <- F # whether or not to run the elo cleaning file (usually F)

use_results <- T # whether or not to use real results when predicting outcome

#RUN_DATE <- "2018-06-15" 
RUN_DATE <- Sys.Date() 

#for(DATE in as.character(seq.Date(ymd("2018-06-18"),ymd("2018-06-19"),"day"))){RUN_DATE <- DATE

print(sprintf("RUNNING FOR: %s",RUN_DATE))

# data ---------
# clean up some data 
if(isTRUE(clean_elo)){source("clean_elo_data.R")}

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
  left_join(elo_start) %>%
  arrange(number) %>%
  mutate(points = 0)

# bind group letters to group stage data
group_stage_2018 <- group_stage_2018 %>% 
  left_join(team_data %>% select(name,group) %>% dplyr::rename(team1=name))

# all group stage games occuring after the run date need to be set to NA
group_stage_2018[mdy(group_stage_2018$date)>ymd(RUN_DATE),]$played <- 0

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

# simulate one game ------------
play_fun_double_poisson(team_data,"Spain","Uruguay",musthavewinner = F)

one_fun_match <- lapply(vector('list',500),
                        FUN=function(x){return(play_fun_double_poisson(team_data,"Brazil","Switzerland"))}) %>%
  do.call('rbind',.)

mean(one_fun_match$Agoals)
mean(one_fun_match$Bgoals)
mean(one_fun_match$Agoals-one_fun_match$Bgoals)

ggplot(one_fun_match) + 
  geom_bar(aes(x=Agoals,col="A"),fill=NA) +
  geom_bar(aes(x=Bgoals,col="B"),fill=NA) +
  scale_x_continuous(breaks=seq(0,10,1))

one_fun_match %>% filter(Agoals>Bgoals) %>% nrow() / nrow(one_fun_match)
one_fun_match %>% filter(Agoals==Bgoals) %>% nrow() / nrow(one_fun_match)
one_fun_match %>% filter(Agoals<Bgoals) %>% nrow() / nrow(one_fun_match)


# play the group stage(s) -------
teams_remaining <- function(teams,group_data,stage_number){
  groups <- group_data %>% filter(stage == stage_number & played ==1)
  groups <- c(groups$team1,groups$team2)
  return(teams[teams$name %in% groups,])
}

# start
teams <- team_data
group_matches <- subset(group_stage_2018,played==1)
use_real_results<-T

## Step 0. Simulate the group matches
teams <- teams_remaining(teams,group_matches,stage_number = 1)

group_matches.1 <- sim_group_stage_2018(teams = teams,
                                        group_data = subset(group_matches,stage==1),
                                        use_real_results = use_real_results)

### find the winners
group_results.1 <- find_group_winners(teams = teams, 
                                      group_match_data = group_matches.1)

### update elo 
group_results.1$goals <- group_results.1$goals %>%
  mutate(new_elo = update_elo_hot(elo = elo,
                                  elo_opp = elo_opp,
                                  win_tri = case_when(goalsDifference>0 ~ 1,
                                                      goalsDifference==0~0.5,
                                                      goalsDifference<0 ~ 0),
                                  goal_dif = goalsDifference,
                                  k_constant = 60))

teams[order(teams$number),]$elo <- group_results.1$goals[order(group_results.1$goals$number),]$new_elo

teams$lambda <- predict(mod,   # get new lambda with new elo
                        newdata = select(teams,elo), 
                        type = "response")

### points 
teams$points.1 <- NA
teams[order(teams$number),]$points.1 <- group_results.1$goals[order(group_results.1$goals$number),]$points

## repeat for parts 2 ----------
teams.2 <- teams_remaining(teams,group_matches,stage_number = 2)

group_matches.2 <- sim_group_stage_2018(teams = teams.2,
                                        group_data = subset(group_matches,stage==2),
                                        use_real_results = use_real_results)

group_matches.2

group_results.2 <- find_group_winners(teams = teams.2, 
                                      group_match_data = group_matches.2)


# filter to only the matches played
group_results.2$goals <- group_results.2$goals %>%
  mutate(new_elo = update_elo_hot(elo = elo,
                                  elo_opp = elo_opp,
                                  win_tri = case_when(goalsDifference>0 ~ 1,
                                                      goalsDifference==0~0.5,
                                                      goalsDifference<0 ~ 0),
                                  goal_dif = goalsDifference,
                                  k_constant = 60))

teams.2[order(teams.2$number),]$elo <- group_results.2$goals[order(group_results.2$goals$number),]$new_elo

teams.2$lambda <- predict(mod,   # get new lambda with new elo
                        newdata = select(teams.2,elo), 
                        type = "response")

### points 
teams.2$points.2 <- NA
teams.2[order(teams.2$number),]$points.2 <- group_results.2$goals[order(group_results.2$goals$number),]$points

# append team 1
teams.2 <- teams.2 %>% bind_rows(teams %>% filter(!name %in% teams.2$name))

### finally, part 3 --------
teams <- teams_remaining(teams,group_matches,stage_number = 3)
group_matches.3 <- sim_group_stage_2018(teams = teams,
                                        group_data = subset(group_matches,stage==3),
                                        use_real_results = use_real_results)
group_matches.3
group_results.3 <- find_group_winners(teams = teams, 
                                      group_match_data = group_matches.3)

group_results.3$goals <- group_results.3$goals %>%
  mutate(new_elo = update_elo_hot(elo = elo,
                                  elo_opp = elo_opp,
                                  win_tri = case_when(goalsDifference>0 ~ 1,
                                                      goalsDifference==0~0.5,
                                                      goalsDifference<0 ~ 0),
                                  goal_dif = goalsDifference,
                                  k_constant = 60))

teams[order(teams$number),]$elo <- group_results.3$goals[order(group_results.3$goals$number),]$new_elo

# get new lambda with new elo
teams$lambda <- predict(mod, 
                        newdata = select(teams,elo), 
                        type = "response")

### points 
teams$points.3 <- NA
teams[order(teams$number),]$points.3 <- group_results.3$goals[order(group_results.3$goals$number),]$points

teams[teams$group=="F",]

# final rankings -------
### get goals
group_goals <- group_results.1$goals %>%
  bind_rows(group_results.2$goals) %>%
  #bind_rows(group_results.3$goals) %>% 
  group_by(number) %>% summarise(group = unique(group),
                                 goals = sum(goalsFore),
                                 goalsFore = sum(goalsFore),
                                 goalsAgainst = sum(goalsAgainst)) %>%
  mutate(goalsDifference = goalsFore-goalsAgainst)

#### group rank
teams <- teams %>% left_join(group_goals,by=c('group','number'))

teams <- teams %>% 
  mutate(points = points.1 + points.2 ) %>% #+ points.3) %>%
  group_by(group) %>%
  arrange(desc(points),desc(goalsDifference),desc(goalsFore)) %>%
  mutate(group_rank = row_number())  %>%
  arrange(group, group_rank) %>% 
  as.data.frame()


View(teams)
