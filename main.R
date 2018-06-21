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

number_of_sims <- 10000 # num of sims
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

 
# play the group stage ---------
# you have to do it in thirds
group_sim.1 <- sim_group_stage_2018(teams = team_data,
                                  group_data = subset(group_stage_2018,stage==1),
                                  use_real_results = F)


# who won the group stage?  
group_sim_results <- find_group_winners(team_data,group_sim.1)


# find winners of the knockout round -----------
eight_matches <- data.frame(team1 = group_sim_results$goals$number[seq(1, 32, by=4)], 
                       team2 = group_sim_results$goals$number[c(6, 2, 14, 10, 22, 18, 30, 26)])

data.frame("t1" = team_data[match(eight_matches$team1,team_data$number),]$name %>% as.character(),
           "t2" = team_data[match(eight_matches$team2,team_data$number),]$name %>% as.character())

ko <- find_knockout_winners(team_data,eight_matches)


# play the entire tournament once --------------
one_sim <- simulate_one(team_data,group_stage_2018,use_real_results=use_results)

data.frame("rank" = 1:32,
           "number" = one_sim$final_ranking,
           "team" = team_data[match(one_sim$final_ranking,team_data$number),]$name) %>% head(10)


# simulate many times ---------
est_time <- ((number_of_sims/100)*17)/60 
print(sprintf("estimated time to finish is: %sm  |  eta: %s",
              round(est_time,1),
              format.Date(Sys.time() + est_time*60,"%I:%M %p")))

# run! 
system.time( # 17 seconds per hundred
  world_cup <- simulate_tournament_par(nsim=number_of_sims,
                                   teams = team_data,
                                   group_matches = group_stage_2018,
                                   use_real_results = use_results)
)

# save as rds just for re-use
saveRDS(world_cup,file="output/world_cup_sim.RDS")

# who is da winner?? -------------
# most number of goals:
goals_scored <- get_top_scorer(nsim=number_of_sims,
                               result_data = world_cup)

# probability of winning the tournament:
prob_win <- get_probability(world_cup)

prob_win%>% print()

# probability of making it to x round
prob_round <- world_cup %>% 
  map_df("round_made") %>% 
  count(round,team) %>%
  group_by(round) %>%
  mutate(prob = round(n/number_of_sims*100)) %>%
  as.data.frame()

prob_round <- prob_round%>%
  mutate(name = team_data[team,]$name) %>%
  arrange(round,desc(prob)) %>%
  select(-team) %>%
  dplyr::rename("team" = name)



# graphics and saving ---------
# top scorers
score.gg <- ggplot(goals_scored,aes(y=reorder(team,goals),x=goals)) +
  geom_lollipop(horizontal = TRUE) 

score.gg

# probability
prob.gg <- ggplot(filter(prob_win,prob>=0.01),aes(y=reorder(team,prob),x=prob,col=team)) +
  geom_lollipop(horizontal = TRUE,size=1.1) +
  geom_label(aes(x=prob+0.005,label=paste0(round(prob*100),"%"))) +
  scale_x_continuous(breaks=seq(0,1,0.05),
                     labels=seq(0,1,0.05)*100) + 
  labs(title = "Most Likely Winner of the 2018 FIFA World Cup",
       subtitle = "Forecasts generated from simulating the outcome of the international tournament 10,000 times.",
       x = "Probability (%)",
       y = "Team") +
  theme_elliott() +
  scale_color_teams

prob.gg

# round probability 
prob_round$round = factor(prob_round$round,
                          c("Make Round of 16",
                             "Make Quarters",
                             "Make Semis",
                             "Make Final",
                             "Win Final"))
prob_round <- prob_round %>%
  arrange(round,desc(prob)) %>%
  mutate(prob_order = ifelse(round!="Win Final",0,prob))

roundprob.gg <- ggplot(prob_round,aes(y=round,x=prob,col=team)) +
  geom_lollipop(horizontal = TRUE,size=1.1) +
  geom_label(aes(x=prob+10,label=paste0(case_when(prob<1~"<1",
                                                  prob>99~">99",
                                                  TRUE ~ as.character(prob)),"%"))) +
  facet_wrap(~team) +
  scale_x_continuous(breaks=seq(0,100,20),
                     labels=seq(0,100,20)) + 
  labs(title = "2018 FIFA World Cup: How Far Will Your Team Make It?",
       subtitle = "Forecasts generated from simulating the outcome of the international tournament 10,000 times.",
       x = "Probability (%)",
       y = "Stage") +
  theme_elliott()  +
  coord_cartesian(xlim=c(0,110)) +
  scale_color_teams

roundprob.gg

# saveing!
if(!isTRUE(use_results)){
  # probability file
  write.csv(prob_win,"output/probs/2018-06-13-probability.csv",row.names = F)
  
  # probability graph
  plot_elliott(prob.gg,"output/graphics/2018-06-13-probability.png",700,1200,unit='px',res=170)
  
  # round probability graph
  plot_elliott(roundprob.gg,"output/graphics/2018-06-13-team-round-probability.png",1600,2000,unit='px',res=180)
  
}else{
  # probability file
  write.csv(prob_win,
            sprintf("output/probs/%s-probability-PM.csv",RUN_DATE),
            row.names = F)
  
  # probability graph
  plot_elliott(prob.gg,
               sprintf("output/graphics/%s-probability-PM.png",RUN_DATE),
               700,1200,unit='px',res=170)
  
  # round probability graph
  plot_elliott(roundprob.gg,
               sprintf("output/graphics/%s-team-round-probability-PM.png",RUN_DATE),
               1600,2000,unit='px',res=180)
}

 # probability over time graph
sims <- vector('list',length(dir("output/probs/")))

for(i in 1:length(dir("output/probs/"))){
  filename <- dir("output/probs")[[i]]
  sims[[i]] <- read.csv(paste0("output/probs/",filename)) %>%
    mutate(date = substr(filename,1,10))
}

sims <- do.call('rbind',sims)

leaders <- head(prob_win,10) %>% pull(team) %>% as.character()

overtime.gg <- ggplot(filter(sims,team %in% leaders),
                      aes(x=ymd(date),y=prob,colour=team)) +
  geom_line(size=0.7) +
  geom_label_repel(data=filter(sims,date==max(date),team %in% leaders),
                   aes(label=paste0(team,": ",round(prob*100),'%')),
                   direction = "y",show.legend = F,
                   nudge_x = as.numeric(ymd(max(sims$date))-ymd(min(sims$date)))/4,
                   segment.colour = "gray60")+
  coord_cartesian(xlim=c(ymd("2018-06-13"),
                         ymd(Sys.Date() +
                               as.numeric(ymd(max(sims$date))-ymd(min(sims$date)))/4))) +
  scale_y_continuous(breaks=seq(0,1,0.1),
                     labels=seq(0,1,0.1)*100) + 
  scale_color_teams +
  labs(title = "2018 FIFA World Cup Chances Over Time",
       subtitle = "How win pobabilities for the 2018 FIFA World Cup have changed since the start of the tournament\n(among today's top ten teams).",
       x = "Date",
       y = "Probability (%)")

overtime.gg

plot_elliott(overtime.gg,
             "output/graphics/probability_overtime.png",
             700,1200,unit='px',res=170)

#}# endloop ------


