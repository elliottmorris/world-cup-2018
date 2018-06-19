# function for update elo scores for winners ---------
update_elo_hot <- function(elo,elo_opp,win_tri,goal_dif,k_constant){
  # formula from https://www.eloratings.net/about
  
  k_update <- k_constant + k_constant * case_when(abs(goal_dif) == 1 ~ 0,
                                                  abs(goal_dif) == 2 ~ 1/2,
                                                  abs(goal_dif) == 3 ~ 3/4,
                                                  abs(goal_dif) > 3 ~ 3/4 + (goal_dif-3)/8,
                                                  TRUE ~ 0)
  
  w_e <- 1 / (10^(-(elo-elo_opp)/400) + 1)
  
  updated_elo <- elo + k_update * (win_tri - w_e)
  
  return(round(updated_elo))
}

# function to simulate one match of play -------
play_fun_double_poisson <- function(team_data, team1, team2,musthavewinner=FALSE) {
  
  # get lambda for teams
  lambda1 <- team_data %>% filter(name==team1) %>% pull(lambda)
  lambda2 <- team_data %>% filter(name==team2) %>% pull(lambda)
  
  # predict team goals
  Agoals <- rpois(length(team1), lambda = lambda1) 
  Bgoals <- rpois(length(team2), lambda = lambda2)
  
  # slight bump to favored team, equal to difference in lambda put back in poisson
  A.adv <- lambda1 - lambda2
  extra_goal <- rpois(1,lambda = abs(A.adv))
  
  if(isTRUE(A.adv>0)){
     Agoals <- Agoals+extra_goal
  }else{
    Bgoals <- Bgoals+extra_goal
  }
  
  
  # adjust for must win
  if(isTRUE(musthavewinner)){
    while(isTRUE(Agoals == Bgoals)){
      # predict team goals
      Agoals <- Agoals + round(runif(1,0,1))
      Bgoals <- Bgoals + round(runif(1,0,1))
    }
  }
  
  # return result
  result <- data.frame("Agoals"=Agoals, "Bgoals"=Bgoals)
  
  return(result)
}

# function to simulate each round in the group stage---------
sim_group_stage_2018 <- function(teams,group_data,use_real_results = FALSE){
  group_stage_sim_once <- lapply(seq(1:nrow(group_data)),
                                 FUN=function(x){
                                   # play the game
                                   game <- play_fun_double_poisson(team_data = teams,
                                                                   team1 = group_data[x,]$team1,
                                                                   team2 = group_data[x,]$team2,
                                                                   musthavewinner = F)
                                   return(game)})
  
  group_stage_sim_once <- do.call('rbind',group_stage_sim_once)
  
  # append
  if(isTRUE(use_results)){
    group_data$goals1 <- ifelse(group_data$played!=1,
                                group_stage_sim_once$Agoals,
                                group_data$goals1)
    
    group_data$goals2 <- ifelse(group_data$played!=1,
                                group_stage_sim_once$Bgoals,
                                group_data$goals2)
  } else {
    group_data$goals1 <- group_stage_sim_once$Agoals
    group_data$goals2 <- group_stage_sim_once$Bgoals
  }
  
  # return
  return(group_data)
}

# function to figure out who won the group stage? ----------
find_group_winners <- function(teams, group_match_data) {
  teams$name <- as.character(teams$name)
  
  ## Create a copy of the matches that we can fill out
  #group_match_data <- group_stage_2018
  group_match_results <- group_match_data
  
  ## Okay the casing is a bit of a mess here. I do apologize.
  teams$goalsFore <- sapply(teams$name, function(i) { sum(group_match_results[c("goals1", "goals2")][i == group_match_data[c("team1","team2")]]) })
  
  teams$goalsAgainst <- sapply(teams$name, function(i) { sum(group_match_results[c("goals2", "goals1")][i == group_match_data[c("team1","team2")]]) })
  
  teams$elo_opp <- sapply(teams$name, 
                              function(i) { 
                                opp <- c(group_match_data[match(i,group_match_data$team2),]$team1,
                                group_match_data[match(i,group_match_data$team1),]$team2)
                                
                                opp <- opp[!is.na(opp)]
                                
                                teams[match(opp,teams$name),]$elo
                                })
  
  teams$goalsDifference <- teams$goalsFore-teams$goalsAgainst
  
  teams$points <- teams$points + case_when(teams$goalsDifference>0~3,
                                           teams$goalsDifference==0~1,
                                           teams$goalsDifference<0~0)
  
  return(list(goals = teams))
  
}

# function to find the winners of the top 16 knockout round ------------
find_knockout_winners <- function(teams, match_data) {
  
  # match names
  match_data$team1_name <- teams[match(match_data$team1,teams$number),]$name %>% as.character()
  
  match_data$team2_name <- teams[match(match_data$team2,teams$number),]$name %>% as.character()
  
  ## Get the results  
  results <- lapply(seq(1,nrow(match_data),1),
                    FUN = function(x){
                      play_fun_double_poisson(
                        team_data=teams,
                        team1 = match_data[x,]$team1_name, # is char 
                        team2 = match_data[x,]$team2_name, # is char
                        musthavewinner = TRUE)
                    }
  )
  
  results <- do.call('rbind',results)
  
  ## Now form the goals dataset
  goals <- data.frame(match_data, results) %>% 
    setNames(c(names(match_data), "goals1", "goals2")) %>%
    mutate(goalsDifference = goals1-goals2)
  
  
  teams$points <- 
    sapply(teams$name, function(i) { sum(goals[c("goals1", "goals2")][i == goals[c("team1_name","team2_name")]]) })
  
  teams$goalsFore <- sapply(teams$name, function(i) { sum(goals[c("goals1", "goals2")][i == goals[c("team1_name","team2_name")]]) })
  
  teams$goalsAgainst <- sapply(teams$name, function(i) { sum(goals[c("goals2", "goals1")][i == goals[c("team1_name","team2_name")]]) })
  
  teams$elo_opp <- sapply(teams$name, 
                          function(i) { 
                            opp <- c(goals[match(i,goals$team2_name),]$team1_name,
                                     goals[match(i,goals$team1_name),]$team2_name)
                            
                            opp <- ifelse(is.integer(opp[!is.na(opp)]),NA,opp[!is.na(opp)])
                            
                            teams[match(opp,teams$name),]$elo
                          })
  
  teams$goalsDifference <- teams$goalsFore-teams$goalsAgainst
  
  
  ## Find the teams that won
  winners <- match_data[cbind(seq(nrow(results)),
                              ifelse(results[, 1] > results[, 2], 1, 2))] %>% 
    stringr::str_trim() %>% 
    as.numeric()
  
  goals
  winners
  
  return(list(winners = winners, goals = goals,teams_updated =teams))
  
}

# a function to simulate the tournament once ---------
simulate_one <- function(teams,            
                         group_matches,
                         use_real_results) {
  
  
  teams <- team_data
  group_matches <- group_stage_2018
  use_real_results<-T
  
  ## Step 0. Simulate the group matches
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
  
  ## repeat for parts 2 and 3
  group_matches.2 <- sim_group_stage_2018(teams = teams,
                                          group_data = subset(group_matches,stage==2),
                                          use_real_results = use_real_results)
  
  group_matches.2
  
  group_results.2 <- find_group_winners(teams = teams, 
                                        group_match_data = group_matches.2)
  
  group_results.2$goals <- group_results.2$goals %>%
    mutate(new_elo = update_elo_hot(elo = elo,
                                    elo_opp = elo_opp,
                                    win_tri = case_when(goalsDifference>0 ~ 1,
                                                        goalsDifference==0~0.5,
                                                        goalsDifference<0 ~ 0),
                                    goal_dif = goalsDifference,
                                    k_constant = 60))
  
  teams[order(teams$number),]$elo <- group_results.2$goals[order(group_results.2$goals$number),]$new_elo
  
  teams$lambda <- predict(mod,   # get new lambda with new elo
                          newdata = select(teams,elo), 
                          type = "response")
  
  ### points 
  teams$points.2 <- NA
  teams[order(teams$number),]$points.2 <- group_results.2$goals[order(group_results.2$goals$number),]$points
  
  
  ### finally, part 3
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
  
  ## Step 1: Get the final ranking after group stage
  ### get goals
  group_goals <- group_results.1$goals %>%
    bind_rows(group_results.2$goals) %>%
    bind_rows(group_results.3$goals) %>% 
    group_by(number) %>% summarise(group = unique(group),
                                   goals = sum(goalsFore),
                                   goalsFore = sum(goalsFore),
                                   goalsAgainst = sum(goalsAgainst)) %>%
    mutate(goalsDifference = goalsFore-goalsAgainst)
  
  #### group rank
  teams <- teams %>% left_join(group_goals,by=c('group','number'))
  
  teams <- teams %>% 
    mutate(points = points.1 + points.2 + points.3) %>%
    group_by(group) %>%
    arrange(desc(points),desc(goalsDifference),desc(goalsFore)) %>%
    mutate(group_rank = row_number())  %>%
    arrange(group, group_rank) %>% 
    as.data.frame()
  
  
  ## Step 2: Design matches for the first part of the knockout match
  ## Select No.1 within group and No.2 in next group
  eight_matches <- data.frame(team1 = teams$number[seq(1, 32, by=4)], 
                              team2 = teams$number[c(6, 2, 14, 10, 22, 18, 30, 26)])
  
  data.frame("1" = teams[match(eight_matches$team1,teams$number),]$name %>% as.character(),
  "2" = teams[match(eight_matches$team2,teams$number),]$name %>% as.character())
  
  eight <- find_knockout_winners(teams=teams,match_data = eight_matches)
  
  teams[match(eight$winners,teams$number),]$name %>% as.character()
  
  ## and parse the results
  eight_winners <- teams[match(eight$winners,teams$number),]$name %>% as.character()
  
  eight_losers <- c(eight$goals[eight$goals$team1_name %in% eight_winners,]$team2_name,
                    eight$goals[eight$goals$team2_name %in% eight_winners,]$team1_name)
  
  eight_winner_goals <- c(eight$goals[eight$goals$team1_name %in% eight_winners,]$goals1,
                          eight$goals[eight$goals$team2_name %in% eight_winners,]$goals2)
  
  eight_loser_goals <- c(eight$goals[eight$goals$team1_name %in% eight_winners,]$goals2,
                         eight$goals[eight$goals$team2_name %in% eight_winners,]$goals1)
  
  ### 2.5: update elo 
  teams[teams$name %in% eight_winners,]$elo <- update_elo_hot(
    elo = eight$teams_updated[eight$teams_updated$name %in% eight_winners,]$elo,
    elo_opp =  eight$teams_updated[eight$teams_updated$name %in% eight_winners,]$elo_opp,
    win_tri = 1,
    goal_dif = eight$teams_updated[eight$teams_updated$name %in% eight_winners,]$goalsDifference,
    k_constant = 60
  )
  
  teams[teams$name %in% eight_losers,]$elo <- update_elo_hot(
    elo = eight$teams_updated[eight$teams_updated$name %in% eight_losers,]$elo,
    elo_opp =  eight$teams_updated[eight$teams_updated$name %in% eight_losers,]$elo_opp,
    win_tri = 0,
    goal_dif = eight$teams_updated[eight$teams_updated$name %in% eight_losers,]$goalsDifference,
    k_constant = 60
  )
  
  # get new lambda with new elo
  teams$lambda <- predict(mod, 
                          newdata = select(teams,elo), 
                          type = "response")
  
  ## Step 3: Design matches for the quarter finals and run them
  quarter_matches <- data.frame(team1 = eight$winners[c(1, 2, 5, 6)], 
                                team2 = eight$winners[c(3, 4, 7, 8)])
  
  quarter <- find_knockout_winners(teams = teams, 
                                   match_data = quarter_matches)
  
  ## and parse the results
  quarter_winners <- teams[match(quarter$winners,teams$number),]$name %>% as.character()
  
  quarter_losers <- c(quarter$goals[quarter$goals$team1_name %in% quarter_winners,]$team2_name,
                    quarter$goals[quarter$goals$team2_name %in% quarter_winners,]$team1_name)
  
  quarter_winner_goals <- c(quarter$goals[quarter$goals$team1_name %in% quarter_winners,]$goals1,
                          quarter$goals[quarter$goals$team2_name %in% quarter_winners,]$goals2)
  
  quarter_loser_goals <- c(quarter$goals[quarter$goals$team1_name %in% quarter_winners,]$goals2,
                         quarter$goals[quarter$goals$team2_name %in% quarter_winners,]$goals1)
  
  ### 2.5: update elo 
  teams[teams$name %in% quarter_winners,]$elo <- update_elo_hot(
    elo = quarter$teams_updated[quarter$teams_updated$name %in% quarter_winners,]$elo,
    elo_opp =  quarter$teams_updated[quarter$teams_updated$name %in% quarter_winners,]$elo_opp,
    win_tri = 1,
    goal_dif = quarter$teams_updated[quarter$teams_updated$name %in% quarter_winners,]$goalsDifference,
    k_constant = 60
  )
  
  teams[teams$name %in% quarter_losers,]$elo <- update_elo_hot(
    elo = quarter$teams_updated[quarter$teams_updated$name %in% quarter_losers,]$elo,
    elo_opp =  quarter$teams_updated[quarter$teams_updated$name %in% quarter_losers,]$elo_opp,
    win_tri = 0,
    goal_dif = quarter$teams_updated[quarter$teams_updated$name %in% quarter_losers,]$goalsDifference,
    k_constant = 60
  )
  
  # get new lambda with new elo
  teams$lambda <- predict(mod, 
                          newdata = select(teams,elo), 
                          type = "response")
  
  
  ## Step 4: Semi finals ... yada yada yada
  semi_matches <- data.frame(team1 = quarter$winners[c(1,3)], 
                             team2 = quarter$winners[c(2,4)])
  
  semi <- find_knockout_winners(teams=teams, 
                                match_data = semi_matches)
  
  ## and parse the results
  semi_winners <- teams[match(semi$winners,teams$number),]$name %>% as.character()
  
  semi_losers <- c(semi$goals[semi$goals$team1_name %in% semi_winners,]$team2_name,
                      semi$goals[semi$goals$team2_name %in% semi_winners,]$team1_name)
  
  semi_winner_goals <- c(semi$goals[semi$goals$team1_name %in% semi_winners,]$goals1,
                            semi$goals[semi$goals$team2_name %in% semi_winners,]$goals2)
  
  semi_loser_goals <- c(semi$goals[semi$goals$team1_name %in% semi_winners,]$goals2,
                           semi$goals[semi$goals$team2_name %in% semi_winners,]$goals1)
  
  ### 2.5: update elo 
  teams[teams$name %in% semi_winners,]$elo <- update_elo_hot(
    elo = semi$teams_updated[semi$teams_updated$name %in% semi_winners,]$elo,
    elo_opp =  semi$teams_updated[semi$teams_updated$name %in% semi_winners,]$elo_opp,
    win_tri = 1,
    goal_dif = semi$teams_updated[semi$teams_updated$name %in% semi_winners,]$goalsDifference,
    k_constant = 60
  )
  
  teams[teams$name %in% semi_losers,]$elo <- update_elo_hot(
    elo = semi$teams_updated[semi$teams_updated$name %in% semi_losers,]$elo,
    elo_opp =  semi$teams_updated[semi$teams_updated$name %in% semi_losers,]$elo_opp,
    win_tri = 0,
    goal_dif = semi$teams_updated[semi$teams_updated$name %in% semi_losers,]$goalsDifference,
    k_constant = 60
  )
  
  # get new lambda with new elo
  teams$lambda <- predict(mod, 
                          newdata = select(teams,elo), 
                          type = "response")
  
  ## Steps 5: Third and fourth consolation
  bronze_match <- matrix(quarter$winners[!quarter$winners %in% semi$winners], ncol=2) %>%
    as.data.frame() %>% 
    setNames(c("team1","team2"))
  
  bronze <- find_knockout_winners(teams=teams, 
                                  match_data = bronze_match)
  
  bronze_winner <- bronze$winners
  
  ## Step 6: Finals!
  final_match <- matrix(semi$winners, ncol=2) %>%
    as.data.frame() %>% 
    setNames(c("team1","team2"))
  
  final <- find_knockout_winners(teams=teams, 
                                 match_data = final_match)
  
  final_result <- final$winners
  
  ## Return a vector with the teams in ranked order. 
  ## Note only the first 4 are individuals - the rest are really groups
  final_result <- final_result %>% as.matrix() %>% as.numeric()
  final_match <- final_match %>% as.matrix() %>% as.numeric()
  bronze_winner <- bronze_winner %>% as.matrix() %>% as.numeric()
  bronze_match <- bronze_match %>% as.matrix() %>% as.numeric()
  quarter_winners <- quarter$winners %>% as.matrix() %>% as.numeric()
  quarter_matches <- quarter_matches %>% as.matrix() %>% as.numeric()
  eight_winners <- eight$winners %>% as.matrix() %>% as.numeric()
  eight_matches <- eight_matches %>% as.matrix() %>% as.numeric()
  
  final_ranking <- c(final_result, # Number 1
                     final_match[!(final_match %in% final_result)], #2
                     bronze_winner, # Number 3
                     bronze_match[!(bronze_match %in% bronze_winner)], #4
                     quarter_matches[!(quarter_matches %in% quarter_winners)], # 5-8
                     eight_matches[!(eight_matches %in% eight_winners)], # 9-16
                     seq(32)[!(seq(32) %in% eight_matches)]
  ) %>% as.numeric()
  
  final_ranking
  
  total_goals <- list(eight$goals, quarter$goals, semi$goals, bronze$goals, final$goals) %>%
    do.call('rbind',.)
  
  total_goals
  
  # goals scored
  goals <- total_goals %>% 
    select(team1, goals1) %>% 
    setNames(c("team", "goals")) %>% 
    bind_rows(
      total_goals %>% 
        select(team2, goals2) %>% 
        setNames(c("team", "goals")) 
    ) %>% 
    rbind(setNames(select(group_goals,number,goals), c('team','goals'))) %>% 
    mutate(team = as.character(team)) %>% 
    group_by(team) %>% 
    summarise(goals = sum(goals)) %>% 
    ungroup()
  
  # roung made 
  round_made <-suppressWarnings(data.frame(team = final_result,
                           round = "Win Final") %>% 
    bind_rows(
      data.frame(team = final_match,
             round=rep("Make Final",2))
    ) %>% 
    bind_rows(
      data.frame(team = c(semi_matches$team1,semi_matches$team2),
             round = rep("Make Semis",4))
    ) %>%
    bind_rows(
      data.frame(team = quarter_matches,
             round = rep("Make Quarters",8))
    ) %>% 
    bind_rows(
      data.frame(team = eight_matches,
             round = rep("Make Round of 16",8))
    ))
  
  return(list(final_ranking = final_ranking,
              goals = goals,
              round_made=round_made))
}

# a functino to repeat the simulation x number of times in parallel --------
simulate_tournament_par <- function(nsim = 10,
                                    teams,
                                    group_matches,
                                    use_real_results) {
  
  # setup parallel backend to use many processors
  ## Calculate the number of cores
  num_cores <- ifelse(detectCores()>1,detectCores() - 1,detectCores()) # get available cores
  registerDoParallel(cores=num_cores)  
  cl <- parallel::makeCluster(num_cores)
  
  simulations <- foreach(i=icount(nsim),.export = c("team_data","group_matches","use_real_results")) %dopar% { 
    draw <- simulate_one(
      teams = team_data,
      group_matches = group_matches,
      use_real_results = use_real_results) 
    
    return(draw)
  }
  
  # stop cluster
  stopCluster(cl)
  
  return(simulations)
  
}
""
# get the highest scorer --------
get_top_scorer <- function(nsim,result_data){
  # result_data <- world_cup
  res <- map_df(result_data, "goals") %>%
    group_by(team=as.numeric(team)) %>%
    summarise(goals = round(sum(goals) / nsim)) %>%
    ungroup() %>%
    dplyr::rename("number" = team)
  
  all <- team_data %>% 
    mutate(number = number) %>% 
    right_join(res) %>%
    dplyr::rename(team = name)
  
  total_goals <- all %>% 
    group_by(team) %>% 
    summarise(goals = sum(goals)) %>% 
    arrange(goals) %>% 
    mutate(team = reorder(team, goals)) %>% 
    arrange(desc(goals))
  
  return(total_goals)
}

# get the most likely winner! -------
get_probability <- function(result_data) {
  # result_data <- world_cup
  result_data <- result_data %>% map("final_ranking")
  
  result_data <- result_data %>% 
    do.call('cbind',.) %>%
    as.matrix()
  
  winner <- table(result_data[1, ]) # first row, first ranking teams in each simulation
  
  names(winner) <- team_data$name[match(names(winner), team_data$number)] %>% as.character()
  
  winner <- (winner / sum(winner)) %>%
    sort(decreasing = T) %>% 
    as.data.frame() %>%
    setnames(c("team","prob"))
  
  return(winner)
}

# function for scaling colors according to team ---------
scale_color_teams <- scale_color_manual(values=c(
  "Brazil" = "#4CAF50",
  "Germany" = "#17202A",
  "Spain" = "#EE2700",
  "Argentina" = "#85C1E9",
  "France" = "#3F51B5",
  "Portugal" = "#A93226",
  "Belgium" = "#F52300",
  "Colombia" = "#FFE082",
  "England" = "#EF9A9A",
  "Uruguay" = "#1E88E5",
  "Switzerland" = "#EF9A9A",
  "Peru" = "#FF765B",
  "Croatia" = "#E96147",
  "Mexico" = "#229954",
  "Denmark" = "#FC866E",
  "Poland" = "#C0392B",
  "Sweden" = "#F1C40F",
  "Morocco" = "#27AE60",
  "South Korea" = "#6FABD4",
  "Panama" = "#1945FF",
  "Tunisia" = "#D74949",
  "Japan" = "#B92626",
  "Senegal" = "#27AE60",
  "Russia" = "#FF3434",
  "Egypt" = "#414141",
  "Saudi Arabia" = "#2DCA70",
  "Iran" = "#21B45F",
  "Australia" = "#1A5276",
  "Costa Rica" = "#1434FF",
  "Nigeria" = "#31D576",
  "Iceland" = "#3486DB",
  "Serbia" = "#223BB3"
))


# a function to simulate one matchup nmatches_in times --------
get_matchup_stats <- function(team_data,teama_in,teamb_in,nmatches_in,musthavewinner_in){
  
  # replicate
  match_sims <- vector('list',nmatches_in)
  match_sims <- lapply(match_sims,
                       function(x){
                         match <- play_fun_double_poisson(team_data,
                                                          teama_in,
                                                          teamb_in,
                                                          musthavewinner = musthavewinner_in)
                         return(match)}
  )
  
  # bind outcome
  match_sims.df <- do.call('rbind',match_sims) %>% 
    as.data.frame() %>%
    mutate(Agoals.margin = Agoals-Bgoals,
           outcome = case_when(Agoals.margin>0~sprintf("%s Wins",teama_in),
                               Agoals.margin<0~sprintf("%s Wins",teamb_in),
                               Agoals.margin==0~"Draw"))
  
  match_probs <- match_sims.df %>% 
    mutate(Agoals = ifelse(Agoals>=5,"5+",Agoals),
           Bgoals = ifelse(Bgoals>=5,"5+",Bgoals)) %>%
    count(Agoals,Bgoals) %>% 
    mutate(prob = round((n/nmatches_in)*100)) %>%
    arrange(desc(prob)) %>% 
    mutate(Agoals = as.character(Agoals),
           Bgoals = as.character(Bgoals),
           prob = ifelse(prob<0.01,0.5,prob)) %>% 
    as.data.frame() %>%
    mutate(comb = paste(Agoals,Bgoals)) %>%
    select(-n)
  
  # replace all less than 1 with "<1%"
  fill_data <- expand.grid(c("0","1","2","3","4","5+"), c("0","1","2","3","4","5+")) %>%
    setNames(c("Agoals","Bgoals")) %>%
    mutate(prob = 0.5,
           comb = paste(Agoals,Bgoals)) 
  
  match_probs <- suppressWarnings(
    bind_rows(match_probs,fill_data %>% filter(!comb %in% match_probs$comb)) %>% 
      mutate(problabel = ifelse(prob == 0.5,"<1",prob))
  )
  
  # print chance team wins and average goals per team
  probs_summary <- match_sims.df %>%
    group_by(outcome) %>%
    summarise(n=n()) %>%
    mutate(prob = paste0(round(n/sum(n)*100)),"%") %>% 
    select(outcome,prob) %>% 
    as.data.frame()
  
  if(isTRUE(musthavewinner_in)){
    probs_summary <- probs_summary %>%
      bind_rows(data.frame("outcome" = "Draw","prob" = "0"))
  }
  
  probs_summary %>% print()
  
  agoals = match_sims.df %>% pull(Agoals)
  bgoals = match_sims.df %>% pull(Bgoals)
  
  median.a = ifelse(length(agoals)>0,median(agoals),0)
  median.b = ifelse(length(bgoals)>0,median(bgoals),0)
  
  goals_summary <- data.frame(A = median.a,
                              B = median.b)
  
  names(goals_summary)<-c(sprintf("Med.%s.Goals",teama_in),
                          sprintf("Med.%s.Goals",teamb_in))
  
  goals_summary %>% print()
  
  # graph
  gg <- ggplot(match_probs,aes(x=Agoals,y=Bgoals,fill=prob)) +
    geom_tile(show.legend = F) +
    geom_label(data=match_probs,
               aes(label=paste0(problabel,"%")),
               col="gray20",show.legend = F) +
    scale_fill_continuous(low="#F4FBFF",high="#E74C3C") +
    labs(title=sprintf("Projected Goals for %s vs %s",teama_in,teamb_in),
         subtitle = sprintf("Chance %s wins: %s%% | Chance %s wins: %s%% | Chance of a draw: %s%%",
                            teama_in,
                            probs_summary$prob[grepl(teama_in,probs_summary$outcome)],
                            teamb_in,
                            probs_summary$prob[grepl(teamb_in,probs_summary$outcome)],
                            probs_summary$prob[!(grepl(teama_in,probs_summary$outcome)|
                                                 grepl(teamb_in,probs_summary$outcome))]),
         x = sprintf("Goals for %s",teama_in),
         y = sprintf("Goals for %s",teamb_in)) 
  
  plot_elliott(plotin = gg,
               directory = sprintf("output/graphics/matches/%s_vs_%s_worldcup2018.png",
                                   teama_in,teamb_in),
               width = 1050,height=1000,unit="px",res=170,
               themearg = theme(panel.grid.major.x = element_blank(),
                                panel.grid.major.y = element_blank()))
  
  return(match_sims.df)
}
