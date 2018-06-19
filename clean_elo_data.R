# set this variable to whatever year we are running 
library(tidyverse)
years <- seq(1998,2014,4)
# historical data ---------
# cleaning function
clean_raw_elo_plus_match <- 
  function(year) {
    to_transform <- read.csv(sprintf('data/elo_clean/raw_%s.csv',year),stringsAsFactors = F)
    
    # get rid of header
    to_transform <- to_transform[5:nrow(to_transform),] %>% as.data.frame() 
    names(to_transform)<-c("data") 
    
    # get rid of pesky characters
    to_transform$data <- gsub("_","",to_transform$data)
    
    # for each match
    matches <- vector('list',length = nrow(to_transform)/16)
    
    for(i in 1:length(matches)){
      stop_window <- i*16
      start_window <- stop_window-15
      
      # get data
      temp <- to_transform[start_window:stop_window,] %>% as.data.frame() 
      
      # data frame for first team
      teama <- data.frame(date = paste0(as.character(temp[1,]),"-",as.character(temp[2,])),
                 team = as.character(temp[3,]),
                 goals = as.character(temp[5,]),
                 elo = as.numeric(as.character(temp[11,])) - as.numeric(as.character(temp[9,])) ,
                 rank = as.numeric(as.character(temp[15,])) - as.numeric(as.character(temp[13,]))
      )
      
      # data frame for second team
      teamb <- data.frame(date = paste0(as.character(temp[1,]),"-",as.character(temp[2,])),
                          team = as.character(temp[4,]),
                          goals = as.character(temp[6,]),
                          elo = as.numeric(as.character(temp[12,])) - as.numeric(as.character(temp[10,])) ,
                          rank = as.numeric(as.character(temp[16,])) - as.numeric(as.character(temp[14,]))
      )
      
      match <- data.frame(suppressWarnings(bind_rows(teama,teamb))) %>%
                   mutate("match_id" = paste0(year,"-",i),
                          "stage" = as.character(temp[7,]))
      
      matches[[i]] <- match
      
    }
    
    matches <- do.call('rbind',matches)
    
    write.csv(matches,sprintf('data/elo_clean/clean_%s.csv',year),row.names = F)
}

# cleaning data
for(year in years){
  #cat(year)
  clean_raw_elo_plus_match(year)
}

# join all the data
read_in <- vector('list',length(years))

for(i in 1:length(years)){
  read_in[[i]] <- read.csv(sprintf("data/elo_clean/clean_%s.csv",years[[i]]),stringsAsFactors = F)
}
  

training_data_out <- do.call('rbind',read_in)

write.csv(training_data_out,"data/matches_history.csv",row.names = F)

# elo at start of 2018 ---------
to_transform <- read.csv("data/elo_clean/elo_2018_start_raw.csv",stringsAsFactors = F)

to_transform <- to_transform[22:nrow(to_transform),] %>% as.data.frame() 
names(to_transform)<-c("data") 

# get rid of pesky characters
to_transform$data <- gsub("_","",to_transform$data)

# for each team
matches <- vector('list',length = nrow(to_transform)/17)

for(i in 1:length(matches)){
  stop_window <- i*17
  start_window <- stop_window-16
  
  # get data
  temp <- to_transform[start_window:stop_window,] %>% as.data.frame() 
  
  # data frame for first team
  team <- data.frame(team = as.character(temp[3,]),
                      elo = as.character(temp[4,])
  )
  
  matches[[i]] <- team
  
}

matches <- do.call('rbind',matches)

write.csv(matches,"elo_start_2018_clean.csv",row.names = F)


# elo at current point in 2018 ---------
to_transform <- read.csv("data/elo_clean/elo_2018_start_raw.csv",stringsAsFactors = F)

to_transform <- to_transform[22:nrow(to_transform),] %>% as.data.frame() 
names(to_transform)<-c("data") 

# get rid of pesky characters
to_transform$data <- gsub("_","",to_transform$data)

# for each team
matches <- vector('list',length = nrow(to_transform)/17)

for(i in 1:length(matches)){
  stop_window <- i*17
  start_window <- stop_window-16
  
  # get data
  temp <- to_transform[start_window:stop_window,] %>% as.data.frame() 
  
  # data frame for first team
  team <- data.frame(team = as.character(temp[3,]),
                     elo = as.character(temp[4,])
  )
  
  matches[[i]] <- team
  
}

matches <- do.call('rbind',matches)

write.csv(matches,"data/elo_clean/elo_2018_start_clean.csv",row.names = F)
