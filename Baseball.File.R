BB12 <- read.csv("C:/Users/quino/OneDrive - Texas Lutheran University/Desktop/Spring 2023 Classes/Data Mining/R/Baseball/gl2012.txt", header=FALSE, stringsAsFactors=TRUE)
IL12<- BB12[BB12$V5 != BB12$V8,]
ALW<- 0;NLW <- 0
IL12[15:161]<- NULL
head(IL12)

BB13 <- read.csv("C:/Users/quino/OneDrive - Texas Lutheran University/Desktop/Spring 2023 Classes/Data Mining/R/Baseball/gl2013.txt", header=FALSE, stringsAsFactors=TRUE)
IL13<- BB13[BB13$V5 != BB13$V8,]
ALW.13<- 0;NLW.13 <- 0
IL13[15:161]<- NULL
head(IL13)

BB14 <- read.csv("C:/Users/quino/OneDrive - Texas Lutheran University/Desktop/Spring 2023 Classes/Data Mining/R/Baseball/gl2014.txt", header=FALSE, stringsAsFactors=TRUE)
IL14<- BB14[BB14$V5 != BB14$V8,]
ALW.14<- 0;NLW.14 <- 0
IL14[15:161]<- NULL
head(IL14)



IL12_14 <- rbind(IL12,IL13,IL14)
View(IL12_14)

# Write the dataframe to a CSV file
write.csv(IL12_14, "IL12_14.csv", row.names = FALSE)

## Part 2
#Question 1
#AL league wins in 2012
AL_wins_12 <- ifelse((IL12$V5== 'AL' &
                         IL12$V10 >IL12$V11)|
                        (IL12$V5=='NL'&
                           IL12$V10<IL12$V11),
                      'AL','NL')
sum(AL_wins_12=='AL')

#NL league wins in 2012
NL_wins_12 <- ifelse((IL12$V5== 'NL' &
                        IL12$V10 >IL12$V11)|
                       (IL12$V5=='AL'&
                          IL12$V10<IL12$V11),
                     'NL','AL')
sum(NL_wins_12=='NL')

#AL wins in 2013
AL_wins_13 <- ifelse((IL13$V5== 'AL' &
                            IL13$V10 >IL13$V11)|
                           (IL13$V5=='NL'&
                              IL13$V10<IL13$V11),
                         'AL','NL')
sum(AL_wins_13=='AL')

#NL league wins in 2013
NL_wins_13 <- ifelse((IL13$V5== 'NL' &
                        IL13$V10 >IL13$V11)|
                       (IL13$V5=='AL'&
                          IL13$V10<IL13$V11),
                     'NL','AL')
sum(NL_wins_13=='NL')


#AL wins in 2014
AL_wins_14 <- ifelse((IL14$V5== 'AL' &
                            IL14$V10 >IL14$V11)|
                           (IL14$V5=='NL'&
                              IL14$V10<IL14$V11),
                         'AL','NL')
sum(AL_wins_14=='AL')

#NL league wins in 2014
NL_wins_14 <- ifelse((IL14$V5== 'NL' &
                        IL14$V10 >IL14$V11)|
                       (IL14$V5=='AL'&
                          IL14$V10<IL14$V11),
                     'NL','AL')
sum(NL_wins_14=='NL')


#Question 2 Calculating the point difference for AL team runs - NL team runs
# Point difference in 2012
IL12_AL_MoV <- ifelse(IL12$V5 == 'AL', IL12$V10 - IL12$V11, IL12$V11- IL12$V10)
IL12_AL_MoV


#confirming the number of NL team wins in 2012 using MoV
NL_WINS_12 <- sum(IL12_AL_MoV <0)
NL_WINS_12

#calculating the NL's win percentage in 2012
NL_WIN_PERC_12 <- round(NL_WINS_12/(nrow(IL12)),2)
NL_WIN_PERC_12

#confirming the number of AL team wins in 2012 using MoV
AL_WINS_12 <- sum(IL12_AL_MoV >0)
AL_WINS_12

#calculating the AL's win percentage in 2012
AL_WIN_PERC_12 <- round(AL_WINS_12/(nrow(IL12)),2)
AL_WIN_PERC_12

# Point difference in 2013 (Margin of Victory)
IL13_AL_MoV <- ifelse(IL13$V5 == 'AL', IL13$V10 - IL13$V11, IL13$V11- IL13$V10)
IL13_AL_MoV

#confirming the number of NL team wins in 2013 using MoV
NL_WINS_13 <- sum(IL13_AL_MoV <0)
NL_WINS_13

#calculating the NL's win percentage in 2013
NL_WIN_PERC_13 <- round(NL_WINS_13/(nrow(IL13)),2)
NL_WIN_PERC_13

#confirming the number of AL team wins in 2013 using MoV
AL_WINS_13 <- sum(IL13_AL_MoV >0)
AL_WINS_13

#calculating the AL's win percentage in 2013
AL_WIN_PERC_13 <- round(AL_WINS_13/(nrow(IL13)),2)
AL_WIN_PERC_13

# Point difference in 2014 (Margin of Victory)
IL14_AL_MoV <- ifelse(IL14$V5 == 'AL', IL14$V10 - IL14$V11, IL14$V11- IL14$V10)
IL14_AL_MoV

#confirming the number of NL team wins in 2014 using MoV
NL_WINS_14 <- sum(IL14_AL_MoV <0)
NL_WINS_14

#calculating the NL's win percentage in 2014
NL_WIN_PERC_14 <- round(NL_WINS_14/(nrow(IL14)),2)
NL_WIN_PERC_14

#confirming the number of AL team wins in 2014 using MoV
AL_WINS_14 <- sum(IL14_AL_MoV >0)
AL_WINS_14

#calculating the AL's win percentage in 2014
AL_WIN_PERC_14 <- round(AL_WINS_14/(nrow(IL14)),2)
AL_WIN_PERC_14

#Question 3
#Avg AL Runs per game in 2012
AL_Runs_12 <- ifelse(IL12$V5 == 'AL', IL12$V10, IL12$V11)
AL_Runs_12
Avg_AL_Runs_PG_12 <- (sum(AL_Runs_12)/nrow(IL12))
Avg_AL_Runs_PG_12


#Avg NL Runs per game in 2012
NL_Runs_12 <- ifelse(IL12$V5 == 'NL', IL12$V10, IL12$V11)
NL_Runs_12
Avg_NL_Runs_PG_12 <- (sum(NL_Runs_12)/nrow(IL12))
Avg_NL_Runs_PG_12

#Avg AL Runs per game in 2013
AL_Runs_13 <- ifelse(IL13$V5 == 'AL', IL13$V10, IL13$V11)
AL_Runs_13
Avg_AL_Runs_PG_13 <- (sum(AL_Runs_13)/nrow(IL13))
Avg_AL_Runs_PG_13


#Avg NL Runs per game in 2013
NL_Runs_13 <- ifelse(IL13$V5 == 'NL', IL13$V10, IL13$V11)
NL_Runs_13
Avg_NL_Runs_PG_13 <- (sum(NL_Runs_13)/nrow(IL13))
Avg_NL_Runs_PG_13

#Avg AL Runs per game in 2014
AL_Runs_14 <- ifelse(IL14$V5 == 'AL', IL14$V10, IL14$V11)
AL_Runs_14
Avg_AL_Runs_PG_14 <- (sum(AL_Runs_14)/nrow(IL14))
Avg_AL_Runs_PG_14


#Avg NL Runs per game in 2014
NL_Runs_14 <- ifelse(IL14$V5 == 'NL', IL14$V10, IL14$V11)
NL_Runs_14
Avg_NL_Runs_PG_14 <- (sum(NL_Runs_14)/nrow(IL14))
Avg_NL_Runs_PG_14


#Question 4: in games in which AL league team won, what was the average winning
#margin

#average margin of victory in 2012 for AL team
avg_AL_MoV_12 <- mean(subset(IL12_AL_MoV, IL12_AL_MoV >0))
avg_AL_MoV_12

#average margin of victory in 2012 for NL team
avg_NL_MoV_12 <- abs(mean(subset(IL12_AL_MoV, IL12_AL_MoV <0)))
avg_NL_MoV_12

#average margin of victory in 2013 for AL team
avg_AL_MoV_13 <- mean(subset(IL13_AL_MoV, IL13_AL_MoV >0))
avg_AL_MoV_13

#average margin of victory in 2013 for NL team
avg_NL_MoV_13 <- abs(mean(subset(IL13_AL_MoV, IL13_AL_MoV <0)))
avg_NL_MoV_13

#average margin of victory in 2014 for AL team
avg_AL_MoV_14 <- mean(subset(IL14_AL_MoV, IL14_AL_MoV >0))
avg_AL_MoV_14

#average margin of victory in 2014 for NL team
avg_NL_MoV_14 <- abs(mean(subset(IL14_AL_MoV, IL14_AL_MoV <0)))
avg_NL_MoV_14

#Question 5: For Each game, calculate the # of games in which the margin of
##victory exceeded 3 runs

#No. Games in AL where MoV exceeded 3 runs during 2012
Games_AL_MoV_3_12 <- length(subset(IL12_AL_MoV, IL12_AL_MoV >3))
Games_AL_MoV_3_12

#No. Games in NL where MoV exceeded 3 runs during 2012
Games_NL_MoV_3_12 <- length(subset(IL12_AL_MoV, IL12_AL_MoV < -3))
Games_NL_MoV_3_12

#No. Games in AL where MoV exceeded 3 runs during 2013
Games_AL_MoV_3_13 <- length(subset(IL13_AL_MoV, IL13_AL_MoV >3))
Games_AL_MoV_3_13

#No. Games in NL where MoV exceeded 3 runs during 2013
Games_NL_MoV_3_13 <- length(subset(IL13_AL_MoV, IL13_AL_MoV < -3))
Games_NL_MoV_3_13

#No. Games in AL where MoV exceeded 3 runs during 2013
Games_AL_MoV_3_14 <- length(subset(IL14_AL_MoV, IL14_AL_MoV >3))
Games_AL_MoV_3_14

#No. Games in NL where MoV exceeded 3 runs during 2013
Games_NL_MoV_3_14 <- length(subset(IL14_AL_MoV, IL14_AL_MoV < -3))
Games_NL_MoV_3_14


## Part 3 
# Creating a user defined function that returns the year, the team, and the number of wins that the team got

#frist, I am going to create a column that contains the full name for each team based on their abbreviation
head(IL12_14)
#I am going to do this by first getting the unique abbreviations fo each year:
#Visiting Team 
unique_vis_teams <- sort(unique(IL12_14$V4))
unique_vis_teams
# In 2012, there were 30 unique visiting teams. I have to make sure that there is an equal number of unique home teams
## and that thet are the same teams
unique_home_teams <- sort(unique(IL12_14$V7))
unique_home_teams
#making sure they contain the same values
unique_vis_teams == unique_home_teams
# They contain the same values

# Reading dataframes containing the respective full name of each team's abbreviation
home_team_names <- read.csv("C:/Users/quino/OneDrive - Texas Lutheran University/Desktop/Spring 2023 Classes/Data Mining/R/Baseball/Home_Names_12.csv", header=TRUE, stringsAsFactors=TRUE)
vis_team_names <- read.csv("C:/Users/quino/OneDrive - Texas Lutheran University/Desktop/Spring 2023 Classes/Data Mining/R/Baseball/Visit_Names_12.csv", header=TRUE, stringsAsFactors=TRUE)
#Merging dataframes to have visitor and home team name
library(dplyr)

merged_df <- merge(IL12_14, home_team_names, by.x = "V4", by.y = "vis_team_abbr", all.x = TRUE)
merged_12_14 <- merge(merged_df, vis_team_names, by.x = "V7", by.y = "home_team_abbr", all.x = TRUE)
merged_12_14 <- merged_12_14[, c( "V1", "V2", "V3", "V4","V5", "V6","V7", "V8", "V9", "V10", "V11", "V12", "V13", "V14", "vis_team_name", "home_team_name")]
head(merged_12_14)

#Extracting the year
library(stringr)

merged_12_14$year<- str_sub(merged_12_14$V1, 1,4)
merged_12_14$year <- as.numeric(merged_12_14$year)
head(merged_12_14)

# Calculating the games played by boston in 2012 to confirm results of UDF
BOS12_wins <- ifelse((merged_12_14$year == 2012) & 
                       ((merged_12_14$V4 == 'BOS' & merged_12_14$V10 > merged_12_14$V11) |
                          (merged_12_14$V7 == 'BOS' & merged_12_14$V10 < merged_12_14$V11)),
                     'Win', 'No win')
# Boston wins
BOS12_total_wins <- sum(BOS12_wins=='Win')
BOS12_total_wins
# Boston games played
BOS_GP12 <- ifelse((merged_12_14$year == 2012) & 
                     (merged_12_14$V4 == 'BOS'|merged_12_14$V7 == 'BOS'), "Played", "No Play")
BOS12_total_GP <- sum(BOS_GP12 == "Played")
BOS12_total_GP
#winning Frequency 
BOS12_Winning_Freq <- round((BOS12_total_wins/BOS12_total_GP),2)
BOS12_Winning_Freq

#season_summary_sentence
season_summary_sentence <- function(team_name, year){
  #current year
  current_year <- year
  #filtering the data for specific team
  team <- merged_12_14[(merged_12_14$V4 == team_name | merged_12_14$V7 == team_name)
                       & merged_12_14$year == year,]
  #calculating games won
  games_won <- sum(ifelse((team$V4 == team_name & team$V10 > team$V11)|
                            (team$V7 == team_name & team$V11 > team$V10),1,0))
  #calculating games played
  games_played <- nrow(team)
  #calculating winning frequency
  win_freq <- round(games_won/games_played, 2)
  #generating and printing the sentence
  sentence <- paste0("In ", current_year," ", team_name, " won ", games_won, " games and played ",
                     games_played, ". Their winning frequency was ", win_freq, ".")
  return(sentence)
}

team_name <- "BOS"
year <- 2012
season_summary_sentence(team_name, year)

merged_12_14[(merged_12_14$V4 == team_name | merged_12_14$V7 == team_name)
             & merged_12_14$year == year,]

View(merged_12_14)
#creating a user defined function that accepts a three letter designator and returns that full name
## and league membership

name_and_membership_sentence <- function(team_name){
  #filtering for the team name
  team <- merged_12_14[merged_12_14$V4 == team_name | merged_12_14$V7 == team_name,]
  #Assigning a variable to the full name
  league_membership <- unique(ifelse(team$V4 == team_name & team$V5 == "AL" | team$V7 == team_name & team$V8 == "AL", "AL", "NL"))
  #Assigning the full name
  full_name <- unique(ifelse(team$V4 == team_name , as.character(team$vis_team_name), as.character(team$home_team_name)))
  #creating a sentence
  league_sentence <- paste0("The ", full_name, " played in the ", league_membership, " from 2012-2014.")
  return(league_sentence)
}

team_name <- "BOS"

name_and_membership_sentence(team_name)

head(merged_12_14)

## Calculating what was the average margin of victory by team for any given year
Avg_Mov_Function_Season <- function() {
  # creating empty vectors for variables
  years <- c()
  team_names <- c()
  avg_win_mov <- c()
  
  # getting list of unique team names
  team_names_unique <- unique(c(merged_12_14$V4, merged_12_14$V7))
  
  # iterating through each team and year and calculating their average margin of victory
  for (team_name in team_names_unique) {
    for (year in unique(merged_12_14$year)) {
      team <- merged_12_14[(merged_12_14$V4 == team_name | merged_12_14$V7 == team_name) &
                             merged_12_14$year == year, ]
      margin_of_victory <- ifelse(team$V4 == team_name, team$V10 - team$V11, team$V11 - team$V10)
      avg_win_Mov <- round(mean(subset(margin_of_victory, margin_of_victory > 0)), 2)
      
      # appending results to vectors
      years <- c(years, year)
      team_names <- c(team_names, team_name)
      avg_win_mov <- c(avg_win_mov, avg_win_Mov)
    }
  }
  
  # creating data frame with results
  results_df <- data.frame(year = years, team_name = team_names, avg_win_mov = avg_win_mov)
  
  # sorting the results by year
  results_df <- results_df[order(results_df$year), ]
  
  # returning the data frame
  return(results_df)
}

Avg_Mov_Function_Season()


#Creating this function so that it gives me a list of every team's MOV for every year
# This list will be ordered by team

Avg_Mov_Function_Team <- function() {
  # creating empty vectors for variables
  years <- c()
  team_names <- c()
  avg_win_mov <- c()
  
  # getting list of unique team names
  team_names_unique <- unique(c(merged_12_14$V4, merged_12_14$V7))
  
  # iterating through each team and year and calculating their average margin of victory
  for (team_name in team_names_unique) {
    for (year in unique(merged_12_14$year)) {
      team <- merged_12_14[(merged_12_14$V4 == team_name | merged_12_14$V7 == team_name) &
                             merged_12_14$year == year, ]
      margin_of_victory <- ifelse(team$V4 == team_name, team$V10 - team$V11, team$V11 - team$V10)
      avg_win_Mov <- round(mean(subset(margin_of_victory, margin_of_victory > 0)), 2)
      
      # appending results to vectors
      years <- c(years, year)
      team_names <- c(team_names, team_name)
      avg_win_mov <- c(avg_win_mov, avg_win_Mov)
    }
  }
  
  # creating data frame with results
  results_df <- data.frame(year = years, team_name = team_names, avg_win_mov = avg_win_mov)
  
  # ordering the results by the team name
  results_df <- results_df[order(results_df$team_name), ]
  
  # returning the data frame
  return(results_df)
}

Avg_Mov_Function_Team()


# Creating a functiont that lists the average MoV by team for every season

#Creating this function so that it gives me a list of every team and every year
#this list will be ordered by year

Avg_Mov_Function_Season <- function() {
  # getting list of unique team names
  team_names <- unique(c(merged_12_14$V4, merged_12_14$V7))
  
  # storing the results in an empty vector
  result <- c()
  
  # iterating through each team and year and calculating their  average margin of victory
  for (team_name in team_names) {
    for (season in unique(merged_12_14$year)) {
      team <- merged_12_14[(merged_12_14$V4 == team_name | merged_12_14$V7 == team_name) &
                             merged_12_14$year == season, ]
      margin_of_victory <- ifelse(team$V4 == team_name, team$V10 - team$V11, team$V11 - team$V10)
      avg_win_Mov <- round(mean(subset(margin_of_victory, margin_of_victory > 0)),2)
      sentence <- paste0("season ", season, " ", team_name, " avg MoV = ", avg_win_Mov)
      result <- c(result, sentence)
    }
  }
  #sorting the results by season
  result <- sort(result)
  # return results
  return(result)
}

Avg_Mov_Function_Season()

#in each year, I am reporting, by team league avg MoV, team avg MoV, % wins,
## league's % wins, team's no. games won by 3+ points, % of games won by 3+ points

final_team_stats_df <- function() {
  # getting list of unique team names
  team_names <- unique(c(merged_12_14$V4, merged_12_14$V7))
  
  # creating an empty dataframe to store the results
  results_df <- data.frame(season = numeric(),
                           team_name = character(),
                           avg_Mov = numeric(),
                           win_perc = numeric(),
                           games_won_by_3 = numeric(),
                           won_by_3_perc = numeric())
  
  # iterating through each team and year and calculating their  average margin of victory
  for (team_name in team_names) {
    for (season in unique(merged_12_14$year)) {
      team <- merged_12_14[(merged_12_14$V4 == team_name | merged_12_14$V7 == team_name) & merged_12_14$year == season, ]
      margin_of_victory <- ifelse(team$V4 == team_name, team$V10 - team$V11, team$V11 - team$V10)
      avg_Mov <- round(mean(subset(margin_of_victory, margin_of_victory > 0)),2)
      games_won <- length(subset(margin_of_victory, margin_of_victory > 0))
      no_games_played <- nrow(team)
      win_perc <- round((games_won/no_games_played),2)
      games_won_by_3 <- length(subset(margin_of_victory, margin_of_victory >= 3))
      won_by_3_perc <- round((games_won_by_3/no_games_played),2)
      league <- unique(subset(team$V5, team$V4 == team_name))
      
      # adding a new row to the results dataframe for this team and season
      new_row <- data.frame(season = season,
                            team_name = team_name,
                            league = league,
                            avg_Mov = avg_Mov,
                            win_perc = win_perc,
                            games_won_by_3 = games_won_by_3,
                            won_by_3_perc = won_by_3_perc)
      results_df <- rbind(results_df, new_row)
    }
  }
  #sorting the results by season
  results_df <- results_df[order(results_df$season),]
  # return results dataframe
  return(results_df)
}

final_team_stats_df()

team_stats_df <- final_team_stats_df()
str(team_stats_df)

final_league_stats_df<- function(){
#getting a list of unique leagues
league_codes <- unique(c(merged_12_14$V5, merged_12_14$V8))

#creating an empty dataframe to store the results
results_df <- data.frame(league = character(),
                         season = numeric(),
                         avg_league_MoV = numeric(),
                         league_win_perc = numeric())

#iterating through each year and calculating their margin of victory and win percentage
for(league in league_codes){
  for(season in unique(merged_12_14$year)){
    league_df <- merged_12_14[(merged_12_14$V5 == league | merged_12_14$V8 == league)&
                                merged_12_14$year == season,]
    MoV <- ifelse(league_df$V5 == league, league_df$V10 - league_df$V11, league_df$V11 - league_df$V10)
    avg_league_MoV <- round(mean(subset(MoV, MoV > 0)),2)
    games_won <- length(subset(MoV, MoV > 0))
    no_games_played <- nrow(league_df)
    league_win_perc <- round((games_won/no_games_played),2)
    
    # adding a new row to the results dataframe for this league and season
    new_row <- data.frame(league = league,
                          season = season,
                          avg_league_MoV = avg_league_MoV,
                          league_win_perc = league_win_perc)
    results_df <- rbind(results_df, new_row)
  }
}
#sorting the results by season
results_df <- results_df[order(results_df$season),]
# return results dataframe
return(results_df)                       
}

final_league_stats_df()

league_stats_df <- final_league_stats_df()
str(league_stats_df)

#Final Dataframe Reporting all Variables FOr Leagues and Teams
final_df <- merge(league_stats_df, team_stats_df, by = c("league", "season"), all.y = TRUE)
final_df <- final_df %>%
  select(season, team_name, avg_Mov, win_perc, games_won_by_3, won_by_3_perc, league, avg_league_MoV, league_win_perc)
final_df <- final_df[order(final_df$season, final_df$team_name),]
rownames(final_df) <- NULL
final_df
head(final_df)

## Looking up which teams played in the World Series in each year



# Creating a function calculating the probability of each team winning the world series
calculate_win_probability <- function(team1, team2, year, data) {
  
  # Subset the data for the given year and the two teams
  subset_data <- data[data$season == year & (data$team_name == team1 | data$team_name == team2), ]
  
  # Extract the win percentages for the two teams
  p1 <- subset_data[subset_data$team_name == team1, "win_perc"]
  p2 <- subset_data[subset_data$team_name == team2, "win_perc"]
  
  # Calculate the probability that team 1 wins the world series
  p <- ((p1 + (1 - p2))/2)
  q <- 1 - p
  sum <- 0
  for (k in 3:6) {
    sum <- sum + choose(k,3)*p^4*q^(k-3)
  }
  
  return(sum)
}


### In 2012, NL was San Francisco giants (Winner) and AL was Detroit Tigers
SFN_W_Prob_2012 <- calculate_win_probability("SFN", "DET", 2012, final_df)
SFN_W_Prob_2012
DET_W_Prob_2012 <- calculate_win_probability("DET", "SFN", 2012, final_df)
DET_W_Prob_2012

#based on this, the win probability was more favorable for Detroit since they had a higher
## win percentage, but they lost the world series in 2012

### In 2013, NL was St. Louis Cardinals and AL was Boston Red Sox (winner)
SLN_W_Prob_2013 <- calculate_win_probability("SLN", "BOS", 2013, final_df)
SLN_W_Prob_2013
BOS_W_Prob_2013 <- calculate_win_probability("BOS", "SLN", 2013, final_df)
BOS_W_Prob_2013

#based on this, the win probability was more favorable for Boston since they had a higher
## win percentage, and they won the world series in 2013

### In 2014, NL was San Francisco giants (winner) and AL was Kansas City Royals
SFN_W_Prob_2014 <- calculate_win_probability("SFN", "KCA", 2014, final_df)
SFN_W_Prob_2014
KCA_W_Prob_2014 <- calculate_win_probability("KCA", "SFN", 2014, final_df)
KCA_W_Prob_2014
