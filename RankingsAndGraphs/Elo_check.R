#About this project#

#Specifically uses glicko to rank players within regions
#Uses head to head matches to calculate rating

#############################################USEFUL DEFINITIONS ##################################
#glicko: A type of player rating system
#region: A dataframe which contains players of a region
#ELO: A player's relative ranking
#Deviation: Certainty of a players ranking


############################################TO_DO_LIST############################################

###########CREATING THE DATABASE###############
#Create a working date function to timestamp games
#Region ID's?

############################################INITIALIZATION########################################
library("PlayerRatings")
library("dplyr")
library("ggplot2")

setwd("/media/mika/OS-Shared/temp/dump-20190623T084546Z-001/dump/Projects/PNWSmashDatabase")
setwd("SmashDatabase")
source("ChallongeAPI.R")
setwd("..")
#########################################UNIVERSAL VARIABLES#######################################
regions <- list(NULL)
matchHistory <- data.frame(pid = 0, Date = 0, NewRank = 0, VS = 0, Result = 0, Tournament_Name = 0, stringsAsFactors = FALSE)
############################################FUNCTIONS FOR DATAFRAMES##############################################

###Creates a new region of players
create_region <- function(region_name) {
  IGN <- region_name
  Player <- 0
  player_info <- data.frame(IGN, Player, stringsAsFactors = FALSE)
  return(player_info)
}

###Adds a new player to the database if they don't already exist
add_player <- function(region, tag) {
  IGN <- tag
  Player <- nrow(region)
  if(!(tag %in% region$IGN)) {
      player_info <- data.frame(IGN, Player, stringsAsFactors = FALSE)
      region <- rbind(player_info, region)
  }
  return(region)
}

###Adds a list of new players to the database
add_players <- function(region, tags.list) {
  if(length(tags.list > 0)) {
    for(i in tags.list) {
      region <- add_player(region, i)
    }
  }
  return(region)
}

###Create a dataframe for a game (time/player1id/player2id/winner)
### 1 = player 1 win
### 0 = player 2 win
create_game <- function(time, player1_id, player2_id, winner) {
  game <- data.frame(time, player1_id, player2_id, winner)  
  return(game)  
}

###Converts a glicko list into a dataframe that can be used as a status
glicko_to_status <- function(glicko) {
  status <- data.frame(glicko[1])
  colnames(status) <- substring(colnames(status), 9)
  return(status) 
}

###Adds the results of a game to a pre-existing glicko
###The glicko ratings need to exist prior
#######Add an if statement to test if the region_ratings is empty
report_game <- function(region_ratings, date, winner_id, loser_id) {
  region_ratings_status <- glicko_to_status(region_ratings)
  game <- create_game(date, winner_id, loser_id, 1)
  region_ratings <- glicko(game, status = region_ratings_status, init = c(2200, 300), gamma = 0, cval = 15, history = TRUE, sort = TRUE)
  return(region_ratings)
}

###Reports games to a rating given two tags, 
###the first tag is the winner the second is the loser, game count doesn't matter
report_game_by_tag <- function(region_ratings, player_list, date, winner_tag, loser_tag) {
  region_ratings_status <- glicko_to_status(region_ratings)
  game <- create_game(date, player_id(player_list, winner_tag),  player_id(player_list, loser_tag), 1)
  region_ratings <- glicko(game, status = region_ratings_status, init = c(2200, 300), gamma = 0, cval = 15, history = TRUE, sort = TRUE)
  return(region_ratings)
}

###returns player id given a region, and tag 
player_id <- function(region, tag) {
  id <- region %>%
    filter(IGN == tag) %>%
    select(Player)
  return(id$Player[1])
}

################################################CHALLONGE RELATED FUNCTIONS#################################
#reports all matches from a challonge tournament to a rating
#The ratings must already exist
#Must create  a player list seperately 
report_challonge_tournament <- function(region_ratings, player_list, date, tournament_url) {
  match.df <- extract_matches(tournament_url)
  participants <<- extract_participants(tournament_url)
  name <- extract_tournamentName(tournament_url)
  ## Extract the new ranks of each player
  for(i in 1:nrow(match.df)) {
    region_ratings <- report_game_by_tag(region_ratings, player_list, 0, match.df$winner_id[i], match.df$loser_id[i])
    
    winner_rank <- region_ratings$ratings$Rating[region_ratings$ratings$Player == player_id(player_list, match.df$winner_id[i])]
    loser_rank <- region_ratings$ratings$Rating[region_ratings$ratings$Player == player_id(player_list, match.df$loser_id[i])]
  ## update the history component accordingly 
    win_history <- data.frame(pid = match.df$winner_id[i], Date = date, NewRank = winner_rank, VS = match.df$loser_id[i], Result = "Win", Tournament_Name = name, stringsAsFactors = FALSE)
    loss_history <- data.frame(pid = match.df$loser_id[i], Date = date, NewRank = loser_rank, VS = match.df$winner_id[i], Result = "Loss", Tournament_Name = name, stringsAsFactors = FALSE)
    matchHistory <<- rbind(matchHistory, win_history)
    matchHistory <<- rbind(matchHistory, loss_history)
  }
  for(i in participants$name) {
    NewRank <- data.frame(pid = name_key$screen_name[name_key$alias == i], rank = region_ratings$ratings$Rating[region_ratings$ratings$Player == player_id(player_list, name_key$screen_name[name_key$alias == i])], Date = date, tournament = name)
    EloHistory <<- rbind(EloHistory, NewRank)
  }
    #return region with added matches to ranking
  return(region_ratings)
}

#
create_region_ranking_full <- function(region_name, bracket_links) {
  region <- create_region_ranking(region_name, bracket_links[1])
  for(i in bracket_links[-1]) {
    region <- update_region_ranking(region, i) 
  }
  return(region)
}

#Helper method that creates the initial region from a single bracket link
create_region_ranking <- function(region_name, bracket_links) {
  player_ids <- create_region(region_name)
  region_ratings <- glicko(create_game(0, 0, 0, 0))
  for(i in bracket_links) {
    extract_matches(i)
  }
  for(i in name_key$screen_name) {
    player_ids <- add_player(player_ids, i)
  }
  View(player_ids)
  EloHistory <<- data.frame(pid = character(), rank = character(), Date = character(), tournament = character())
  for(i in bracket_links) {
    participants <- extract_participants(i)
    tournament.get <- GET(paste0(challonge_base, "tournaments/", i, ".json"), accept_json())
    ##The Date has been properly Extracted :)
    ##Now Convert that into a date format that's usable ``
    tournament.body <- content(tournament.get, "text")
    tournament.table <- fromJSON(tournament.body)
    tournament_date <- as.Date(substr(tournament.table$tournament$updated_at ,0, 10)) 
    numeric_date <- as.numeric(tournament_date)
    
    
    ##Trying to create a history component
    region_ratings  <- report_challonge_tournament(region_ratings , player_ids, tournament_date, i)
  }
  status_temp <- glicko_to_status(region_ratings)
  rating_with_id <- merge(player_ids, status_temp, by = "Player")
  rating_with_id <- rating_with_id %>% filter(Player != 0) %>%
    mutate(win_rate = (Win / Games)*100)
  return(rating_with_id)
}

update_region_ranking <- function(initial_region, region_rating, bracket_link) {
}
