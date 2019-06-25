
library(httr)
library(jsonlite)
library(dplyr)

##Need a table with player IDs
##Will check for new players and confirm their existance

##Need a table with tournaments
##Add to the original table 
##If the tournament is already in the original then don't be redundant 

#Initial Setup
setwd("/media/mika/OS-Shared/temp/dump-20190623T084546Z-001/dump/Projects/PNWSmashDatabase/SmashDatabase/")
source("keys.R")
username <- "Mikazuchi"
challonge_base <- paste0("https://Mikazuchi:", challonge_key, "@api.challonge.com/v1/")
screen_name <- c("Mika", "Kai")
name_key <- read.csv("name_key.csv", colClasses = c("NULL", NA, NA), stringsAsFactors = FALSE)

##Accepts the original tag of the player, and a different tag that they have used
##Adds a new name to the name key
#The name key is used to ensure that even if a player uses differing tags, their match history is tracked under their ID
new_alias <- function(screen_name, alias) {
  name_key2 <- data.frame(screen_name = screen_name, alias = alias, stringsAsFactors = FALSE)
  name_key = rbind(name_key, name_key2)
  return(name_key)
}

#Accepts a players tag
#Returns a version of the players tag with all prefixes removed  
removeSponsors <- function(tag) {
  while(grepl("\\|", tag)) {
    tag = sub("^[^|]*", "", tag)
    tag = substring(tag, 2)
  } 
  return(tag)
}

#Returns a list of all tournaments on the challonge account
extract_tournamentlist <- function() {
  tournaments.get <- GET(paste0(challonge_base, "tournaments.json"), state = "all", accept_json())
  tournaments.body <- content(tournaments.get, "text")
  tournaments.table <- fromJSON(tournaments.body)
  tournaments <- data.frame(tournaments.table) 
  return(tournaments$tournament)
}

#Given a tournament id returns all of the players who competed in that tournament
extract_participants <- function(tournament_id) {
  participants.get <- GET(paste0(challonge_base, "tournaments/", tournament_id, "/participants.json"), accept_json())
  participants.body <- content(participants.get, "text")
  participants.table <- fromJSON(participants.body)
  participants <- data.frame(participants.table)
  return(participants$participant)
}

#Given a tournament id
##returns a data frame of all of the matches that occured at that tournamnet 
extract_matches <- function(tournament_id) {
  participants <- extract_participants(tournament_id)
  # participant_Record <- participants
  matches.get <- GET(paste0(challonge_base, "tournaments/", tournament_id, "/matches.json"), accept_json())
  matches.body <- content(matches.get, "text")
  matches.table <- fromJSON(matches.body)
  matches <- data.frame(matches.table, row.names = NULL)
  #renames by id and tag
  for(i in participants$id) {
    # participant_Record <- participant_Record
    temp = removeSponsors(participants$name[participants$id == i])
    matches$match[matches$match == i] <- temp
    #If theirs already a match change it
    if(!(temp %in% name_key$alias)) {
     # response <- readline(paste0("Is ", temp, " a new player? (hit y)"))
     # if(response != "y") {
     #   screen_name <- readline(paste0("What is ", temp,"\'s actual tag? "))
     #   if(!(screen_name %in% name_key$alias)) {
     #     name_key <<- new_alias(screen_name, screen_name)
     #   }
     #   name_key <<- new_alias(screen_name, temp)
     # }
     # else {
    name_key <<- new_alias(temp, temp)
     # }
    }
    accurate_name <- name_key$screen_name[name_key$alias == temp]
    matches$match[matches$match == temp] <-  accurate_name
    # print(participant_Record)
    # participant_Record$name[participant_Record$name == temp] <<- accurate_name
  }
  return(matches$match)
}

##Given a tournament if returns the name of the tournament
extract_tournamentName <- function(tournament_table) {
  name <- tournament_table$tournament$name
  return(name)
}

#Given a tournament it returns the date the tournament occured on
extract_date <- function(tournament_table) {
  tournament_date <- as.Date(substr(tournament_table$tournament$updated_at ,0, 10)) 
  numeric_date <- as.numeric(tournament_date)
  return(tournament_date)
}

#Given a tournament id returns a formattted table of that tournaments sets 
formatted_matches <- function(tournament_id) {
  #Get to the tournament table
  participants <- extract_participants(tournament_id)
  tournament.get <- GET(paste0(challonge_base, "tournaments/", tournament_id, ".json"), accept_json())
  tournament.body <- content(tournament.get, "text")
  tournament.table <- fromJSON(tournament.body)
  #Pass that on to all of the other functions instead of the tournament_id
  name = extract_tournamentName(tournament.table)
  date = extract_date(tournament.table)
  #now let's get all those matches
  matches <- extract_matches(tournament_id)
  matches <- matches %>% select(id, Tournament = tournament_id, player1_id, player2_id, scores_csv)
  matches$Tournament = name; 
  matches$player1_score <- NA 
  matches$player2_score <- NA
  matches$url <- paste0("https://challonge.com/", tournament_id)
  for (i in matches$scores_csv) {
    temp <- unlist(strsplit(i, "-"))
    matches$player1_score[matches$scores_csv == i] <- temp[1]
    matches$player2_score[matches$scores_csv == i] <- temp[2]
  }
  matches$date <- NA
  matches$date <- date
  matches$numericDate <- as.numeric(date)  
  ##temporarily removed numericDate
  matches <- matches %>% select(Tournament, date, player1_id, player2_id, player1_score, player2_score, url)
  return(matches)
}

Challonge_example <- formatted_matches("Mika71618b") 
PR2_Brackets <- formatted_matches("supportpriority3")

                                 