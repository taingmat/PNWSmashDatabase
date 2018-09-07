
library(httr)
library(jsonlite)
library(dplyr)

###############################CHALLONGE API#####################################
setwd("~/../Desktop/Projects/PNWSmashDatabase/SmashDatabase")
source("keys.R")
username <- "Mikazuchi"
challonge_base <- paste0("https://Mikazuchi:", challonge_key, "@api.challonge.com/v1/")

##alias <- list(c("WeS | Mika", "Matt"), c("Kakai", "Kyra"))
screen_name <- c("Mika", "Kai")
name_key <- read.csv("name_key.csv", colClasses = c("NULL", NA, NA), stringsAsFactors = FALSE)
##Please maintain x = change_x format when calling this function 
new_alias <- function(screen_name, alias) {
  name_key2 <- data.frame(screen_name = screen_name, alias = alias, stringsAsFactors = FALSE)
  name_key = rbind(name_key, name_key2)
  return(name_key)
}

##If there is a name that doesn't exist in names already
##Prompt to add to an existing user or create a new one
##If the name isn't in screen_names, check to see if it's in alias's 
##If the name is in alias's change it to that of the screen name instead 
#Extracts a list of all tournaments on the challonge account
extract_tournamentlist <- function() {
  tournaments.get <- GET(paste0(challonge_base, "tournaments.json"), state = "all", accept_json())
  tournaments.body <- content(tournaments.get, "text")
  tournaments.table <- fromJSON(tournaments.body)
  tournaments <- data.frame(tournaments.table) 
  return(tournaments$tournament)
}

#Participants
extract_participants <- function(tournament_id) {
  participants.get <- GET(paste0(challonge_base, "tournaments/", tournament_id, "/participants.json"), accept_json())
  participants.body <- content(participants.get, "text")
  participants.table <- fromJSON(participants.body)
  participants <- data.frame(participants.table)
  return(participants$participant)
}

Participants <- extract_participants("EloTestMika1")

#Matches
extract_matches <- function(tournament_id) {
  participants <- extract_participants(tournament_id)
  matches.get <- GET(paste0(challonge_base, "tournaments/", tournament_id, "/matches.json"), accept_json())
  matches.body <- content(matches.get, "text")
  matches.table <- fromJSON(matches.body)
  matches <- data.frame(matches.table, row.names = NULL)
  #renames by id and tag
  for(i in participants$id) {
    temp = participants$name[participants$id == i]
    matches$match[matches$match == i] <- temp
    ##If theirs already a match change it 
    if(!(temp %in% name_key$alias)) {
      response <- readline(paste0("Is ", temp, " a new player? (hit y)"))
      if(response != "y") {
        screen_name <- readline(paste0("What is ", temp,"\'s actual tag? "))
        if(!(screen_name %in% name_key$alias)) {
          name_key <<- new_alias(screen_name, screen_name) 
        }
        name_key <<- new_alias(screen_name, temp)  
      }
      else {
        name_key <<- new_alias(temp, temp)  
      }
    }
    accurate_name <- name_key$screen_name[name_key$alias == temp]
    matches$match[matches$match == temp] <-  accurate_name
    Participants$name[Participants$name == temp] <<- accurate_name
  }
  return(matches$match)
}

extract_tournamentName <- function(tournament_url) {
  tournament.get <- GET(paste0(challonge_base, "tournaments/", tournament_url, ".json"), accept_json())
  tournament.body <- content(tournament.get, "text")
  tournament.table <- fromJSON(tournament.body)
  name <- tournament.table$tournament$name
  return(name)
}

extract_date <- function(tournament_url) {
  participants <- extract_participants(tournament_url)
  tournament.get <- GET(paste0(challonge_base, "tournaments/", tournament_url, ".json"), accept_json())
  ##The Date has been properly Extracted :)
  ##Now Convert that into a date format that's usable ``
  tournament.body <- content(tournament.get, "text")
  tournament.table <- fromJSON(tournament.body)
  tournament_date <- as.Date(substr(tournament.table$tournament$updated_at ,0, 10)) 
  numeric_date <- as.numeric(tournament_date)
  return(tournament_date)
}

formatted_matches <- function(tournament_id) {
  name = extract_tournamentName(tournament_id)
  date = extract_date(tournament_id)
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
