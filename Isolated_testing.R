############################################INITIALIZATION########################################
library("PlayerRatings")
library("dplyr")
library("ggplot2")

source("RankingsAndGraphs/Elo_check.R")
source("SmashDatabase/Challonge_api.R")
#Create UW_SMASH

##Trying to create a working date component to be used with Glicko 
tournament_id <- "Mika71618"
participants <- extract_participants(tournament_id)
tournament.get <- GET(paste0(challonge_base, "tournaments/", tournament_id, ".json"), accept_json())
##The Date has been properly Extracted :)
tournament.get$date
##Now Convert that into a date format that's usable 
tournament_date <- as.Date(substr(tournament.get$date,0, 10)) 
b <- as.numeric(tournament_date)
as.Date(b, origin="1970-01-01")
##Trying to create a history component