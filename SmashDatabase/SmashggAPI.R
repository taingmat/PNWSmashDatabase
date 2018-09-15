#test playground 

library(httr)
library(jsonlite)
library(dplyr)
library(rlist)

setwd("~/../Desktop/Projects/PNWSmashDatabase/Data/")

##Smash gg dates: as.Date(501198, origin="646-04-27") 

##Need a table with player IDs
##Will check for new players and confirm their existance

##Need a table with tournaments
tournaments <- read.csv("Tournaments.csv", stringsAsFactors = FALSE)
##Add to the original table 
##If the tournament is already in the original then don't be redundant 

#Read in a previous set history 

##Given a tag returns the tag with sponsors removed
#Sponsors are any text that comes before the "|" symbol
removeSponsors <- function(x) {
  while(grepl("\\|", x)) {
    x = sub("^[^|]*", "", x)
    x = substring(x, 2)
  } 
  return(x)
}

#Given a phase id 
#Returns a dataframe of all sets played during that phase
SetsFromPhaseId <- function(phaseID) {
  ##Collects entrants and their player IDS
  entrants.Get <- GET(paste0("https://api.smash.gg/phase_group/", phaseID,"?expand[]=entrants"), state = "all", accept_json())
  entrants.body <- content(entrants.Get, "text")
  entrants.table <- fromJSON(entrants.body, flatten = TRUE)
  entrants <- entrants.table$entities$entrants 
  ##Collects all sets played throughout the tournament 
  test.Get <- GET(paste0("https://api.smash.gg/phase_group/", phaseID, "?expand[]=sets&expand[]=seeds"), state = "all", accept_json())
  test.body <- content(test.Get, "text")
  test.table <- fromJSON(test.body, flatten = TRUE)
  SetHistory <- test.table$entities$sets 
  SetHistory$date = as.Date(SetHistory$completedAt /60/ 60/ 24, origin="1970-01-01") 
  SetHistory$numericDate = as.integer(as.numeric(SetHistory$date))
  SetHistory = SetHistory %>%
    select(id, eventId, phaseGroupId, entrant1Id, entrant2Id, entrant1Score, entrant2Score, date, numericDate) %>%
    filter(!(is.na(entrant1Id) | is.na(entrant2Id)))
  #Renames all of the entrants by name 
  for(i in SetHistory $ entrant1Id) {
    newTag = removeSponsors(entrants$name[i == entrants$id])
    SetHistory$entrant1Id[i == SetHistory$entrant1Id] = newTag
  }
  
  for(i in SetHistory $ entrant2Id) {
    newTag = removeSponsors(entrants$name[i == entrants$id])
    SetHistory$entrant2Id[i == SetHistory$entrant2Id] = newTag
  }
  
  return(SetHistory)
}

#Accepts a tournamentid
#Returns all of the phase ids from the given tournament 
extract_Phase_Ids <- function(tournamentId) {
  stars.Get <- GET(paste0("https://api.smash.gg/tournament/", tournamentId, "?expand[]=groups"), state = "all", accept_json())
  stars.body <- content(stars.Get, "text")
  stars.table <- fromJSON(stars.body, flatten = TRUE)
  
  return(stars.table$entities$groups$id)
}

##given a list of tournament names, returns all the phase ids from those tournaments 
extractMultiplePhaseIds <- function(tournamentIds) {
  phaseIds <- list()
  for(tournamentId in tournamentIds) {
    phaseIds <- c(phaseIds, extract_Phase_Ids(tournamentId))
  }
  return(phaseIds)
}


##Includes phase_Id, tournament name, and start date
phaseKey <- function(tournamentName) {
  stars.Get <- GET(paste0("https://api.smash.gg/tournament/", tournamentName, "?expand[]=groups&expand[]=phase&expand[]=event"), state = "all", accept_json())
  stars.body <- content(stars.Get, "text")
  stars.table <- fromJSON(stars.body, flatten = TRUE)
  expandGroups <- stars.table$entities$groups
  expandGroups$tournamentName <- NA 
  expandGroups$slug <- NA
  expandPhase <- stars.table$entities$phase
  expandEvent <- stars.table$entities$event
  for(i in expandGroups$phaseId) {
    expandGroups$tournamentName[expandGroups$phaseId == i] <- expandEvent$name[ expandEvent$id == expandPhase$eventId[expandPhase$id == i]]
    expandGroups$slug[expandGroups$phaseId == i] <- expandEvent$slug[ expandEvent$id == expandPhase$eventId[expandPhase$id == i]]
  }
  return(expandGroups %>% select(id, phaseId, tournamentName, slug))
}

SetHistory <- data.frame(Tournament = character(), 
                         date = character(), 
                         player_1id = character(),
                         player_2id = character(), 
                         player_1score = character(), 
                         player2_score = character(), 
                         url = character())

##Extracts the setHistory of multiple tournaments given their phase ids
extractSetHistoryMultiple <- function(tournamentIds) {
  newTournaments <- data.frame(type = character(), 
                               name = character(), 
                               date = character(), 
                               url = character(), 
                               pr = character(), 
                               region = character())
  for (tournamentId in tournamentIds) {
    #check to see if it's not in the set history 
    if (!(tournamentId %in% tournaments$name)) {
      print(paste0(tournamentId, " has not yet been processed"))
      newTournament <- data.frame(type = NA, 
                                   name = tournamentId, 
                                   date = NA, 
                                   url = NA, 
                                   pr = NA, 
                                   region = NA)
      newTournaments <- rbind(newTournaments, newTournament)
      tournaments <<- rbind(newTournaments, tournaments)
      
    }
  }
  phaseIds <- extractMultiplePhaseIds(newTournaments) 
  testTable <- data.frame(id = character(), 
                          eventId = character(), 
                          phaseGroupId = character(), 
                          entrant1Id = character(), 
                          entrant2Id = character(), 
                          winnerId = character(), 
                          loserId = character(), 
                          entrant1Score = character(), 
                          entrant2Score = character(),
                          date = character(), 
                          numericDate = character())
  for(tournamentURL in phaseIds) {
    print(tournamentURL)
    Sets <- SetsFromPhaseId(tournamentURL)
    testTable = rbind(testTable, Sets)
  }
  testTable$slug = NA
  testTable$tournamentName = NA
  for(tournamentId in tournamentIds) {
    print(tournamentId)
    key = phaseKey(paste0(tournamentId))
    for(i in key$id) {
      testTable$slug[testTable$phaseGroupId == i] = paste0("https://smash.gg/", key$slug[key$id == i])
      testTable$tournamentName[testTable$phaseGroupId == i] = key$tournamentName[key$id == i]
    }
  }
  testTable <- testTable %>% select(Tournament = tournamentName, 
                              date = date, 
                              player1_id = entrant1Id, 
                              player2_id = entrant2Id, 
                              player1_score = entrant1Score, 
                              player2_score = entrant2Score, 
                              url = slug)
  print(testTable)
  SetHistory <<- rbind(testTable, SetHistory)
  write.csv(SetHistory, file = "SetHistory.csv", row.names = FALSE)
  return(SetHistory)
}

mergePlayers <- function(originalTag, alias, SetHistory) {
  SetHistory$player_1id[player_1id == alias] == originalTag
  SetHistory$player_2id[player_2id == alias] == originalTag
  return(SetHistory)
}

##Must have X
tournamentNames <- list("orbitar-stars-1", "oribtar-43")
# x <- 0
# while(x < 43) {
#   y <- x + 11
#   if(y >= 40) {
#     tournamentNames[x + 2] = paste0("orbitar-", y + 1)
#   }
#   else {
#     tournamentNames[x + 2] = paste0("orbitar-", y)
#   }
#   x <- x + 1
# }

SmashGGExample <- extractSetHistoryMultiple(tournamentNames)



#Take in a list of tournaments 
#check the slug to see if it's already been added to set history 
#if it's not then add it to the history
#PR should default to FALSE
#region should be passed in at the beggining 