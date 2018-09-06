#test playground 

library(httr)
library(jsonlite)
library(dplyr)



SetsFromPhaseId <- function(tournamentID) {
  ##tournamentID <- 195176
  ##Collects entrants and their player IDS
  entrants.Get <- GET(paste0("https://api.smash.gg/phase_group/", tournamentID,"?expand[]=entrants"), state = "all", accept_json())
  entrants.body <- content(entrants.Get, "text")
  entrants.table <- fromJSON(entrants.body, flatten = TRUE)
  
  entrants <- entrants.table$entities$entrants 
  ##%>% 
    ##select(id, eventId, participantIds, name)
  
  
  ##Collects all sets played throughout the tournament 
  test.Get <- GET(paste0("https://api.smash.gg/phase_group/", tournamentID, "?expand[]=sets&expand[]=seeds"), state = "all", accept_json())
  test.body <- content(test.Get, "text")
  test.table <- fromJSON(test.body, flatten = TRUE)
  
  SetHistory <- test.table$entities$sets %>%
    select(id, eventId, phaseGroupId, entrant1Id, entrant2Id, winnerId, loserId, entrant1Score, entrant2Score) %>%
    filter(!(is.na(winnerId) | is.na(loserId)))
  
  for(i in SetHistory $ winnerId) {
    SetHistory$winnerId[i == SetHistory$winnerId] = entrants$name[i == entrants$id]
  }
  
  for(i in SetHistory $ loserId) {
    SetHistory$loserId[i == SetHistory$loserId] = entrants$name[i == entrants$id]
  }
  
  return(SetHistory)
}

extract_Phase_Ids <- function(tournamentURL) {
  stars.Get <- GET(paste0("https://api.smash.gg/tournament/", tournamentURL, "?expand[]=groups"), state = "all", accept_json())
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


##Extracts the setHistory of multiple tournaments given their phase ids
extractSetHistoryMultiple <- function(tournamentIds) {
  phaseIds <- extractMultiplePhaseIds(tournamentIds) 
  testTable <- data.frame(id = character(), 
                          eventId = character(), 
                          phaseGroupId = character(), 
                          entrant1Id = character(), 
                          entrant2Id = character(), 
                          winnerId = character(), 
                          loserId = character(), 
                          entrant1Score = character(), 
                          entrant2Score = character())
  for(tournamentURL in phaseIDs) {
    Sets <- SetsFromPhaseId(tournamentURL)
    testTable = rbind(testTable, Sets)
  }
  return(testTable)
}

tournamentNames <- list("orbitar-stars-1", "orbitar-34")
yip <- extractSetHistoryMultiple(tournamentNames)



