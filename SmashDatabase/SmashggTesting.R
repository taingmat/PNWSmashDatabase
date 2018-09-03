#test playground 

library(httr)
library(jsonlite)
library(dplyr)

test.Get <- GET("https://api.smash.gg/phase_group/486502?expand[]=sets&expand[]=seeds", state = "all", accept_json())
test.body <- content(test.Get, "text")
test.table <- fromJSON(test.body, flatten = TRUE)

 Hmm <- test.table$entities$sets
 
 seeds <- test.table$entities$seeds ##Find a way to automate id collection
 seedmute <- seeds$mutations        ##Use these ids and join a table of name-id pairs 
 seedentrants <- seedmute$entrants
 testt <- seedentrants$`1336182`
 test2<- seeds$mutations$participants
 Pokepen <- testt %>% select(name, participantIds, id, initialSeedNum) %>% filter(!is.na(name))
 

##renameSingle <- function(id) {
  #info <- temp %>% select(name, participantIds, id, initialSeedNum) %>% filter(!is.na(name))
#  return(temp)
#}