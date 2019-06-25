#Starting from the beggining 
library("PlayerRatings")
library("dplyr")
library("ggplot2")
library("scales")
library("plotly")
library("stringr")

setwd("/media/mika/OS-Shared/temp/dump-20190623T084546Z-001/dump/Projects/PNWSmashDatabase")
source("RankingsAndGraphs/Elo_check.R")
setwd("SmashDatabase")
source("ChallongeAPI.R")
source("SmashggAPI.R")
source("../RankingsAndGraphs/Elo_check.R")
setwd("..")


#############CALCULATION 1 INCLUDING WGA COLLOSEUM ONLY #########################
#Set of challonge brackets needed for PR 
# cBrackets <- list("CSLSmashSEASingles", "WGAspr2")
# cBrackets2 <- list("RSU1", "TRC121318", "RSU2", "gig122018", "UMSSingles122218", "RSUS3", "gig122718", "ESSM28U", "RSU4", "gig1319", "RSU5", "wgac1", "UWUPR1", "gig11018", "RSU6")
# cBrackets3 <- list("wgac2", "gig11719", "UMSS11919", "RSU7", "wgac3", "RSU8", "wgac4", "gig13119", "RSU9", "wgac5", "gig2719", "wgac6", "ultimatethursday10", "RSU11", "wgac7", "ULTIMATETHURSDAY11", "ums3singles", "wgac8", "gig12")

cBrackets <- list("wgac11", "WGASSPR1", "WGAC12", "wgac13", "wgac14", "wgac15", "WGASSPR2", "wgac16", "wgac17_prime", "wgac18", "wgac19")
cBrackets2 <- list("GyromightsFinalPRSinglesBracket")
cBrackets3 <- list("wgac20")
#Set of SmashGG brackets eligible for PR 
# sggBrackets1 <- list("smash-homelessness-3", "orbitar-63", "orbitar-64", "orbitar-65", "orbitar-66", "orbitar-67") 
# sggBrackets <- list("orbitar-63", "orbitar-64", "orbitar-65", "evergreen-rising-5",  "smashworks-ultimate", "evergreen-rising-6")

sggBrackets1 <- list()
sggBrackets <- list()
#Create a new Match History
cMatches <- data.frame()
for (i in cBrackets) {
  print(i)
  temp <- formatted_matches(i) 
  cMatches <- rbind(cMatches, temp)
}

for (i in cBrackets2) {
  print(i)
  temp <- formatted_matches(i) 
  cMatches <- rbind(cMatches, temp)
}

for (i in cBrackets3) {
  print(i)
  temp <- formatted_matches(i) 
  cMatches <- rbind(cMatches, temp)
}

sggMatches <- data.frame()
for (i in sggBrackets) {
  print(i)
  temp <- unique(extractSetHistoryMultiple(i))
  sggMatches <- rbind(sggMatches, temp)
}

#Puts two histories together
Match_History <- unique(rbind(cMatches, sggMatches))
Match_History$player1_id <- tolower(str_replace_all(Match_History$player1_id, "[^[:alnum:]]", ""))
Match_History$player2_id <- tolower(str_replace_all(Match_History$player2_id, "[^[:alnum:]]", ""))
Match_History$player1_id[Match_History$player1_id == "mikazuchi"] = "mika"
Match_History$player2_id[Match_History$player2_id == "mikazuchi"] = "mika"
Match_History$player1_id[Match_History$player1_id == "sungchingchong"] = "sungchung"
Match_History$player2_id[Match_History$player2_id == "sungchingchong"] = "sungchung"
Match_History$player1_id[Match_History$player1_id == "sung"] = "sungchung" 
Match_History$player2_id[Match_History$player2_id == "sung"] = "sungchung" 
Match_History$player1_id[Match_History$player1_id == "jkeezy"] = "jtreezy"
Match_History$player2_id[Match_History$player2_id == "jkeezy"] = "jtreezy"
Match_History$player1_id[Match_History$player1_id == "cloutcobain"] = "wriggle"
Match_History$player2_id[Match_History$player2_id == "cloutcobain"] = "wriggle"
Match_History$player1_id[Match_History$player1_id == "bathsaltxangeiifff"] = "wriggle"
Match_History$player2_id[Match_History$player2_id == "bathsaltxangeiifff"] = "wriggle"
Match_History$player1_id[Match_History$player1_id == "bleggaman"] = "bleg"
Match_History$player2_id[Match_History$player2_id == "bleggaman"] = "bleg"
Match_History$player1_id[Match_History$player1_id == "kingkila"] = "kila"
Match_History$player2_id[Match_History$player2_id == "kingkila"] = "kila"


Match_History$player1_id[Match_History$player1_id == "snickelberry"] = "snickeldorf"
Match_History$player2_id[Match_History$player2_id == "snickelberry"] = "snickeldorf"

Match_History$player1_id[Match_History$player1_id == "snickledorf"] = "snickeldorf"
Match_History$player2_id[Match_History$player2_id == "snickledorf"] = "snickeldorf"

Match_History$player1_id[Match_History$player1_id == "trahkoopagod"] = "tkg"
Match_History$player2_id[Match_History$player2_id == "trahkoopagod"] = "tkg"

Match_History <- Match_History %>% arrange(date)
oor <- list("captainl", "bigd", "irakaz", "keoneon", "everest", "biggymouth", "kantrip", "focus", "firefly", "len", "austin")

for (player in oor) {
  Match_History <- Match_History %>% 
    filter(player1_id != player) %>% 
    filter(player2_id != player)
}

nonUltSingles <- list("Melee Singles", "Melee Doubles", "Ultimate Doubles", "PM Singles", "PM Doubles", "Super Smash Bros. Ultimate Doubles")
  
for (t in nonUltSingles) {
  Match_History <- Match_History %>% 
    filter(Tournament != t) 
}

#ELO STUFF
WWA <- create_region("WI19")
WWA_ratings <- glicko(create_game(0, 0, 0, 0))
matchHistory <- data.frame(pid = 0, Date = 0, NewRank = 0, VS = 0, Result = 0, Tournament_Name = 0, stringsAsFactors = FALSE)

player_ids = list()
for (row in 1:nrow(Match_History)) {
  WWA <- add_player(WWA, Match_History$player1_id[row])
  WWA <- add_player(WWA, Match_History$player2_id[row])
}

for (row in 1:nrow(Match_History)){
  if (!is.na(Match_History$player1_score[row]) && !is.na(Match_History$player2_score[row])) {
    if ((Match_History$player1_score[row] != -1) && (Match_History$player2_score[row] != -1)) {
      if (Match_History$player1_score[row] > Match_History$player2_score[row]) {
        winner <-  Match_History$player1_id[row]
        loser <- Match_History$player2_id[row]
        WWA_ratings <- report_game_by_tag(WWA_ratings, WWA, 0, winner, loser)
        winner_rank <- WWA_ratings$ratings$Rating[WWA_ratings$ratings$Player == player_id(WWA, winner)]
        loser_rank <- WWA_ratings$ratings$Rating[WWA_ratings$ratings$Player == player_id(WWA, loser)]
        ## update the history component accordingly 
        win_history <- data.frame(pid = winner, Date = Match_History$date[row] , NewRank = winner_rank, VS = loser, Result = "Win", Tournament_Name = Match_History$Tournament[row], stringsAsFactors = FALSE)
        loss_history <- data.frame(pid = loser, Date = Match_History$date[row] , NewRank = loser_rank, VS = winner, Result = "Loss", Tournament_Name = Match_History$Tournament[row], stringsAsFactors = FALSE)
        matchHistory <<- rbind(matchHistory, win_history)
        matchHistory <<- rbind(matchHistory, loss_history)
      }
      if (Match_History$player1_score[row] < Match_History$player2_score[row]) {
        winner <-  Match_History$player2_id[row]
        loser <- Match_History$player1_id[row]
        WWA_ratings <- report_game_by_tag(WWA_ratings, WWA, 0, winner, loser)
        winner_rank <- WWA_ratings$ratings$Rating[WWA_ratings$ratings$Player == player_id(WWA, winner)]
        loser_rank <- WWA_ratings$ratings$Rating[WWA_ratings$ratings$Player == player_id(WWA, loser)]
        ## update the history component accordingly 
        win_history <- data.frame(pid = winner, Date = Match_History$date[row] , NewRank = winner_rank, VS = loser, Result = "Win", Tournament_Name = Match_History$Tournament[row] , stringsAsFactors = FALSE)
        loss_history <- data.frame(pid = loser, Date = Match_History$date[row] , NewRank = loser_rank, VS = winner, Result = "Loss", Tournament_Name = Match_History$Tournament[row], stringsAsFactors = FALSE)
        matchHistory <<- rbind(matchHistory, win_history)
        matchHistory <<- rbind(matchHistory, loss_history)
      }
    }
  }
}

rankings <- WWA_ratings$ratings

for (name in rankings$Player) {
  rankings$Player[rankings$Player == name] <- WWA$IGN[WWA$Player == name]
}

matchHistory2 <- matchHistory[-c(1)]


# ###Adds wins and losses to each match 
# for (row in 1:nrow()) {
#   if(!is.na(Match_History$player1_score[row]) && !is.na(Match_History$player2_score[row])) {
#     if (Match_History$player1_score[row] > Match_History$player2_score[row]) {
#       winner <-  Match_History$player1_id[row]
#       loser <- Match_History$player2_id[row]
#     }
#     if (Match_History$player1_score[row] > Match_History$player2_score[row]) {
#       winner <-  Match_History$player1_id[row]
#       loser <- Match_History$player2_id[row]
#     }
#   }
# }



