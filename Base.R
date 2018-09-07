
############################################INITIALIZATION########################################
library("PlayerRatings")
library("dplyr")
library("ggplot2")
library("scales")
library("plotly")

setwd("~/../Desktop/Projects/PNWSmashDatabase")
source("RankingsAndGraphs/Elo_check.R")

PR_Brackets <- list("supportpriority3", "supportpriority3V2", "RIPANTi")
PR_Brackets2 <- list("Wgasmashsept2017", "wgasmashfall2017pr3", "wgasmash4octpr")
UW_PR_Spring2018 <- create_region_ranking("UWFALL2017", PR_Brackets)
write.csv(UW_PR_Spring2018, file = "~/../Desktop/UW_Spring_ELO_S4.csv", row.names = FALSE)
write.csv(name_key, file = "name_key.csv", row.names = FALSE)
SOWA_Brackets <- list("GiG905",
                       "GiG907",
                       "GiG912",
                       "GiGMM2Y",
                       "gig103",
                       "gig105",
                       "gig1102",
                       "GiG1114",
                       "GiG1116",
                       "gig1121",
                       "gig1128",
                       "gig1205")
SOWA_PR_Fall2017 <- create_region_ranking("2017", SOWA_Brackets)
##This is made within elo check when the region ranking is created
##It's the current EloHistory 
SOWAHistory <- EloHistory
write.csv(SOWAHistory, file = "RegionHistories/SOWAHistory.csv") 

Test_Brackets <- list("Mika71618a", "Mika71618b")
Test_Ranking <- create_region_ranking("2017", Test_Brackets)
Test_History <- EloHistory
write.csv(Test_History, file = "RegionHistories/TestHistory.csv")
##########################################Example of extracting brackets#########################################

# Mika_tournaments <- extract_tournamentlist()
# PR1_matches <- extract_matches("Wgasmashsept2017")
# PR1_participants <- extract_participants("Wgasmashsept2017") 
# PR2_matches <- extract_matches("wgasmash4octpr")
# PR2_participants <- extract_participants("wgasmash4octpr") 
# UW_PR <- rbind(PR1_matches, PR2_matches)

############################Example of doing things manually#################################

# UW_Smash_PlayerRatings <- create_region("UW_Smash")
# region_name <- "UW_Smash"
# bracket_links <-  list("Mika71618a", "Mika71618b")
# player_ids <- create_region(region_name)
# region_ratings <- glicko(create_game(0, 0, 0, 0))
# for(i in bracket_links) {
#   extract_matches(i)
# }
# for(i in name_key$screen_name) {
#   player_ids <- add_player(player_ids, i)
# }
# View(player_ids)
# for(i in bracket_links) {
#   region_ratings  <- report_challonge_tournament(region_ratings , player_ids, 1, i)
# }
# status_temp <- glicko_to_status(region_ratings)
# rating_with_id <- merge(player_ids, status_temp, by = "Player")
# rating_with_id <- rating_with_id %>% filter(Player != 0) %>%
#   mutate(win_rate = (Win / Games)*100)
# 
#UW_ratings <- glicko(create_game(0, 0, 0, 0))
#UW_Smash_PlayerRatings <- add_player(UW_Smash_PlayerRatings, "Kai")
 ##adds players to region
#player_list <- list("Mika", "AQT", "Yayzors", "Pokalink", "Kai")
#UW_Smash_PlayerRatings <- add_players(UW_Smash_PlayerRatings, player_list)
  #print(UW_Smash_PlayerRatings)
#UW_ratings <- glicko(create_game(0, 0, 0, 0))
#UW_ratings <- report_game_by_tag(UW_ratings, UW_Smash_PlayerRatings, 1, "Mika", "AQT")
#print(UW_ratings)

 MikaRankHistory <- SOWAHistory %>% filter(pid =='Mika')
 a <- ggplot( MikaRankHistory, aes(x = as.Date(Date, origin="1970-01-01"), y = rank)) + 
   geom_line() +
   scale_x_date(labels = date_format("%d-%m-%Y"))
 
 MikaRankGraph <- ggplotly(a)
 
 SetHistory <- rbind(Challonge_example, SmashGGExample)
