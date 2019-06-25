
library(shiny)
library(dplyr)
library(data.table)
library(ggplot2)
library(plotly)
##source("../SmashDatabase/keys.R")
history <- read.csv("../Data/RegionRankings/history.csv")
players <- history$pid

#source("../Elo_check.R")
#source("Challonge_api.R")
#username <- "Mikazuchi"
#challonge_base <- paste0("https://Mikazuchi:", challonge_key, "@api.challonge.com/v1/")


shinyServer(function(input, output) {
  
  output$test <- renderDataTable({
    history <- read.csv("../Data/RegionRankings/SOWAHistory.csv")
    history  %>% filter(pid == input$player)
  })

  output$matchHistory <- renderDataTable({
    history <- read.csv("../Data/RegionRankings/history.csv")
    history  %>% filter(pid == input$player)
  })
  
  output$choose_player <- renderUI ({
    history <- read.csv("../Data/RegionRankings/SOWAHistory.csv")
    selectInput("player", label = h3("Select Player"), 
                choices = players) 
  })
  
  output$visual <- renderPlotly({
    history <- read.csv("../Data/RegionRankings/SOWAHistory.csv")
    history <- history  %>% filter(pid == input$player)
      history$Date = as.Date(history$Date, origin="1970-01-01") 
      a <- ggplot(history, aes(x = Date, y = rank, label = tournament)) +
      geom_line() + 
      labs(x = "Date")
      ggtitle(paste(input$player, history)) 
      
        # + scale_x_date(labels = date_format("%d-%m-%Y"))
      
      g <- ggplotly(a)
      
      # style(
      #   g, traces = 2, 
      #   mode = "markers+lines",
      #   hoverinfo = "x+y", 
      #   fillcolor = "transparent",
      #   hoverlabel = list(bgcolor = "white")
      # )
      
      g
  })

  #output$matches <- renderDataTable({ extract_matches("wgasmashfall2017pr3")
  #})
  
  # Single tournament match history
  # output$downloadSingleMatches <- downloadHandler(
  #   filename = function(){paste(input$text, ".csv", sep = "")},
  #   content = function(file){
  #     write.csv(extract_matches(input$text), file)
  #   }
  # )
  # 
  # # Single tournament ranking
  # output$downloadSingleRanking <- downloadHandler(
  #   filename = function(){paste(input$text, ".csv", sep = "")},
  #   content = function(file){
  #     write.csv(create_region_ranking("UWFALL2017", list(input$text)), file)
  #   }
  # )
  # 
  # #Multi tournamnet match history   
  # output$downloadMultiMatches <- downloadHandler(
  #   filename = function(){paste(input$text, ".csv", sep = "")},
  #   content = function(file){
  #     write.csv(extract_matches(input$text), file)
  #   }
  # )
  # fileInput <- reactive ({
  #   read.csv(input$file_1)
  # })
  # #Multi tournament ranking 
  # output$downloadMultiRanking <- downloadHandler(
  #   filename = function(){paste(input$text, ".csv", sep = "")},
  #   content = function(file){
  #     write.csv(create_region_ranking("MultiRankingTest", fileInput$brackets), file)
  #   }
  # )
})