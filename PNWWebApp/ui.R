#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(plotly)
history <- read.csv("../Data/RegionRankings/history.csv")
# Define UI for application that draws a histogram
shinyUI(fluidPage( theme = shinytheme("superhero"),
  
  # Application title
  titlePanel("PNW Ranking"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      # selectInput("region", label = h3("Select Region"), 
      #             choices = c( "SOWA" = "SOWAHistory.csv", 
      #                          "Test" = "TestHistory.csv"), 
      #             selected = "SOWAHistory.csv"),
      # 
      uiOutput("choose_player", selected = "Mika")),
      
      # textInput("text", label = h3("Single Bracket"), value = "Enter text...")
      # ,
      # downloadButton("downloadSingleMatches", label = "Download Matches")
      # ,
      # downloadButton("downloadSingleRanking", label = "Download Ranking", class = NULL)
      # ,
      # fileInput("file_1", "Multiple Brackets", multiple = TRUE, accept = NULL, width = NULL,
      #         buttonLabel = "Browse...", placeholder = "No file selected")
      # , 
      # downloadButton("downloadMultiMatches", label = "Download Matches")
      # ,
      # downloadButton("downloadMultiRanking", label = "Download Ranking", class = NULL)
      # ),
    
    
    
    #Show a plot of the generated distribution
    mainPanel(
      plotlyOutput("visual"),
      dataTableOutput("test"),
      dataTableOutput("matchHistory")
    )
  )
))
