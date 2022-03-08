library(shiny)
library(tidyverse)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(robotstxt)
library(rvest)
library(DT)
library(leaflet)
library(leaflet.extras)
library(maps)
library(maptools)
library(sp)
library(rgeos)

######Web Scraping
page <- "https://github.com/XLiu-02/travel-index/blob/main/data"
paths_allowed(page)
df <- read_html(page) %>%
  html_nodes(".js-file-line") %>%
  html_text()

travel <- tibble(df)
travel <- travel[-1,]
travel <- travel %>% 
  separate(col = df,into = c("Country","CPI","Safety","Region","Goal","Terrain"),sep = ",")
travel[,c(2,3)] <- sapply(travel[,c(2,3)], as.numeric)
travel$Region <- gsub("^.{0,1}", "", travel$Region)
travel$Goal <- gsub("^.{0,1}", "", travel$Goal)
travel$Terrain <- gsub("^.{0,1}", "", travel$Terrain)
countries <- as.data.frame(travel)

######UI
ui<- shinyUI(
  fluidPage(
    theme = shinytheme("darkly"),
    titlePanel("Recommended Country to Travel"),
    
    #inputs 
    fluidRow(
      div(class="span4", radioButtons("Region","Region:", 
                                      c("All",unique(as.character(countries$Region))))),
      div(class="span4",selectInput("Goal","Type:", 
                                    c("All",unique(as.character(countries$Goal))))),
      div(class="span4",selectInput("Terrain","Terrain:", 
                                    c("All",unique(as.character(countries$Terrain))))),
      div(class="span4",selectInput("Safety","Safety (1-10):", 
                                    c("All",1:10))),
      div(class="span4",sliderInput("CPI","Cost:",
                                    min = 0, max = 100, value = 80))
    ),
    #tabs
    mainPanel(
      tabsetPanel(
        tabPanel("Table",textOutput("text"),tableOutput("table")),
        tabPanel("Map",leafletOutput("map"))
      )
    ) 
  )
)

#####Server
server<- shinyServer(function(input, output) {
  # Filter data based on selections
  filter1 <- reactive({
    data <- countries
    if (input$Region != "All"){
      data <- data[data$Region == input$Region,]
    }
    if (input$Goal != "All"){
      data <- data[data$Goal == input$Goal,]
    }
    if (input$Terrain != "All"){
      data <- data[data$Terrain == input$Terrain,]
    }
    if (input$Safety != "All"){
      data <- data[data$Safety >= input$Safety,]
    }
    if (input$CPI >= data$CPI){
      data <- data[data$CPI <= input$CPI,]
    }
    data
  }) 
  
  filter2 <- reactive({
    data <- countries
    if (input$Region != "All"){
      data <- data[data$Region == input$Region,]
    }
    if (input$Goal != "All"){
      data <- data[data$Goal == input$Goal,]
    }
    if (input$Terrain != "All"){
      data <- data[data$Terrain == input$Terrain,]
    }
    if (input$Safety != "All"){
      data <- data[data$Safety >= input$Safety,]
    }
    if (input$CPI >= data$CPI){
      data <- data[data$CPI <= input$CPI,]
    }
    data 
    #Make World Map
    world <- map("world", fill=TRUE, plot=FALSE)
    world_map <- map2SpatialPolygons(world, sub(":.*$", "", world$names))
    world_map <- SpatialPolygonsDataFrame(world_map,
                                          data.frame(country=names(world_map), 
                                                     stringsAsFactors=FALSE), 
                                          FALSE)
    target <- subset(world_map, country %in% data[,1])
    leaflet(target) %>%
      addTiles() %>%
      addPolygons(weight = 1)
  }) 
  
  #outputs
  #text
  output$text <- renderText({ 
    "You have selected this:"
  })
  #table
  output$table <- renderTable({
    filter1()
    #map plot
  })
  output$map<- renderLeaflet({
    filter2()
  })
  
})

#####Result
shinyApp(ui = ui, server = server)
