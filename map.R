library(shiny)
library(tidyverse)
library(leaflet)
library(leaflet.extras)

chicago_clean <- readRDS("data/clean_data/chicago_cleaned_data.rds")
#register_google(key = "AIzaSyC46bco6pjyrIJYIPzY5U1r5T2ktssaFck")

ui <- fluidPage(
  titlePanel("Chicago Listing Heatmap"),
  selectInput("variable", "Choose Variable for Intensity:",
              choices = c("rating", "bedrooms", "price")),  # Add more choices as needed
  leafletOutput("map")
)

server <- function(input, output, session) {
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addHeatmap(
        data = chicago_clean,
        lat = ~latitude,
        lng = ~longitude,
        intensity = as.numeric(chicago_clean[[input$variable]]),
        blur = 20,
        radius = 8
      )
  })
}

shinyApp(ui, server)