library(shiny)
library(tidyverse)
library(leaflet)
library(leaflet.extras)
library(sf)

chicago_clean <- readRDS("data/clean_data/chicago_cleaned_data.rds")
#register_google(key = "AIzaSyC46bco6pjyrIJYIPzY5U1r5T2ktssaFck")

ui <- fluidPage(
  titlePanel("Chicago Listing Heatmap"),
  sidebarPanel(
    selectInput("variable", "Choose Variable for Intensity:",
                choices = c("rating", "bedrooms", "price", "number_of_reviews")),  # Add more choices as needed
  ),
  mainPanel(
    tabsetPanel(type = "tabs", 
                tabPanel("Map", leafletOutput("map")), 
                tabPanel("Neighborhood Group Summary", plotOutput(outputId = "barplot")))
  )
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
  
  output$barplot <- renderPlot({
    
    var_bar <- switch(
      input$variable, 
      'rating' = chicago_clean$rating, 
      'bedrooms' = chicago_clean$bedrooms, 
      'price' = chicago_clean$price, 
      "number_of_reviews" = chicago_clean$number_of_reviews
    )
    
    var_title <- switch(
      input$variable, 
      'rating' = 'Rating',
      'bedrooms' = 'Number of Bedrooms', 
      'price' = 'Price', 
      "number_of_reviews" = "Number of Reviews"
    )
    
    chicago_clean %>% ggplot(aes(neighbourhood_group, var_bar)) + 
      geom_bar(position = 'dodge', stat = 'summary', fun = 'mean', fill = "lightblue") + 
      ggtitle(paste('Average', var_title, 'in each Neighborhood Group')) + 
      labs(
        x = "Neighborhood Group", 
        y = var_title
      ) +
      theme_minimal()
  })
  
  
}

shinyApp(ui, server)