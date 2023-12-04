library(shiny)
library(tidyverse)
library(leaflet)
library(leaflet.extras)

chicago_clean <- readRDS("data/clean_data/chicago_cleaned_data.rds")
#register_google(key = "AIzaSyC46bco6pjyrIJYIPzY5U1r5T2ktssaFck")

ui <- fluidPage(
  titlePanel("Chicago Listing Heatmap"),
  selectInput("variable", "Choose Variable for Intensity:",
              choices = c("rating", 
                          "bedrooms", 
                          "price", 
                          "baths", 
                          "reviews_per_month",
                          "availability_365")),  # Add more choices as needed
  
  sliderInput("num_listings", "Number of Listings:",
              min = 0, max = 1000, value = 100, step = 1),  # Add slider input
  
  checkboxInput("show_markers", "Show Markers", value = TRUE),  # Add checkbox input
  
  leafletOutput("map")
)
server <- function(input, output, session) {
  output$map <- renderLeaflet({
    chicago_subset <- chicago_clean |> slice_sample(n = input$num_listings)  # Use selected number of listings
    
    map <- leaflet() %>%
      addProviderTiles("CartoDB.DarkMatter") 
    
    if (input$show_markers) {
      map <- map %>%
        addMarkers(data = chicago_subset, 
                   lat = ~latitude, 
                   lng = ~longitude,
                   popup = ~paste(
                     "<strong>Name:</strong>", listing_name, "<br>",
                     "<strong>Rating:</strong>", rating, "<br>",
                     "<strong>Bedrooms:</strong>", bedrooms, "<br>",
                     "<strong>Price:</strong>", price, "<br>",
                     "<strong>Baths:</strong>", baths, "<br>",
                     "<strong>Reviews per Month:</strong>", reviews_per_month, "<br>",
                     "<strong>Availability 365:</strong>", availability_365
                   ))
    }
    map <- map %>%
      addHeatmap(
        data = chicago_clean,
        lat = ~latitude,
        lng = ~longitude,
        intensity = chicago_subset[[input$variable]],
        blur = 20,
        radius = 8
      ) %>%
      addLegend(
        title = paste("", input$variable),
        values = chicago_subset[[input$variable]],
        pal = colorNumeric("viridis", domain = NULL),
        opacity = 0.7
      )
    
    map
  })
}
shinyApp(ui, server)
