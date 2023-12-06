library(shiny)
library(tidyverse)
library(leaflet)
library(leaflet.extras)

chicago_clean <- readRDS("data/clean_data/chicago_cleaned_data.rds")

ui <- fluidPage(
  tags$head(tags$style(
    HTML(
      "
        body {
          background-color: #dcdcdc;
        }

        .header {
          background-color: #333;
          color: white;
          padding: 10px;
          text-align: center;
          font-size: 24px;
          font-weight: bold;
        }

        .container {
          background-color: #333;
          color: white;
          padding: 20px;
          border-radius: 10px;
        }

        .leaflet-popup-content {
          color: black;
        }

        .map-container {
          position: relative;
        }

        .action-button {
          position: absolute;
          top: 10px;
          right: 10px;
        }
        .nav-tabs > li > a {
      color: black;
    }

    .nav-tabs > li.active > a,
    .nav-tabs > li.active > a:hover,
    .nav-tabs > li.active > a:focus {
      color: white;
      background-color: #333;
    }
    "
    )
  )),
  titlePanel(""),
  
  # TabsetPanel with two tabs
  tabsetPanel(
    tabPanel("Map", fluid = TRUE,
             fluidRow(column(
               12, div("Chicago Listing Heatmap", class = "header")
             )),
             fluidRow(
               column(
                 4,
                 selectInput(
                   "variable",
                   "Choose Variable for Heat Map Intensity:",
                   choices = c(
                     "rating",
                     "bedrooms",
                     "price",
                     "reviews_per_month",
                     "availability_365"
                   )
                 )
               ),
               
               column(
                 4,
                 sliderInput(
                   "num_listings",
                   "Number of Sample Listings:",
                   min = 0,
                   max = 1000,
                   value = 100,
                   step = 1
                 )
               ),
               
               column(
                 4,
                 actionButton(
                   "show_markers",
                   "Show Listing Markers",
                   icon = icon("eye"),
                   class = "action-button"
                 )
               )
             ),
             
             fluidRow(column(
               12,
               div(class = "map-container",
                   leafletOutput(
                     "map", width = "100%", height = "600px"
                   ))
             ))
    ),
    
    tabPanel("Explanation", fluid = TRUE,
             fluidRow(column(
               12, div("Chicago Listing Visualization: Explanation an", class = "header")
             )),
             fluidRow(
               column(
                 12,
                 p("For our final project, we elected to work with the Airbnb data set from insideairbnb.com. This data provides information on over 5,000 different Airbnb listings in the Chicago Area")
               ),
              
               column(
                 12,
                 h4("Core Concepts & Insights"),
                 p("For our final visualization, we elected to create a Shiny app which allows users to visualize and manipulate the Airbnb data set to better understand the distribution of various variables in the data set. Particularly, in our animated map, users are able to explore the relative listing densities of price, bedrooms, ratings, and even availability over a calendar year to better understand how the platform fluctuates across the city of Chicago and beyond. In our case, users can select the price variable to visualize which areas of the city are most expensive and where there is the highest density of Airbnb’s with a certain price. This is an extremely helpful visualization as it enables us to demonstrate the most expensive and desirable locations in Chicago. Beyond this, users can also explore the density of availability (how many days an Airbnb listing spends vacant on average in a year) in each region to see which areas of Chicago are most and least popular. Finally, the bedroom and bath variables demonstrate the sizes of the listings and where the largest and smallest listings are.
	In viewing the data, users are able to conclude that the most expensive and popular parts of the city are just north of South Loop in Chicago as both availability and price have the highest density of listings here. Even further, bedroom and bath data shows that the majority of listings have only 1-2 bedrooms and that there are only a select number of listings with extremely large property sizes. Finally, the review per month variable visualizes which listings are most frequented in Chicago and thus another visualization of which neighborhoods are most popular in the city.
"),
h4("Animation and Visualization Guidebook"),
p("blah blah."),
h4("Key Features and Explanations"),
p("blah blah"),
h4("Next Steps and Future Explorations"),
p("Possible future enhancements include incorporating more advanced analytics, additional map layers, and user customization options."),
               )
             )
    )
  )
)

server <- function(input, output, session) {
  markers <- reactiveVal(TRUE)
  
  observeEvent(input$show_markers, {
    markers(!markers())
  })
  
  output$map <- renderLeaflet({
    chicago_subset <-
      chicago_clean |> slice_sample(n = input$num_listings)  # Use selected number of listings
    
    map <- leaflet() |> 
      addProviderTiles("CartoDB.DarkMatter")
    
    if (markers()) {
      map <- map |> 
        addAwesomeMarkers(
          data = chicago_subset,
          lat = ~ latitude,
          lng = ~ longitude,
          icon = awesomeIcons(
            icon = 'home',
            markerColor = 'white',
            iconColor = 'black',
            library = 'fa'
          ),
          popup = ~ paste(
            "<strong>Name:</strong>",
            listing_name,
            "<br>",
            "<strong>Rating:</strong>",
            rating,
            "<br>",
            "<strong>Bedrooms:</strong>",
            bedrooms,
            "<br>",
            "<strong>Price:</strong>",
            price,
            "<br>",
            "<strong>Baths:</strong>",
            baths,
            "<br>",
            "<strong>Reviews per Month:</strong>",
            reviews_per_month,
            "<br>",
            "<strong>Availability 365:</strong>",
            availability_365
          )
        )
    }
    
    map <- map |> 
      addHeatmap(
        data = chicago_clean,
        lat = ~ latitude,
        lng = ~ longitude,
        intensity = chicago_subset[[input$variable]],
        blur = 20,
        radius = 15
      ) |> 
      addLegend(
        title = paste("", input$variable),
        values = chicago_subset[[input$variable]],
        pal = colorNumeric("viridis", domain = NULL),
        opacity = 1
      )
    
    map
  })
}

shinyApp(ui, server)


