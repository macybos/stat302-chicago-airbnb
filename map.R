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
                     "baths",
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
               12, div("Explanation of the Chicago Listing Visualization", class = "header")
             )),
             fluidRow(
               column(
                 12,
                 p("For our final project, we elected to work with the Airbnb data set from insideairbnb.com (http://insideairbnb.com/get-the-data/)[http://insideairbnb.com/get-the-data/]")
               ),
               # Subheadings and content
               column(
                 12,
                 h4("Research Question"),
                 p("The app aims to address the research question: How are different listing variables distributed across the city of Chicago?"),
                 h4("Initiative"),
                 p("The initiative involves creating an interactive tool for users to visualize and analyze Chicago housing data."),
                 h4("Ideas"),
                 p("Possible future enhancements include incorporating more advanced analytics, additional map layers, and user customization options.")
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
    
    map <- leaflet() %>%
      addProviderTiles("CartoDB.DarkMatter")
    
    if (markers()) {
      map <- map %>%
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
    
    map <- map %>%
      addHeatmap(
        data = chicago_clean,
        lat = ~ latitude,
        lng = ~ longitude,
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
