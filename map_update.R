library(shiny)
library(tidyverse)
library(leaflet)
library(leaflet.extras)
library(sf)

chicago_static_map_data <- read_sf("data/chicagomap.shp")
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
        
        .plot-container {
          backgroud-color : #dcdcdc;
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
  
  # tabs
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
    
    tabPanel("Summary Visuals", fluid = TRUE,
             fluidRow(column(
               12, div("A More Detailed Look", class = "header")
             )),
             fluidRow(br()),
             column(12, h2("First, a breakdown of the sample shown on the map.")), 
             fluidRow(
               column(12,
                      div(tableOutput("variableBreakdown"))
               ), 
               column(12,
                      div(class = "plot-container", plotOutput("histogram"))
               )), 
             br(), 
             column(12, h2("Now, lets look at the neighborhoods and neighborhood groups.")), 
             fluidRow(
               column(6,
                       div(class = "plot-container", plotOutput(outputId = "neighborhoodBarplot"))),
                column(6, fluidRow(div(class = "plot-container", plotOutput(outputId = "neighborhoodGroupBarplot"))), 
                          fluidRow(div(class = "plot-container", plotOutput(outputId = "neighborhoodMap")))
                )
             )  
    ),
    tabPanel("Neighborhood Groups", fluid = TRUE,
             fluidRow(column(
               12, div("Chicago Listing Visualization: Explanation an", class = "header")
             )),
             
             div("The code categorizes neighborhoods in Chicago into different groups based on their geographical locations. The neighborhoods are assigned to broader areas or sides of the city to facilitate analysis and interpretation. The groupings are as follows:"), 
             
             fluidRow(
               column(4,
                 fluidRow(p("North Side")), 
                 fluidRow(p(" Albany Park, Avondale, Lake View, Lincoln Park, Lincoln Square, Logan Square, North Center, Rogers Park, Uptown, Edgewater, West Ridge, Loop, Near North Side, North Side"))
               )
             ), 
             fluidRow(
               column(4,
                 fluidRow(p("Southwest Side")), 
                 fluidRow(p("Archer Heights, Ashburn, Beverly, Gage Park, Garfield Ridge, Hermosa, West Elsdon, West Englewood"))
               )
             ),fluidRow(
               column(4,
                 fluidRow(p("South Side")), 
                 fluidRow(p("Armour Square, Bridgeport, Burnside, Calumet Heights, Chatham, Chicago Lawn, Clearing, Douglas, Grand Boulevard, Hyde Park, Kenwood, South Chicago, South Deering, South Lawndale, South Shore, Auburn Gresham, West Garfield Park, Near South Side, Avalon Park, New City, Woodlawn, Fuller Park, Englewood"))
             )),fluidRow(
               column(4,
                 fluidRow(p("West Side")), 
                 fluidRow(p(" Austin, East Garfield Park, Humboldt Park, West Town")
             )))
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
  
  chicago_subset <- reactive({
    chicago_clean |> slice_sample(n = input$num_listings)
  })
  

   observeEvent(input$show_markers, {
    markers(!markers())
  })
  
 
  output$map <- renderLeaflet({
    map <- leaflet() |> 
      addProviderTiles("CartoDB.DarkMatter")
    
    if (markers()) {
      map <- map |> 
        addAwesomeMarkers(
          data = chicago_subset(),
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
        intensity = chicago_subset()[[input$variable]],
        blur = 20,
        radius = 15
      ) |> 
      addLegend(
        title = paste("", input$variable),
        values = chicago_subset()[[input$variable]],
        pal = colorNumeric("viridis", domain = NULL),
        opacity = 1
      )
    
    map
    })
    
    output$variableBreakdown <- renderTable({
      var_summarise <- switch(
        input$variable,
        'rating' = chicago_subset()$rating,
        'bedrooms' = chicago_subset()$bedrooms,
        'price' = chicago_subset()$price,
        'reviews_per_month' = chicago_subset()$number_of_reviews,
        'availability_365' = chicago_subset()$availability_365
      ) 
      
      var_title <- switch(
        input$variable,
        'rating' = 'Rating',
        'bedrooms' = 'Number of Bedrooms',
        'price' = 'Price per Night',
        'reviews_per_month' = 'Reviews Per Month', 
        'availability_365' = 'Availability throughout the Year'
      )
      
      chicago_subset() %>% 
          summarise(Variable = var_title, Total = n(), Mean = mean(var_summarise), Distinct = n_distinct(var_summarise), Minimum = min(var_summarise), Maximum = max(var_summarise))
    })
    
    output$histogram <- renderPlot({

      var_bar <- switch(
        input$variable,
        'rating' = chicago_subset()$rating,
        'bedrooms' = chicago_subset()$bedrooms,
        'price' = chicago_subset()$price,
        'reviews_per_month' = chicago_subset()$number_of_reviews,
        'availability_365' = chicago_subset()$availability_365
      )
      
      var_title <- switch(
        input$variable,
        'rating' = 'Rating',
        'bedrooms' = 'Number of Bedrooms',
        'price' = 'Price per Night',
        'reviews_per_month' = 'Reviews Per Month', 
        'availability_365' = 'Availability throughout the Year'
      )
      
      if (input$variable != 'bedrooms') {
        chicago_subset() %>% ggplot(aes(var_bar)) +
          geom_histogram() + 
          ggtitle(paste("Distribution of", var_title, "in the Sample"))
      }
    })
    
    output$neighborhoodBarplot <- renderPlot({
      
      var_bar <- switch(
        input$variable,
        'rating' = chicago_subset()$rating,
        'bedrooms' = chicago_subset()$bedrooms,
        'price' = chicago_subset()$price,
        'reviews_per_month' = chicago_subset()$number_of_reviews,
        'availability_365' = chicago_subset()$availability_365
      )
      
      var_title <- switch(
        input$variable,
        'rating' = 'Rating',
        'bedrooms' = 'Number of Bedrooms',
        'price' = 'Price per Night',
        'reviews_per_month' = 'Reviews Per Month',
        'availability_365' = 'Availability throughout the Year'
      )
      
      chicago_subset() %>% ggplot(aes(var_bar, neighbourhood)) +
        geom_col(aes(fill = neighbourhood), position = 'dodge', width = 0.9, show.legend = FALSE) +
        ggtitle(paste('Average', var_title, 'in each Neighborhood')) +
        labs(
          x = var_title,
          y = "Neighborhood"
        ) + 
        scale_x_continuous(
          expand =  expansion(mult = c(0, .1))
        ) +
        theme(plot.title = element_text(size = 12, face = "bold"), 
              axis.text = element_text(size = 12), 
              axis.title = element_text(size = 18),
              rect = element_rect(color = "#dcdcdc")
        )
    }, height = 800)
    
    output$neighborhoodGroupBarplot <- renderPlot({
      
      var_bar <- switch(
        input$variable,
        'rating' = chicago_subset()$rating,
        'bedrooms' = chicago_subset()$bedrooms,
        'price' = chicago_subset()$price,
        'reviews_per_month' = chicago_subset()$number_of_reviews,
        'availability_365' = chicago_subset()$availability_365
      )
      
      var_title <- switch(
        input$variable,
        'rating' = 'Rating',
        'bedrooms' = 'Number of Bedrooms',
        'price' = 'Price per Night',
        'reviews_per_month' = 'Reviews Per Month',
        'availability_365' = 'Availability throughout the Year'
      )
      
      chicago_subset() %>% ggplot(aes(var_bar, neighbourhood_group)) +
        geom_col(position = 'dodge', fill = "lightblue", width = 0.9) +
        ggtitle(paste('Average', var_title, 'in each Neighborhood Group')) +
        labs(
          y = "Neighborhood Group",
          x = var_title
        ) + 
        scale_x_continuous(
          expand = expansion(mult = c(0, .1))
        ) +
        theme(plot.title = element_text(size = 12, face = "bold"), 
              axis.text = element_text(size = 12), 
              axis.title = element_text(size = 18),
              rect = element_rect(color = "#dcdcdc")
        )
    }, height = 400)
    
    output$neighborhoodMap <- renderPlot({
      chicago_static_map_data %>% ggplot(aes(fill = area_num_1)) + 
        geom_sf(show.legend = FALSE) + 
        theme_void()
      
    }, height = 400)

}

shinyApp(ui, server)


