library(shiny)
library(tidyverse)
library(leaflet)
library(leaflet.extras)
library(sf)
library(ggmap)

chicago_clean <- readRDS("data/clean_data/chicago_cleaned_data.rds")
register_google(key = "AIzaSyC46bco6pjyrIJYIPzY5U1r5T2ktssaFck")

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
        
        .body-text{
          padding: 25px;
          font-size: 20;
          text-align: center;
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
               12, div("A More Detailed Breakdown", class = "header")
             )),
             br(),
             column(12, h2("First, a breakdown of the sample shown on the map.")), 
             fluidRow(
               column(12,
                      div(tableOutput("variableBreakdown"))
               ), 
               column(12,
                      div(plotOutput("histogram"))
               )), 
             br(), 
             column(12, h2("Now, lets look at the neighborhoods and neighborhood groups.")), 
       
             column(6,
                     div(plotOutput(outputId = "neighborhoodBarplot"))),
              column(6, div(plotOutput(outputId = "neighborhoodGroupBarplot")),
                        br(),
                        div(plotOutput(outputId = "neighborhoodListingsMakeup")))
    ),
    tabPanel("Neighborhood Groups", fluid = TRUE,
             fluidRow(column(
               12, div("Neighborhood Groups", class = "header")
             )),
        
             div(class = "body-text",
               fluidRow(
                 p("The code categorizes neighborhoods in Chicago into different groups based on their geographical locations. The neighborhoods are assigned to broader areas or sides of the city to facilitate analysis and interpretation. The groupings are as follows:")
               ),
                 
               fluidRow(
                 column(12,
                   fluidRow(h5("North Side")), 
                   fluidRow(p(" Albany Park, Avondale, Lake View, Lincoln Park, Lincoln Square, Logan Square, North Center, Rogers Park, Uptown, Edgewater, West Ridge, Loop, Near North Side, North Side"))
                 )
               ), 
               fluidRow(
                 column(12,
                   fluidRow(h5("Southwest Side")), 
                   fluidRow(p("Archer Heights, Ashburn, Beverly, Gage Park, Garfield Ridge, Hermosa, West Elsdon, West Englewood"))
                 )
               ),fluidRow(
                 column(12,
                   fluidRow(h5("South Side")), 
                   fluidRow(p("Armour Square, Bridgeport, Burnside, Calumet Heights, Chatham, Chicago Lawn, Clearing, Douglas, Grand Boulevard, Hyde Park, Kenwood, South Chicago, South Deering, South Lawndale, South Shore, Auburn Gresham, West Garfield Park, Near South Side, Avalon Park, New City, Woodlawn, Fuller Park, Englewood"))
               )),fluidRow(
                 column(12,
                   fluidRow(h5("West Side")), 
                   fluidRow(p(" Austin, East Garfield Park, Humboldt Park, West Town"))
               )), fluidRow(
                 column(12, 
                        fluidRow(h5("Northwest Side")),
                        fluidRow(p("Belmont Cragin, Dunning, North Park, Norwood Park, Near West Side, Edison Park, Forest Glen, Irving Park, Jefferson Park, Montclare, Ohare, Portage Park"))
                )),fluidRow(
                  column(12, 
                         fluidRow(h5("Far Northwest Side")),
                         fluidRow(p("Edgewater, Edison Park, Forest Glen, Irving Park, Jefferson Park, Montclare, Ohare, Portage Park"))
                )),fluidRow(
                  column(12, 
                         fluidRow(h5("Southwest Side")),
                         fluidRow(p("Fuller Park, Englewood, Lower West Side, Mckinley Park, North Lawndale, Brighton Park"))
                )),fluidRow(
                  column(12, 
                         fluidRow(h5("Other")),
                         fluidRow(p("All neighborhoods not explicitly listed in the above groups are categorized as “Other.” These are neighborhoods which lie far beyond the defined Chicagoland geography."))
                ))
               )
    ),
    
    tabPanel("Explanation", fluid = TRUE,
             fluidRow(column(
               12, div("Chicago Listing Visualization: Explanation", class = "header")
             )),
             br(),
             fluidRow(
               column(
                 8,
                 offset = 2,
                 p("For our final project, we elected to work with the Airbnb data set from insideairbnb.com. This data provides information on over 5,000 different Airbnb listings in the Chicago Area")
               ),
              
               column(
                 8,
                 offset = 2,
                 h4("Visualizations: Core Concepts & Insights"),
                 p("For our final visualization, we elected to create a Shiny app which allows users to visualize and manipulate the Airbnb data set to better understand the distribution of various variables in the data set. Particularly, in our animated map, users are able to explore the relative listing densities of price, bedrooms, ratings, and even availability over a calendar year to better understand how the platform fluctuates across the city of Chicago and beyond. In our case, users can select the price variable to visualize which areas of the city are most expensive and where there is the highest density of Airbnb’s with a certain price. This is an extremely helpful visualization as it enables us to demonstrate the most expensive and desirable locations in Chicago. Beyond this, users can also explore the density of availability (how many days an Airbnb listing spends vacant on average in a year) in each region to see which areas of Chicago are most and least popular. Finally, the bedroom and bath variables demonstrate the sizes of the listings and where the largest and smallest listings are."),
                
                p("In addition to the interactive map, we decided to include a tab with summary statistics and visualizations by neighborhood to give the user a more in depth look at the variable they chose. Key summary statistics that we display include the mean, maximum, minimum, total number of values, and distinct number of values. Below these statistics, we included a histogram for the variable selected to show the user how it is distributed in the sample they have chosen. The users can also see a breakdown of how their variable fairs in each neighborhood in the barplots, as well as, how many listings from the sample are in each neighborhood group in the pie chart. The barplots show the average for both neighborhoods and neighborhood groups.  We felt the addition of this summary tab ensured that the user could not only see the density of values on the map, but also get a more explicit breakdown of their variable in the sample."),
                 
                p("Together, we think the map and summary tab give a holistic view of the airbnb data and how distinct variables are distributed across Chicago and in the data set itself."),
                br(),
                h4("Shiny App: Key Widgets and Explanations"),
                p("Variable selection - We wanted to be able to look at any of the continuous variables and how their densities differ across Chicago"),

                 p("Sample selection - Allowing the users to choose how many listings are on the map lets them see the effect that more listings may have on the variable they have chosen. With fewer listings, it may seem as though neighborhood prices are fairly equal, but as you increase the sample size, you can see the variation between neighborhood groups increase."),

                  p("Listing markers - The density map gives a good idea of where the variable is concentrated, but we thought that adding the listing markers was helpful, especially when the sample size was small. It shows exactly which listings are being pulled to create the density map"),

                  p("Neighborhood group tab - We thought this was necessary to show which neighborhoods fall into which groups 
                  "),
                br(),
                h4("Next Steps and Future Explorations"),
                p("Possible future enhancements include incorporating more advanced analytics, additional map layers, and more user customization options. We think it would be very interesting to look at the densities of two variables concurrently. With this addition, the summary tab could develop to also show the correlation between the two variables selected."),
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
        data = chicago_subset(),
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
          summarise(Variable = var_title, Total = n(), Mean = mean(var_summarise, na.rm = TRUE), Distinct = n_distinct(var_summarise), Minimum = min(var_summarise, na.rm = TRUE), Maximum = max(var_summarise, na.rm = TRUE))
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
      
      
      chicago_subset() %>% ggplot(aes(var_bar)) +
        geom_histogram(fill = "#482677ff") + 
       labs(
         title = paste("Distribution of", var_title, "in the Sample"),
         x = var_title, 
         y = "Count"
         ) +
        theme(
          plot.background = element_rect(fill= "#dcdcdc",  color = "#dcdcdc"), 
          plot.title = element_text(size = 18, face = "bold"),
          axis.text = element_text(size = 12), 
          axis.title = element_text(size = 14)
        )
      
      
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
      
      chicago_subset() %>%  
        ggplot(aes(var_bar, neighbourhood)) +
        geom_col(aes(fill = neighbourhood_group), position = "dodge", width = 0.9) +
        ggtitle(paste('Average', var_title, 'in each Neighborhood')) +
        labs(
          x = var_title,
          y = "Neighborhood",
          fill = "Neighborhood Groups"
        ) + 
        scale_x_continuous(
          expand =  expansion(mult = c(0, .1))
        ) +
        theme(plot.title = element_text(size = 18, face = "bold"), 
              axis.text = element_text(size = 12), 
              axis.title = element_text(size = 14),
              plot.background = element_rect(fill= "#dcdcdc",  color = "#dcdcdc"), 
              legend.position = "right", 
              legend.background = element_rect(fill= "#dcdcdc",  color = "black"), 
              legend.direction = "vertical", 
              legend.text = element_text(size=18), 
              legend.title = element_text(size=18)
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
        geom_col(aes(fill = neighbourhood_group), position = 'dodge', width = 0.9, show.legend = FALSE) +
        labs(
          title = paste('Average', var_title, 'in each Neighborhood Group'),
          y = "Neighborhood Group",
          x = var_title, 
          fill = "Neighborhood\nGroup"
        ) + 
        scale_x_continuous(
          expand = expansion(mult = c(0, .1))
        ) +
        theme(plot.title = element_text(size = 18, face = "bold"), 
              axis.text = element_text(size = 12), 
              axis.title = element_text(size = 14),
              plot.background = element_rect(fill= "#dcdcdc", color = "#dcdcdc")
             
        )
    }, height = 400)
    
    output$neighborhoodListingsMakeup <- renderPlot({
      chicago_subset() %>% ggplot(aes(x = 1, fill = neighbourhood_group)) + 
        geom_bar(stat = "count", width = 1, show.legend = FALSE) + 
        geom_text(stat='count', aes(label = ..count..,  fontface = "bold"),
                position = position_stack(vjust = 0.5)) +
        coord_polar(theta = "y") + 
        labs(
          title = "Number of Listings in Each Neighborhood Group", 
          x = NULL, 
          y = NULL
        ) +
        theme_minimal() +
        theme(
          axis.text = element_blank(), 
          axis.ticks = element_blank(),
          plot.background = element_rect(fill= "#dcdcdc", color = "#dcdcdc"), 
          plot.title = element_text(size = 18, face = "bold", hjust = 0.5)
        )
      
    }, height = 400, width= 1000, bg = "#dcdcdc")

}

shinyApp(ui, server)


