##Load Data
chicago_raw<- read.csv("raw_data/listings.csv")
chicago_neighborhoods <- read.csv("raw_data/neighbourhoods.csv")

#Joining data 

chicago_listings <- chicago_raw 

#Cleaning and joining by neighborhood 
chicago_neighborhoods <- chicago_neighborhoods |> 
  mutate(neighbourhood_group = case_when(
    neighbourhood %in% c("Albany Park", "Avondale", "Lake View", "Lincoln Park", "Lincoln Square", "Logan Square", "North Center", "Rogers Park", "Uptown", "Edgewater", "West Ridge", "Loop", "Near North Side", "North Side") ~ "North Side",
    neighbourhood %in% c("Archer Heights", "Ashburn", "Beverly", "Gage Park", "Garfield Ridge", "Hermosa", "West Elsdon", "West Englewood") ~ "Southwest Side",
    neighbourhood %in% c("Armour Square", "Bridgeport", "Burnside", "Calumet Heights", "Chatham", "Chicago Lawn", "Clearing", "Douglas", "Grand Boulevard", "Hyde Park", "Kenwood", "South Chicago", "South Deering", "South Lawndale", "South Shore") ~ "South Side",
    neighbourhood %in% c("Auburn Gresham", "West Garfield Park", "Near South Side") ~ "South Side",
    neighbourhood %in% c("Austin", "East Garfield Park", "Humboldt Park", "West Town") ~ "West Side",
    neighbourhood %in% c("Avalon Park", "New City", "Woodlawn") ~ "South Side",
    neighbourhood %in% c("Belmont Cragin", "Dunning", "North Park", "Norwood Park", "Near West Side") ~ "Northwest Side",
    neighbourhood %in% c("Edgewater") ~ "North Side",
    neighbourhood %in% c("Edison Park", "Forest Glen") ~ "Far Northwest Side",
    neighbourhood %in% c("Fuller Park", "Englewood") ~ "South Side",
    neighbourhood %in% c("Greater Grand Crossing", "Pullman", "Riverdale", "Roseland", "Washington Park") ~ "Far Southeast Side",
    neighbourhood %in% c("Hegewisch", "Mount Greenwood", "Washington Heights") ~ "Far Southwest Side",
    neighbourhood %in% c("Irving Park", "Jefferson Park") ~ "Far Northwest Side",
    neighbourhood %in% c("Lower West Side", "Mckinley Park", "North Lawndale", "Brighton Park") ~ "Southwest Side",
    neighbourhood %in% c("Montclare", "Ohare", "Portage Park") ~ "Far Northwest Side",
    TRUE ~ "Other"
  ))

chicago_new <- chicago_listings |> left_join(chicago_neighborhoods, by = join_by(neighbourhood))

#Separating by rating, bedroom, baths, beds
chicago_clean <- chicago_new |> 
  rename(neighbourhood_group = neighbourhood_group.y) |> 
  relocate(neighbourhood_group, .before = neighbourhood) |> 
  select(-neighbourhood_group.x) |> 
  separate(name,
           into = c("listing_name", "rating", "bedrooms", "beds", "baths"),
           sep = " · ",
           extra = "drop",
           fill = "right",
           convert = TRUE) |> 
  mutate(rating = ifelse(!is.na(rating), as.numeric(str_extract(rating, "\\d+\\.\\d+")), NA),
         bedrooms = as.numeric(str_extract(bedrooms, "\\d+")),
         beds = as.numeric(str_extract(beds, "\\d+")),
         baths = as.numeric(str_extract(baths, "\\d+")), 
         last_review = as.Date(last_review),
         neighbourhood_group = factor(neighbourhood_group)) |> 
  filter(!is.na(rating))

write_rds(chicago_clean, "clean_data/chicago_cleaned_data.rds")

#Chicago_Clean is the base of all my plots and tables for this project
chicago_clean

#Missingness Check
missing_plot <- chicago_clean |> gg_miss_upset()

missing_plot

missing_var <- chicago_clean |> gg_miss_var()

missing_var