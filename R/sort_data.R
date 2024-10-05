

# ==============  FUNCTION TO SORT DATA  ===================
# These functions are used to handle with similar event daily
# Written by QUAN DAU - 2024
# Climate Smart Lab, UPEI
# --------------------------------------------------------


format_data <- function(join_data) {
    join_data$year <- format(as.Date(join_data$Date), "%Y")
    join_data$id <- format(as.Date(join_data$Date), "%j")
  return(join_data)
}


decrease_sort <- function(data, variable){
  data%>%
  group_by(year) %>%
  arrange(year, desc(data[[variable]]))
}


top_10_event <- function(sorted_data, variable, no){
  sorted_data %>%
    group_by(year) %>%
    slice_max(order_by = get(variable), n = as.numeric(no))
}



# Assuming df is your original dataframe with Date, SumRain, Date.1, MaxTideLevel, and year columns
# and first_match_per_year is the dataframe with matches per year from previous step



# Define a function to find the closest match within a moving window
# find_closest_within_window <- function(year_data_rain, year_data_tide, window_size = 10) {
#   # Initialize an empty dataframe to store matches
#   matches <- data.frame(Date = as.Date(character()), MaxTideLevel = numeric(), stringsAsFactors = FALSE)
#   
#   # Loop through each day in the rainfall data
#   for (i in 1:nrow(year_data_rain)) {
#     rain_date <- year_data_rain$Date[i]
#     
#     # Define the moving window range
#     window_start <- max(1, i - floor(window_size / 2))
#     window_end <- min(nrow(year_data_rain), i + floor(window_size / 2))
#     
#     # Extract the window of tide data
#     window_data <- year_data_tide %>%
#       filter(Date >= year_data_rain$Date[window_start] & Date <= year_data_rain$Date[window_end])
#     
#     # Check if there is a matching date in the window
#     match <- window_data %>%
#       filter(Date == rain_date) %>%
#       slice(1)  # Pick the first match if multiple matches
#     
#     if (nrow(match) > 0) {
#       match <- match %>%
#         mutate(RainDate = rain_date) %>%
#         select(RainDate, Date, MaxTideLevel)
#       matches <- rbind(matches, match)
#     }
#   }
#   
#   
#   # Return the best match found (if any)
#   if (nrow(matches) > 0) {
#     return(matches %>% slice(1))  # Assuming you want the first match or you can apply other criteria
#   } else {
#     return(tibble(RainDate = NA, Date = NA, MaxTideLevel = NA))
#   }
# }
# 
# 
# years_with_no_matches <- df %>%
#   filter(!year %in% first_match_per_year$year) %>%
#   distinct(year)
# 
# # Apply the function to each year with no matches
# additional_matches <- lapply(years_with_no_matches$year, function(year) {
#   year_data_rain <- df %>% filter(year == year) %>% select(Date, SumRain)
#   year_data_tide <- df %>% filter(year == year) %>% select(Date, MaxTideLevel.1)
#   
#   match <- find_closest_within_window(year_data_rain, year_data_tide)
#   
#   tibble(year = year, RainDate = match$RainDate, Date = match$Date, MaxTideLevel = match$MaxTideLevel)
# })
# 


