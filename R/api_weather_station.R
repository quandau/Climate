
# Function to download data from METEOSTAT

api_weather_loop <- function(station_id, start_date, end_date, max_retries = 2) {
 
    #api_key <- "b770f168bamshe29045b882f8f04p1c9d85jsnaf630d73a4f3"   #GMAIL
   #api_key <- "a2b7f2e431msh910c00645611f19p113070jsne9188a58463e"    #UPEI
   #api_key <- "0a4e19b6d9msha1cd944c35b4f9bp1ef210jsnba0cd0dabef4"    #KKU
   #api_key <-  "0e514ff5d5msh51a2fd6ae66c357p153ae9jsnb967d7f247d6" #VIETHA
   api_key <-  "da7c75a6b3msh6d68ce589a315e2p1e1ebajsnb2b19174c0e1" #DUNG
  
  # Convert start and end dates to Date format
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  
  # Initialize empty dataframe to store all the weather data
  all_weather_data <- data.frame()
  
  current_start_date <- start_date
  
  # Loop through the period, retrieving data in 10-year chunks
  while (current_start_date <= end_date) {
    
    # Define the current end date (up to 9 years later or the overall end date)
    current_end_date <- min(current_start_date + years(9) - days(1), end_date)
    
    start_10year_date <- format(current_start_date, "%Y-%m-%d")
    end_10year_date <- format(current_end_date, "%Y-%m-%d")
    
    # Construct the API URL
    url <- paste0("https://meteostat.p.rapidapi.com/stations/daily?station=", station_id, 
                  "&start=", start_10year_date, "&end=", end_10year_date)
    
    print(paste("Requesting data for:", station_id, "from", start_10year_date, "to", end_10year_date))
    
    # Initialize variables for retries
    success <- FALSE
    attempts <- 0
    
    # Try making the API request, with retries if it fails
    while (!success && attempts < max_retries) {
      attempts <- attempts + 1
      
      # Make the API request
      response <- GET(url, add_headers(
        "x-rapidapi-host" = "meteostat.p.rapidapi.com",
        "x-rapidapi-key" = api_key
      ))
      
      # Check if the request was successful
      if (http_status(response)$category == "Success") {
        data <- content(response, as = "parsed", simplifyDataFrame = TRUE)
        weather_data <- data$`data`
        
        if (!is.null(weather_data)) {
          weather_df <- data.frame(weather_data)
          all_weather_data <- rbind(all_weather_data, weather_df)
        }
        
        print(paste("Success:", station_id, "data from", start_10year_date, "to", end_10year_date))
        success <- TRUE  # Exit the retry loop
      } else {
        warning(paste("Failed attempt", attempts, "to retrieve data for", station_id, 
                      "from", start_10year_date, "to", end_10year_date))
        Sys.sleep(1)  # Wait before retrying (optional, you can adjust the wait time)
      }
    }
    
    # If all attempts fail, log an error
    if (!success) {
      warning("Failed to retrieve data after", max_retries, "attempts for period:", 
              start_10year_date, "to", end_10year_date)
    }
    
    # Update current_start_date to the day after the current_end_date
    current_start_date <- current_end_date + days(1)
  }
  
  return(all_weather_data)
}


calculate_segments <- function(start_date, end_date) {
  # Convert dates to Date format
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  
  # Calculate total number of days
  total_days <- as.numeric(end_date - start_date)
  
  # Define number of days covered in each request (10 years)
  days_per_request <- 3652  # or use 365.25 for a more precise calculation
  
  # Calculate the number of segments (requests needed)
  num_segments <- ceiling(total_days / days_per_request)
  
  return(num_segments)
}


# api_weather_loop <- function(station_id, start_date, end_date) {
#   # api_key <- "b770f168bamshe29045b882f8f04p1c9d85jsnaf630d73a4f3"
#   api_key <- "a2b7f2e431msh910c00645611f19p113070jsne9188a58463e"
#   start_date <- as.Date(start_date)
#   end_date <- as.Date(end_date)
# 
#   all_weather_data <- data.frame()
# 
#   current_start_date <- start_date
# 
#   while (current_start_date < end_date) {
#     current_end_date <- min(current_start_date + years(9) - days(1), end_date)
# 
# 
#     start_10year_date <- format(current_start_date, "%Y-%m-%d")
#     end_10year_date <- format(current_end_date, "%Y-%m-%d")
# 
#     url <- paste0("https://meteostat.p.rapidapi.com/stations/daily?station=", station_id,
#                   "&start=", start_10year_date, "&end=", end_10year_date)
# 
#     response <- GET(url, add_headers(
#       "x-rapidapi-host" = "meteostat.p.rapidapi.com",
#       "x-rapidapi-key" = api_key
#     ))
# 
#     if (http_status(response)$category == "Success") {
#       data <- content(response, as = "parsed", simplifyDataFrame = TRUE)
#       weather_data <- data$`data`
#       weather_df <- data.frame(weather_data)
# 
#       all_weather_data <- rbind(all_weather_data, weather_df)
#     } else {
#       warning("Failed to retrieve data for period: ", start_10year_date, " to ", end_10year_date)
#     }
# 
#     current_start_date <- current_end_date + days(1)
#   }
# 
#   return(all_weather_data)
# }
# 
# 
#   # weather_data <- api_weather_loop(71706, "1943-04-01", "2024-12-28")
# 
