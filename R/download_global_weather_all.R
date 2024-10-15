# Function to calculate the distance between two latitude/longitude pairs
calculate_distance <- function(lat1, lon1, lat2, lon2) {
  R <- 6371  # Earth's radius in kilometers
  dlat <- (lat2 - lat1) * (pi / 180)
  dlon <- (lon2 - lon1) * (pi / 180)
  
  a <- sin(dlat / 2) ^ 2 + cos(lat1 * (pi / 180)) * cos(lat2 * (pi / 180)) * sin(dlon / 2) ^ 2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  
  distance <- R * c  # Distance in kilometers
  return(distance)
}

# Function to find the nearest climate stations for each tide station within a buffer
find_nearest_stations <- function(tide_stations, climate_stations, buffer_km ) {
  all_longest_stations <- data.frame()  # Initialize an empty data frame
  tide_outside_buffer <- data.frame()  # To store tide stations with no nearby climate stations
  
  for (i in 1:nrow(tide_stations)) {
    tide_lat <- as.numeric(tide_stations$latitude[i])
    tide_lon <- as.numeric(tide_stations$longitude[i])
    tide_station_name <- tide_stations$Location[i] 
    tide_ID <- tide_stations$ID[i]  
    
    # Calculate distances to all climate stations
    distances <- apply(climate_stations, 1, function(x) {
      lat2 <- as.numeric(x["latitude"])
      lon2 <- as.numeric(x["longitude"])
      calculate_distance(tide_lat, tide_lon, lat2, lon2)
    })
    
    # Get stations within the buffer
    within_buffer_indices <- which(distances <= buffer_km)
    climate_within_buffer <- climate_stations[within_buffer_indices, ]
    
    # Filter out stations without valid Start and End dates
    climate_within_buffer <- climate_within_buffer[!is.na(climate_within_buffer$Start) & !is.na(climate_within_buffer$End), ]
    
    if (nrow(climate_within_buffer) == 0) {
      # If no climate station is within the buffer, add the tide station to the result
      tide_outside_buffer <- rbind(tide_outside_buffer, tide_stations[i, ])
    } else {
      # Calculate dataset lengths
      climate_within_buffer$Dataset_Length <- as.Date(climate_within_buffer$End) - as.Date(climate_within_buffer$Start)
      
      # Find the station(s) with the longest dataset
      max_length <- max(climate_within_buffer$Dataset_Length, na.rm = TRUE)
      longest_stations <- climate_within_buffer[climate_within_buffer$Dataset_Length == max_length, ]
      
      if (nrow(longest_stations) > 0) {
        # Combine the longest stations' data (if more than one)
        for (j in 1:nrow(longest_stations)) {
          individual_station <- data.frame(
            Tide_ID = tide_ID,  # Add corresponding tide station info (added)
            Tide_Station = tide_station_name,  # Add corresponding tide station info (added)
            Tide_Latitude = tide_lat,          # Tide station latitude
            Tide_Longitude = tide_lon,         # Tide station longitude
            Climate_ID = longest_stations$ID[j],
            Climate_Station = longest_stations$Station.en[j],
            Climate_Country = longest_stations$Country[j],
            Climate_Region = longest_stations$Region[j],
            Climate_Latitude = longest_stations$latitude[j],
            Climate_Longitude = longest_stations$longitude[j],
            Start = longest_stations$Start[j],
            End = longest_stations$End[j],
            Dataset_Length = longest_stations$Dataset_Length[j]
          )
          
          # Append the combined station to the results
          all_longest_stations <- rbind(all_longest_stations, individual_station)
        }
      }
    }
  }
  
  return(list(all_longest_stations = all_longest_stations, tide_outside_buffer = tide_outside_buffer))
}
