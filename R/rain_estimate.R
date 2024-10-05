
# =========== ESTIMATE RAINFALL BASED ON IDW METHOD ==================
# These functions are used to estimate rainfall based on IDW method
# Written by QUAN DAU - 2024
# Climate Smart Lab, UPEI
# ---------------------------------------------------------------



# Function to calculate distances between stations
calculate_distances <- function(coords) {
  dist_matrix <- as.matrix(dist(coords[, c("lat", "lon")]))
  rownames(dist_matrix) <- coords$station
  colnames(dist_matrix) <- coords$station
  return(dist_matrix)
}


# Function to fill missing values using Inverse Distance Weighting Method
fill_missing_inverse_distance <- function(data, distances, target_station) {
  stations <- colnames(data)[-1]  # Exclude the time column
  
  # Create a new column for estimated values of the target station
  data[[paste0(target_station, "_estimated")]] <- NA
  
  # Iterate over each time point
  for (i in 1:nrow(data)) {
    # Identify other stations (excluding the target station)
    other_stations <- setdiff(stations, target_station)
    
    # Compute weights based on distances and other station values
    weights <- 1 / distances[target_station, other_stations]
    
    # Remove NA values from the other stations
    valid_values <- data[i, other_stations]
    valid_weights <- weights[!is.na(valid_values)]
    valid_values <- valid_values[!is.na(valid_values)]
    
    # Compute the estimated value
    if (length(valid_weights) > 0) {
      estimated_value <- sum(valid_weights * valid_values, na.rm = TRUE) / sum(valid_weights, na.rm = TRUE)
      data[i, paste0(target_station, "_estimated")] <- estimated_value
    } else {
      # If no valid values are available, the estimated value remains NA
      data[i, paste0(target_station, "_estimated")] <- NA
    }
  }
  
  return(data)
}


# Function to fill missing values using Inverse Distance Weighting Method
# Using Eq: data[i, station] = (Sum of Weight x data[i, other station]) / Total weights
# Weights are calculated based on the inverse of the distance between the target station (station) and the other station
fill_missing_inverse_distance_NA <- function(data, distances) {
  stations <- colnames(data)[-1]  # Exclude the time column

  for (station in stations) {
    for (i in 1:nrow(data)) {
      if (is.na(data[i, station])) {
        other_stations <- setdiff(stations, station)
        weights <- 1 / distances[station, other_stations]
        data[i, station] <- sum(weights * data[i, other_stations], na.rm = TRUE) / sum(weights, na.rm = TRUE)
      }
    }
  }
  return(data)
}



# Function to clip data based on a selected period
clip_data <- function(station, clip_station) {
  station$Date <- as.POSIXct(station$Date, format = "%m/%d/%Y %H:%M")
  
  # Identify the beginning and the ending of original data
  begin <- head(station$Date, n = 1)
  end <- tail(station$Date, n = 1)
  startDate <- which(clip_station == as.character(begin))
  endDate <- which(clip_station == as.character(end))
  clippedStation <- clip_station[c(startDate:endDate), ]
  
}

# Joint storm and rain data by Date
joint_max_storm_rain <- function(storm, rain){
  storm$Date <- as.POSIXct(storm$Date, format = "%m/%d/%Y %H:%M")
  rain$Date <- as.POSIXct(rain$Date, format = "%m/%d/%Y %H:%M")
  joint <- left_join(storm, rain, by = "Date")
}



 