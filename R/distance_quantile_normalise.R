
# ==============  DISTANCE BASED FUNCTIONS  =====================
# These functions are used to handle with data using Distance-based approach
# Written by QUAN DAU - 2024
# Climate Smart Lab, UPEI
# --------------------------------------------------------


distance_quantile_normalization <- function(df, stationName){
 # df<- join_data
  df$year <- format(as.Date(df$Date), "%Y")
  df$month <- format(as.Date(df$Date), "%m")
  df$Rain <- ifelse(is.na(df$Rain), mean(df$Rain, na.rm = T), df$Rain)
  df$Tide <- ifelse(is.na(df$Tide), mean(df$Tide, na.rm = T), df$Tide)
  
  
  # Percentile Normalization Function
  data_normalized_percentile <- function(df, variable) {
    df %>%
      group_by(year) %>%
      mutate(value_percentile = rank(get(variable)) / n(),
             value_normalized = value_percentile * 100) %>%
      ungroup()
  }
  
  # Apply percentile normalization to rain and tide
  data_normalized_rain <- data_normalized_percentile(df, "Rain")
  data_normalized_tide <- data_normalized_percentile(df, "Tide")
  
  
  # Join the normalized data
  normalized_data <- data.frame(data_normalized_rain, data_normalized_tide)
  
  
  # Calculate distance 
  calculate_distance <- function(rain, tide) {
    sqrt((100 - rain) ^ 2 + (100 - tide) ^ 2)
  }
  
  
  # Apply the distance calculation
  data_with_distance <- normalized_data %>%
    group_by(year) %>%
    mutate(distance_from_extreme = calculate_distance(value_normalized, value_normalized.1)) %>%
    slice(which.min(distance_from_extreme)) %>%
    ungroup()
  
  # Test with Kendall


  # Plot time series data
   # par(mfrow = c(2,1))
   #  plot(data_with_distance$Date, data_with_distance$SumRain, type ="l", col = "blue", xlab= "Date", ylab= "Precipitation(mm)", main = stationName)
   #  plot(data_with_distance$Date, data_with_distance$MaxTideLevel, type ="l", col = "red", xlab= "Date", ylab= "Precipitation(mm)")
   # 
  # Return to data table
   return(data_with_distance)

}
