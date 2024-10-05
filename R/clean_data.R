

# ====== CLEARN TIDE DATA FROM HOBO ==============================
# These functions are used to clean tidal data from Hobo database
# Written by QUAN DAU
# Climate Smart Lab, UPEI
# ----------------------------------------------------------------


clean_data <- function(dataset, WL_CGVD2013, WL, P, datum) {
  
  # Creating a sequence of date
  start_date <-
    as.POSIXct("01/01/2021 00:00", format = "%m/%d/%Y %H:%M")
  end_date <-
    as.POSIXct("08/25/2024 23:55", format = "%m/%d/%Y %H:%M")
  date_sequence <-
    as.data.frame(seq(from = start_date, to = end_date, by = "5 mins"))
  names(date_sequence) <- "Date"
  
  # Select the required data
  dataset %>%
    dplyr::select(Date = Date,  WL_CGVD2013,  WL,  P)
  
  # Formatting date
  dataset$Date <- as.POSIXct(dataset$Date, format = "%m/%d/%Y %H:%M")
  names(dataset) <- c("Date", "WL_CGVD2013", "WL", "P")
  
  # Convert CGVD2013 to CGVD28
  dataset$WL2 <-
    ifelse(is.na(dataset$WL),
           dataset$WL_CGVD2013 - datum,
           dataset$WL)
  
  # Joint original data with selected date
  join_dataset <- left_join(date_sequence, dataset, by = "Date")
}


sort_by_year <- function(global_all){
  global_wl_name <- names(global_all)
  count_years_list <- list()  
  row_count_df <- data.frame(Data_Name = character(), Row_Count = numeric(), stringsAsFactors = FALSE)
  
  for (name in global_wl_name) {
    
    global_data_all <- global_all[[name]]
    
    # Group by Year and count rows for each year
    count_year <- global_data_all %>%
      group_by(Year) %>%
      summarise(Row_Count = n())
    
    # Store the count_year data frame for each dataset in the list
    count_years_list[[name]] <- count_year
  }
  
  
  for (name in global_wl_name) {
    
    # Extract the data frame from count_years_list (or global_all)
    count_data <- count_years_list[[name]]  
    
    # Get the number of rows for the current data frame
    row_count <- nrow(count_data)
    
    # Append the result to the data frame
    row_count_df <- rbind(row_count_df, data.frame(Data_Name = name, Row_Count = row_count))
  }
  return(row_count_df)
}



sort_by_day <- function(global_all) {
  global_wl_name <- names(global_all)
  count_days_list <- list()  
  row_count_df <- data.frame(Data_Name = character(), Missing_Day_Ratio = numeric(), stringsAsFactors = FALSE)
  
  for (name in global_wl_name) {
    global_data_all <- global_all[[name]]
    
    # Create a Date column from Year, Month, and Day
    global_data_all$Date <- as.Date(with(global_data_all, paste(Year, Month, Day, sep = "-")), "%Y-%m-%d")
    
    # Determine the complete sequence of dates from the dataset
    complete_dates <- seq(min(global_data_all$Date, na.rm = TRUE), 
                          max(global_data_all$Date, na.rm = TRUE), 
                          by = "day")
    
    # Count total days in the complete sequence
    total_days <- length(complete_dates)
    
    # Count unique days present in the dataset
    unique_days <- length(unique(global_data_all$Date))
    
    # Calculate the number of missing tide readings
    missing_days <- max(0, (total_days - unique_days))
    
    # Calculate the missing day ratio
    missing_day_ratio <- missing_days / total_days *100
    
    # Store results in the list
    count_days_list[[name]] <- data.frame(Data_Name = name, Missing_Day_Ratio = missing_day_ratio)
  }
  
  # Combine all results into a single data frame
  row_count_df <- do.call(rbind, count_days_list)
  
  return(row_count_df)
}

