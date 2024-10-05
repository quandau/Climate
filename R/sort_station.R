


years_diff <- function(start_date, end_date) {
  if (is.na(start_date) | is.na(end_date)) {
    return(NA)  # Return NA if either date is missing
  }
  
  # Extract the year as numeric
  start_year <- as.numeric(format(start_date, "%Y"))
  end_year <- as.numeric(format(end_date, "%Y"))
  
  # Check if the start year is a leap year
  is_leap_year <- function(year) {
    (year %% 4 == 0 & year %% 100 != 0) | (year %% 400 == 0)
  }
  
  # Calculate the difference in days and divide by appropriate year length
  total_days <- as.numeric(difftime(end_date, start_date, units = "days"))
  total_years <- total_days / ifelse(is_leap_year(start_year), 366, 365)
  
  return(total_years)
}

sort_station <- function(threshold){
  
  tide_station_rq <- read.csv("tide_station_rq.csv")
  tide_station <- read.csv("tide_station.csv")
  
  tide_station_rq$Start <- as.Date(tide_station_rq$Start, "%m/%d/%Y")
  tide_station_rq$End <- as.Date(tide_station_rq$End, "%m/%d/%Y")
  tide_station$Start <- as.Date(tide_station$Start, "%m/%d/%Y")
  tide_station$End <- as.Date(tide_station$End, "%m/%d/%Y")
 
  tide_station$Start_Date <- NA
  tide_station$End_Date <- NA
 
  for (i in 1:nrow(tide_station)) {
    # Get the UH. value for the current row
    current_UH <- tide_station$UH.[i]
    
    # Filter based on current UH. value
    a <- filter(tide_station_rq, UH. == current_UH)
    b <- filter(tide_station, UH. == current_UH)

    # Update Start_Date and End_Date based on conditions
    ifelse (length(a[,1])>1,
      tide_station$Start_Date[i] <- as.Date(a$Start[1], format = "%Y-%m-%d"),
       tide_station$Start_Date[i] <- as.Date(b$Start[1], format = "%Y-%m-%d"))

      tide_station$End_Date[i]   <- as.Date(b$End[1], format = "%Y-%m-%d")
    
  }
  
  tide_station$Start_Date <- as.Date(tide_station$Start_Date, format = "%Y-%m-%d")
  tide_station$End_Date <- as.Date(tide_station$End_Date, format = "%Y-%m-%d")
  
  tide_station$DifTime <- mapply(years_diff, tide_station$Start_Date, tide_station$End_Date)
  stations_greater_threshold <- tide_station[tide_station$DifTime > threshold, ]
 
  return(stations_greater_threshold)
}


