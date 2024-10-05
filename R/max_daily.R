
# =========== CALCULATE MAX AND SUM DAILY DATA ==================
# These functions are used to estimate daily data
# Written by QUAN DAU
# Climate Smart Lab, UPEI
# ---------------------------------------------------------------

# Function extract maximum daily daily for annual data
daily_max_tide <- function(data){
  names(data) <- c("Date", "Hour","Tide")
  data <- data %>%
    mutate(Date = as.Date(Date, format = "%m/%d/%Y"))
  daily_max_tides <- data %>%
    group_by(Date = date(Date)) %>%
    summarise(MaxTideLevel = max(Tide, na.rm = TRUE))
}


#  Find the maximum tide for each year
annual_max_tide <- function(data) {
  data %>%
  mutate(Year = format(Date, "%Y")) %>%
  group_by(Year) %>%
  summarise(Max_Tide = max(MaxTideLevel, na.rm = TRUE))
}


#  Find the maximum tide for each year
annual_max_rain <- function(data) {
  data %>%
    mutate(Year = format(Date, "%Y")) %>%
    group_by(Year) %>%
    summarise(Sum_Rain = max(SumRain, na.rm = TRUE))
}

extract_daily <- function(data, startDate, endDate){
  data %>%
  filter(Date >= startDate & Date <= endDate)
}


# Function extract maximum daily data
max_daily <- function(data){
  data <- data[complete.cases(data$WL2), ]
  data$Date <-
    as.POSIXct(data$Date, format = "%m/%d/%Y %H:%M")
  
  # Calculate the maximum tide level for each day
  daily_max_tides <- data %>%
    group_by(Date = date(Date)) %>%
    summarise(MaxTideLevel = max(WL2))
  
}


# Function extract sum daily data
sum_daily <- function(data, Station){
  data$Date <- as.POSIXct(data$Date, format = "%m/%d/%Y %H:%M")
  
  # Calculate the daily sum of rainfall
  daily_sum_rain <- data %>%
    group_by(Date = as.Date(Date)) %>%
    summarise(SumRain = sum(get(Station), na.rm = TRUE))
  
  # Return the result
  return(daily_sum_rain)
}
