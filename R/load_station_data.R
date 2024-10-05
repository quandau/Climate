


# MEAN TIDE FOR CHARLOTTETOWN -SHINY
station_data_mean_1700 <- function(station1700) {
  tide <-  read.csv("1700_charlottetown/mean_WL_CD_local.csv")
  names(tide) <- c("Date", "Tide")
  
  tide$Date <- as.Date(tide$Date, format = "%m/%d/%Y")

  rain_1872_1943 <- dplyr::select(read.csv("1700_charlottetown/data-6525-1872-01-01-2024-09-14.csv"), date, total_precip)
  rain_1943_2012 <- dplyr::select(read.csv("1700_charlottetown/data-6526-1943-01-01-2024-09-14.csv"), date, total_precip)
  rain_2012_2024 <- dplyr::select(read.csv("1700_charlottetown/daily-50621-2012-09-14-2024-08-05.csv"), date, total_precip)
  
  combine_rain <- rbind(rain_1872_1943, rain_1943_2012, rain_2012_2024)
  
  sum_daily_rain <- combine_rain %>%
    group_by(Date = as.Date(date)) %>%
    summarise(SumRain = sum(total_precip, na.rm = TRUE))

  
  df <- left_join(tide, sum_daily_rain, by = "Date")
  return(df)
}





# CHARLOTTETOWN - SHINY
station_charlottetown <- function(station_charlottetown) {
  tide <- read.csv("1700_charlottetown/tide_data.csv") # measure by CD
  max_daily_tide <- daily_max_tide(tide)
  # annual_max_tide <- annual_max_tide(max_daily_tide)
  
  rain_1872_1943 <- dplyr::select(read.csv("1700_charlottetown/data-6525-1872-01-01-2024-09-14.csv"), date, total_precip)
  rain_1943_2012 <- dplyr::select(read.csv("1700_charlottetown/data-6526-1943-01-01-2024-09-14.csv"), date, total_precip)
  rain_2012_2024 <- dplyr::select(read.csv("1700_charlottetown/daily-50621-2012-09-14-2024-08-05.csv"), date, total_precip)
  
  combine_rain <- rbind(rain_1872_1943, rain_1943_2012, rain_2012_2024)
  
  sum_daily_rain <- combine_rain %>%
    group_by(Date = as.Date(date)) %>%
    summarise(SumRain = sum(total_precip, na.rm = TRUE))
  
  # annual_max_rain <- sum_daily_rain %>%
  #   mutate(Year = format(Date, "%Y")) %>%
  #   group_by(Year) %>%
  #   summarise(Sum_Rain = max(SumRain, na.rm = TRUE))
  
  df <- left_join(max_daily_tide, sum_daily_rain, by = "Date")
  return(df)
}


# STATION 575
station_data_575 <- function(station575) {
  tide1 <- read.csv("Tide/575_port_hawkesbury/575-01-JAN-2015_slev.csv")
  
  names(tide1) <- c("Date", "Tide")
  tide1 <- tide1 %>%
    mutate(Date = as.Date(Date, format = "%m/%d/%Y"))
  daily_max_tides <- tide1 %>%
    group_by(Date = date(Date)) %>%
    summarise(MaxTideLevel = max(Tide, na.rm = TRUE))
  
  rain1 <- read.csv("Tide/575_port_hawkesbury/Port_Hawkesbury_00575_rain.csv") %>%
    select(date, total_precip)
  
  rain1$date <- as.POSIXct(rain1$date)
  
  names(rain1) <- c("Date", "SumRain")
  
  df <- left_join(daily_max_tides, rain1, by = "Date")
  return(df)
}
 
 
 # STATION 1805
station_data_1805 <- function(station1805) {
  tide1 <- read.csv("Tide/1805_shediacbay/1805-01-JAN-1995_slev.csv")
  tide2 <- read.csv("Tide/1805_shediacbay/1805-01-JAN-2005_slev.csv")
  tide3 <- read.csv("Tide/1805_shediacbay/1805-01-JAN-2015_slev.csv")
  tide4 <- rbind(tide1, tide2, tide3)
  
  names(tide4) <- c("Date", "Tide")
  tide4 <- tide4 %>%
    mutate(Date = as.Date(Date, format = "%m/%d/%Y"))
  daily_max_tides <- tide4 %>%
    group_by(Date = date(Date)) %>%
    summarise(MaxTideLevel = max(Tide, na.rm = TRUE))
  
  rain1 <-  read.csv("Tide/1805_shediacbay/data-6207-2003-01-01-2024-09-18.csv") %>%
    select(date, total_precip)
  rain2 <-  read.csv("Tide/1805_shediacbay/data-50309-2003-01-01-2024-09-18.csv") %>%
    select(date, total_precip)
  rain2 <- rain2[-c(1:4), ]
  
  rain3 <- rbind(rain1, rain2)
  rain3$date <- as.POSIXct(rain3$date)
  names(rain3) <- c("Date", "SumRain")
  df <- left_join(daily_max_tides, rain3, by = "Date")
  return(df)
}

