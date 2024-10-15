
#Loading Tide data from Global_Tide
# file_list_global <- list.files("./Global_Tide/")
# id_global <- as.numeric(gsub(".*-(\\d+)\\.csv", "\\1", file_list_global))
# tide_path_global <- "./Global_Tide/"
# file_list_tide_global <- list.files(tide_path_global, pattern = "\\.csv$", full.names = TRUE)


merging_data_automate <- function(merge_pair_automate) {
  
  con <- dbConnect(RSQLite::SQLite(), "database/data.sqlite")
  on.exit(dbDisconnect(con))
  
  # List data of weather near Tide stations
  listdata <- dbReadTable(con, "list_nearby_station_pr_wl")
  
  
  # Tide data
  list_database <- dbListTables(con)
  tide_station_tables <- grep("^Global_Tide_", list_database, value = TRUE)
  tide_station_tables <- tide_station_tables[!is.na(tide_station_tables)]
  
  # tide_all <- lapply(file_list_tide_global, function(file) {
  #   tryCatch({
  #     read.csv(file)
  #   }, error = function(e) {
  #     message("Error reading file ", file, ": ", e$message)
  #     return(NULL)
  #   })
  # })
  # 
  # tide_all <- tide_all[!sapply(tide_all, is.null)]
  # names(tide_all) <- lapply(file_list_tide_global, function(x)
  #   sub(paste0(tide_path_global, "Global_Tide_"), "", x))
  # tide_ids <- sapply(names(tide_all), function(x)
  #   sub(".csv", "", x))
  # 
  # Rain data

  # all_rain_tables <- dbListTables(con)
  station_tables <- grep("^station_", list_database, value = TRUE)
  station_tables <- station_tables[!is.na(station_tables)]  # Remove NA values
 
  

  merged_data <- list()
  
  for (i in 1:nrow(listdata)) {
    
    if (is.na(station_tables[i])) {
      message(paste(
        "Skipping station due to missing station table:",
        listdata$Tide_ID[i]
      ))
      next  # Skip if station table is NA
    }
    

    rain <- dbReadTable(con, paste0("station_", listdata$Climate_ID[i]))[, c(1, 5)]
    names(rain) <- c("Date", "Rain")
    rain$Date <- as.Date(rain$Date, format = "%Y-%m-%d")
    rain$Rain <- ifelse(is.nan(rain$Rain), NA, rain$Rain)
    
    
    tide <- dbReadTable(con, paste0("Global_Tide_", listdata$Tide_ID[i]))[, c(1:4)]
    
    # tide <- tide_all[[paste0(listdata$Tide_ID[i], ".csv")]] [, c(1:4)]
    tide$Date <- as.Date(with(tide, paste(Year, Month, Day, sep = "-")), "%Y-%m-%d")
    tide$Tide <- tide$Tide / 1000
    tide$Tide <- ifelse(tide$Tide == -32.767, NA, tide$Tide)
    
    common_start <- max(min(as.Date(rain$Date)), min(tide$Date))
    common_end <- min(max(as.Date(rain$Date)), max(tide$Date))
    
    tide_filtered <- tide %>%
      filter(Date >= common_start & Date <= common_end) %>%
      group_by(Date) %>%
      summarise(Tide = mean(Tide, na.rm = TRUE))
    
    rain_filtered <- rain %>%
      filter(Date >= common_start & Date <= common_end) %>%
      group_by(Date) %>%
      summarise(Rain = mean(Rain, na.rm = TRUE))
    
    merged_df <- inner_join(tide_filtered,
                            rain_filtered,
                            by = "Date")
    
    if (nrow(merged_df) == 0) {
      message(paste("No matching dates found for station:", listdata$Tide_ID[i]))
      next  # Skip if no matching dates
    }
    
    merged_df <- merged_df[, c("Date", "Rain", "Tide")]
    
    merged_data[[as.character(listdata$Tide_ID[i])]] <- merged_df
    
  }


  return(merged_data)
 
}





get_station_with_adjusted_data <- function(merged_data, station_list) {
  con <- dbConnect(RSQLite::SQLite(), "database/data.sqlite")
  on.exit(dbDisconnect(con))
  for (station_name in names(merged_data)) {
    station_base_name <- gsub("^Global_Tide_", "", station_name)
    adjusted_version <- paste0(station_base_name, "_adjusted")
   
        if (adjusted_version %in% station_list) {
      merged_data[[station_name]] <- 
        dbReadTable(con, paste0("Global_Tide_", adjusted_version))
       merged_data[[station_name]]$Date <- as.Date(merged_data[[station_name]]$Date)
    } 
  }
  
  return(merged_data)  
}

