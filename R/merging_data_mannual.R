

#Loading Rain data from local
rain_path <- "./Climate_Data/"
file_list_rain <- list.files(rain_path, pattern = "\\.csv$", full.names = TRUE)


file_list <- list.files("./Tide/")
id <- as.numeric(gsub(".*-(\\d+)\\.csv", "\\1", file_list))
tide_path <- "./Tide/"
file_list_tide <- list.files(tide_path, pattern = "\\.csv$", full.names = TRUE)


rain_all <- lapply(file_list_rain, function(file) {
  tryCatch({
    read.csv(file)
  }, error = function(e) {
    message("Error reading file ", file, ": ", e$message)
    return(NULL)
  })
})

rain_all <- rain_all[!sapply(rain_all, is.null)]
names(rain_all) <- lapply(file_list_rain, function(x)
  sub(rain_path, "", x))


#Load tide data from local `file_list_tide`
tide_all <- lapply(file_list_tide, function(file) {
  tryCatch({
    read.csv(file)
  }, error = function(e) {
    message("Error reading file ", file, ": ", e$message)
    return(NULL)
  })
})

tide_all <- tide_all[!sapply(tide_all, is.null)]
names(tide_all) <- lapply(file_list_tide, function(x)
  sub(paste0(tide_path, "data-"), "", x))

rain_ids <- sapply(names(rain_all), function(x)
  sub("-.*", "", x))
tide_ids <- sapply(names(tide_all), function(x)
  sub(".csv", "", x))


merging_data_mannual <- function(merge_pair_mannual) {
  
  
  merged_data <- list()

#Merging data for rain and tide based on marching ID
for (i in seq_along(rain_ids)) {
  rain_id <- rain_ids[i]
  matching_tide_index <- which(tide_ids == rain_id)
  
  if (length(matching_tide_index) > 0) {
    rain_data <- rain_all[[i]]
    tide_data <- tide_all[[matching_tide_index]]
    
    
    if (!"Date" %in% colnames(tide_data) ) {
      tide_data$Date <- as.Date(with(tide_data, paste(Year, Month, Day, sep = "-")), "%Y-%m-%d")
    } else {
      tryCatch({
        tide_data$Date <- as.POSIXct(with(data, paste(Year, Month, Day, Hour, sep = "-")), format = "%Y-%m-%d-%H")
      }, error = function(e) {
        return(NULL)
      })
    }
    
    if (!"Date" %in% colnames(rain_data)) {
      stop(paste("Column 'Date' not found in rain_data for ID:", rain_id))
    }
    
    
    tryCatch({
      rain_data$Date <- as.character(rain_data$Date)
      rain_data$Date <- as.Date(rain_data$Date, format = "%Y-%m-%d")
      
      tide_data$Date <- as.Date(tide_data$Date)
      tide_data <- tide_data[complete.cases(tide_data$Tide), ]
      
      merged_df <- left_join(tide_data, rain_data, by = "Date")
      merged_df$Tide <- merged_df$Tide / 1000
      
      merged_df <- merged_df[, c(6, 5, 8)]
      names(merged_df) <- c("Date", "Tide", "Rain")
      merged_data[[rain_id]] <- merged_df
      
    }, error = function(e) {
      return(NULL)
    })
    
  } else {
    warning(paste("No matching tide data found for rain ID:", rain_id))
  }
  
 
}
  return(merged_data)
}
