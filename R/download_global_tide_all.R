# Load necessary libraries
library(dplyr)
library(httr)

download_global_tide_all<- function(fastDelivery, researchQuality) {
# Read CSV files
tide_station_rq <- researchQuality 
# tide_station <- read.csv("tide_station.csv")
tide_station <- fastDelivery


# Convert date columns to Date type
tide_station_rq$Start <- as.Date(tide_station_rq$Start, "%m/%d/%Y")
tide_station_rq$End <- as.Date(tide_station_rq$End, "%m/%d/%Y")
tide_station$Start <- as.Date(tide_station$Start, "%m/%d/%Y")
tide_station$End <- as.Date(tide_station$End, "%m/%d/%Y")

# Create an empty list to store combined data by station
combined_data_by_station <- list()

con <- dbConnect(RSQLite::SQLite(), "database/data.sqlite")
# Loop through each unique UH value in tide_station
for (i in 1:length(tide_station[,1])) {
   
  current_UH <- tide_station$UH.[i]
    
  a <- filter(tide_station_rq, UH. == current_UH)
  b <- filter(tide_station, UH. == current_UH)
  
  # Format UH. values with leading zeros
  a$UH. <- sprintf("%03d", as.numeric(a$UH.))
  b$UH. <- sprintf("%03d", as.numeric(b$UH.))
  
  version_data_list <- list()
  
  # Check the number of unique versions in 'a'
  unique_versions <- unique(a$Version)
  n_versions <- length(unique_versions)
  
  # Download data based on the number of versions
  if (n_versions == 1) {
    # Only "a"
    url1 <- paste0("https://uhslc.soest.hawaii.edu/data/csv/fast/daily/d",  b$UH.[1], ".csv")  
    url2 <- paste0("https://uhslc.soest.hawaii.edu/data/csv/rqds/pacific/daily/d", 
                   a$UH.[1], unique_versions[1], ".csv") 
    url <- ifelse (is.null(url1),  url2, url1)
    
    print(paste("Attempting to download:", url))
    
    response <- GET(url)
    
    if (http_status(response)$category == "Success") {
      list_file <- read.csv(url)
      names(list_file) <- c("Year", "Month", "Day", "Tide")
      list_file$source_UH <- b$UH.[1]
      list_file$source_version <- "update"
       version_data_list[[length(version_data_list) + 1]] <- list_file
      
    } else {
      message(paste("Failed to download:", url, "HTTP Status:", http_status(response)$message))
    }
    
  } else if (n_versions == 2) {
    # "a" and "b"

    if (as.numeric(current_UH) >= 100 & as.numeric(current_UH) <= 200) {
      url1 <- paste0("https://uhslc.soest.hawaii.edu/data/csv/rqds/indian/daily/d", 
                     a$UH.[1], unique_versions[1], ".csv")  
    } else if ((as.numeric(current_UH) > 200 & as.numeric(current_UH) <= 300) | (as.numeric(current_UH) >= 600 & as.numeric(current_UH) <= 880)) {
      url1 <- paste0("https://uhslc.soest.hawaii.edu/data/csv/rqds/atlantic/daily/d", 
                     a$UH.[1], unique_versions[1], ".csv")  
    } else if (as.numeric(current_UH) >= 900) {
      url1 <- paste0("https://uhslc.soest.hawaii.edu/data/csv/rqds/indian/daily/d", 
                     a$UH.[1], unique_versions[1], ".csv") 
    } else {
      url1 <- paste0("https://uhslc.soest.hawaii.edu/data/csv/rqds/pacific/daily/d", 
                     a$UH.[1], unique_versions[1], ".csv") 
    }
    
    
    
    url2 <- paste0("https://uhslc.soest.hawaii.edu/data/csv/fast/daily/d", 
                   b$UH.[1], ".csv")  
    
    print(paste("Attempting to download:", url1))
    response1 <- GET(url1)
    
    if (http_status(response1)$category == "Success") {
      list_file1 <- read.csv(url1)
      names(list_file1) <- c("Year", "Month", "Day", "Tide")
      list_file1$source_UH <- a$UH.[1]
      list_file1$source_version <- unique_versions[1]
      version_data_list[[length(version_data_list) + 1]] <- list_file1
     
    } else {
      message(paste("Failed to download:", url1, "HTTP Status:", http_status(response1)$message))
    }
    
    print(paste("Attempting to download:", url2))
    response2 <- GET(url2)
    
    if (http_status(response2)$category == "Success") {
      list_file2 <- read.csv(url2)
      names(list_file2) <- c("Year", "Month", "Day", "Tide")
      list_file2$source_UH <- b$UH.[1]
      list_file2$source_version <- paste(unique_versions[1],"next")
      version_data_list[[length(version_data_list) + 1]] <- list_file2
      
    } else {
      message(paste("Failed to download:", url2, "HTTP Status:", http_status(response2)$message))
    }
    
  } else if (n_versions == 3) {
    # "a", "b", and another version
    if (as.numeric(current_UH) >= 100 & as.numeric(current_UH) <= 200) {
      url1 <- paste0("https://uhslc.soest.hawaii.edu/data/csv/rqds/indian/daily/d", 
                     a$UH.[1], unique_versions[1], ".csv")  
    } else if ((as.numeric(current_UH) > 200 & as.numeric(current_UH) <= 300) | (as.numeric(current_UH) >= 600 & as.numeric(current_UH) <= 880)) {
      url1 <- paste0("https://uhslc.soest.hawaii.edu/data/csv/rqds/atlantic/daily/d", 
                     a$UH.[1], unique_versions[1], ".csv")  
    } else if (as.numeric(current_UH) >= 900) {
      url1 <- paste0("https://uhslc.soest.hawaii.edu/data/csv/rqds/indian/daily/d", 
                     a$UH.[1], unique_versions[1], ".csv") 
    } else {
      url1 <- paste0("https://uhslc.soest.hawaii.edu/data/csv/rqds/pacific/daily/d", 
                     a$UH.[1], unique_versions[1], ".csv") 
    }
    
    if (as.numeric(current_UH) >= 100 & as.numeric(current_UH) <= 200) {
      url2 <- paste0("https://uhslc.soest.hawaii.edu/data/csv/rqds/indian/daily/d", 
                     a$UH.[1], unique_versions[2], ".csv")  
    } else if ((as.numeric(current_UH) > 200 & as.numeric(current_UH) <= 300) | (as.numeric(current_UH) >= 600 & as.numeric(current_UH) <= 880)) {
      url2 <- paste0("https://uhslc.soest.hawaii.edu/data/csv/rqds/atlantic/daily/d", 
                     a$UH.[1], unique_versions[2], ".csv")  
    } else if (as.numeric(current_UH) >= 900) {
      url2 <- paste0("https://uhslc.soest.hawaii.edu/data/csv/rqds/indian/daily/d", 
                     a$UH.[1], unique_versions[2], ".csv") 
    } else {
      url2 <- paste0("https://uhslc.soest.hawaii.edu/data/csv/rqds/pacific/daily/d", 
                     a$UH.[1], unique_versions[2], ".csv") 
    }
    
    
    url3 <- paste0("https://uhslc.soest.hawaii.edu/data/csv/fast/daily/d", 
                   b$UH.[1], ".csv")  
    
    print(paste("Attempting to download:", url1))
    response1 <- GET(url1)
    
    if (http_status(response1)$category == "Success") {
      list_file1 <- read.csv(url1)
      names(list_file1) <- c("Year", "Month", "Day", "Tide")
      list_file1$source_UH <- a$UH.[1]
      list_file1$source_version <- unique_versions[1]
      version_data_list[[length(version_data_list) + 1]] <- list_file1
      # print(paste("Data from:", url, "downloaded successfully."))
    } else {
      message(paste("Failed to download:", url1, "HTTP Status:", http_status(response1)$message))
    }
    
    print(paste("Attempting to download:", url2))
    response2 <- GET(url2)
    
    if (http_status(response2)$category == "Success") {
      list_file2 <- read.csv(url2)
      names(list_file2) <- c("Year", "Month", "Day", "Tide")
      list_file2$source_UH <- a$UH.[1]
      list_file2$source_version <- unique_versions[2]
       version_data_list[[length(version_data_list) + 1]] <- list_file2
      # print(paste("Data from:", url, "downloaded successfully."))
    } else {
      message(paste("Failed to download:", url2, "HTTP Status:", http_status(response2)$message))
    }
    
    print(paste("Attempting to download:", url3))
    response3 <- GET(url3)
    
    if (http_status(response3)$category == "Success") {
      list_file3 <- read.csv(url3)
      names(list_file3) <- c("Year", "Month", "Day", "Tide")
      list_file3$source_UH <- b$UH.[1]
      list_file3$source_version <- paste(unique_versions[1],"next")
      version_data_list[[length(version_data_list) + 1]] <- list_file3
      # print(paste("Data from:", url, "downloaded successfully."))
    } else {
      message(paste("Failed to download:", url3, "HTTP Status:", http_status(response3)$message))
    }
    
  } else if (n_versions == 4) {
    # "a", "b", and two more versions
    if (as.numeric(current_UH) >= 100 & as.numeric(current_UH) <= 200) {
      url1 <- paste0("https://uhslc.soest.hawaii.edu/data/csv/rqds/indian/daily/d", 
                     a$UH.[1], unique_versions[1], ".csv")  
    } else if ((as.numeric(current_UH) > 200 & as.numeric(current_UH) <= 300) | (as.numeric(current_UH) >= 600 & as.numeric(current_UH) <= 880)) {
      url1 <- paste0("https://uhslc.soest.hawaii.edu/data/csv/rqds/atlantic/daily/d", 
                     a$UH.[1], unique_versions[1], ".csv")  
    } else if (as.numeric(current_UH) >= 900) {
      url1 <- paste0("https://uhslc.soest.hawaii.edu/data/csv/rqds/indian/daily/d", 
                     a$UH.[1], unique_versions[1], ".csv") 
    } else {
      url1 <- paste0("https://uhslc.soest.hawaii.edu/data/csv/rqds/pacific/daily/d", 
                     a$UH.[1], unique_versions[1], ".csv") 
    }
    
    if (as.numeric(current_UH) >= 100 & as.numeric(current_UH) <= 200) {
      url2 <- paste0("https://uhslc.soest.hawaii.edu/data/csv/rqds/indian/daily/d", 
                     a$UH.[1], unique_versions[2], ".csv")  
    } else if ((as.numeric(current_UH) > 200 & as.numeric(current_UH) <= 300) | (as.numeric(current_UH) >= 600 & as.numeric(current_UH) <= 880)) {
      url2 <- paste0("https://uhslc.soest.hawaii.edu/data/csv/rqds/atlantic/daily/d", 
                     a$UH.[1], unique_versions[2], ".csv")  
    } else if (as.numeric(current_UH) >= 900) {
      url2 <- paste0("https://uhslc.soest.hawaii.edu/data/csv/rqds/indian/daily/d", 
                     a$UH.[1], unique_versions[2], ".csv") 
    } else {
      url2 <- paste0("https://uhslc.soest.hawaii.edu/data/csv/rqds/pacific/daily/d", 
                    a$UH.[1], unique_versions[2], ".csv") 
    }
    
    if (as.numeric(current_UH) >= 100 & as.numeric(current_UH) <= 200) {
      url3 <- paste0("https://uhslc.soest.hawaii.edu/data/csv/rqds/indian/daily/d", 
                     a$UH.[1], unique_versions[3], ".csv")  
    } else if ((as.numeric(current_UH) > 200 & as.numeric(current_UH) <= 300) | (as.numeric(current_UH) >= 600 & as.numeric(current_UH) <= 880)) {
      url3 <- paste0("https://uhslc.soest.hawaii.edu/data/csv/rqds/atlantic/daily/d", 
                     a$UH.[1], unique_versions[3], ".csv")  
    } else if (as.numeric(current_UH) >= 900) {
      url3 <- paste0("https://uhslc.soest.hawaii.edu/data/csv/rqds/indian/daily/d", 
                     a$UH.[1], unique_versions[3], ".csv") 
    } else {
      url3 <- paste0("https://uhslc.soest.hawaii.edu/data/csv/rqds/pacific/daily/d", 
                    a$UH.[1], unique_versions[3], ".csv") 
    }
    
    url4 <- paste0("https://uhslc.soest.hawaii.edu/data/csv/fast/daily/d", 
                   b$UH.[1], ".csv")  
    
    print(paste("Attempting to download:", url1))
    response1 <- GET(url1)
    
    if (http_status(response1)$category == "Success") {
      list_file1 <- read.csv(url1)
      names(list_file1) <- c("Year", "Month", "Day", "Tide")
      list_file1$source_UH <- a$UH.[1]
      list_file1$source_version <- unique_versions[1]
       version_data_list[[length(version_data_list) + 1]] <- list_file1
      # print(paste("Data from:", url, "downloaded successfully."))
    } else {
      message(paste("Failed to download:", url1, "HTTP Status:", http_status(response1)$message))
    }
    
    print(paste("Attempting to download:", url2))
    response2 <- GET(url2)
    
    if (http_status(response2)$category == "Success") {
      list_file2 <- read.csv(url2)
      names(list_file2) <- c("Year", "Month", "Day", "Tide")
      list_file2$source_UH <- a$UH.[1]
      list_file2$source_version <- unique_versions[2]
       version_data_list[[length(version_data_list) + 1]] <- list_file2
      # print(paste("Data from:", url, "downloaded successfully."))
    } else {
      message(paste("Failed to download:", url2, "HTTP Status:", http_status(response2)$message))
    }
    
    print(paste("Attempting to download:", url3))
    response3 <- GET(url3)
    
    if (http_status(response3)$category == "Success") {
      list_file3 <- read.csv(url3)
      names(list_file3) <- c("Year", "Month", "Day", "Tide")
      list_file3$source_UH <- a$UH.[1]
      list_file3$source_version <- unique_versions[3]
       version_data_list[[length(version_data_list) + 1]] <- list_file3
      # print(paste("Data from:", url, "downloaded successfully."))
    } else {
      message(paste("Failed to download:", url3, "HTTP Status:", http_status(response3)$message))
    }
    
    print(paste("Attempting to download:", url4))
    response4 <- GET(url4)
    
    if (http_status(response4)$category == "Success") {
      list_file4 <- read.csv(url4)
      names(list_file4) <- c("Year", "Month", "Day", "Tide")
      list_file4$source_UH <- b$UH.[1]
      list_file4$source_version <- paste(unique_versions[1],"next")
       version_data_list[[length(version_data_list) + 1]] <- list_file4
      # print(paste("Data from:", url, "downloaded successfully."))
    } else {
      message(paste("Failed to download:", url4, "HTTP Status:", http_status(response4)$message))
    }
  }
  
  
  # Combine all data for the current UH
  if (length(version_data_list) > 0) {
    combined_data <- bind_rows(version_data_list)
    combined_data_by_station[[current_UH]] <- combined_data
    dbWriteTable(con, paste0("Global_Tide_", current_UH), combined_data, overwrite = T)
    # write.csv(combined_data, file = paste0("Global_Tide/Global_Tide_", current_UH, ".csv"), row.names = FALSE)
  }
}
   dbDisconnect(con)
}
