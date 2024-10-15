




function(input, output, session) {

  # Loading all data  
 
  source("R/clean_data.R")
  source("R/plot.R")
  source("R/copulas.R")
  source("R/max_daily.R")
  source("R/sort_data.R")
  source("R/load_station_data.R")
  source("R/distance_quantile_normalise.R")
  source("R/idf.R")
  source("R/sort_station.R")
  source("R/api_weather_station.R")
  source("R/download_global_tide_all.R")
  source("R/download_global_weather_all.R")
  source("R/merging_data_automate.R")
  source("R/merging_data_mannual.R")
  
  update_modal_text <- function(text) {
    removeModal()
    showModal(modalDialog(
      div(
        style = "display: flex; justify-content: center;  align-items: center; height: 20vh;",
        div(
          style = "text-align: center;",
          img(src = 'https://loading.io/assets/mod/spinner/atom/sample.gif', style = "width: 80px; height: 80px;"), 
          h3(text)
        )
      ),
      footer = NULL,
      easyClose = FALSE
    ))
  }
  
  update_modal_text("Please wait ! Loading data ...")
  # loading stations globally
  # tide_station <- read.csv("tide_station.csv")
  # tide_station_rq <- read.csv("tide_station_rq.csv")
  con <- dbConnect(RSQLite::SQLite(), "database/data.sqlite")
  tide_station <- dbReadTable(con, "tide_station")
  tide_station_rq <- dbReadTable(con, "tide_station_rq")
  dbDisconnect(con)
  #Loading Tide data from local
  # file_list <- list.files("./Tide/")
  # id <- as.numeric(gsub(".*-(\\d+)\\.csv", "\\1", file_list))
  # tide_path <- "./Tide/"
  # file_list_tide <- list.files(tide_path, pattern = "\\.csv$", full.names = TRUE)
  # 
  #Loading Rain data from local
  # rain_path <- "./Climate_Data/"
  # file_list_rain <- list.files(rain_path, pattern = "\\.csv$", full.names = TRUE)
  
  # Loading data for specific Charlottetown
  startDate <- "1943-01-01"
  endDate <-   "2023-12-31"
  cha_data <- extract_daily(station_data_mean_1700(station1700) , startDate, endDate)
  names(cha_data) <- c("Date", "Tide", "Rain")
  
 
  # global_path <- "./Global_Tide/"
  # file_list_global <- list.files(global_path, pattern = "\\.csv$", full.names = TRUE)
 
  
  
   
   
  # ---------------------------------------------------------------------------------
  #====================================CAN WEATHER ==================================
  
  # Search box station
  observe({ updateSelectizeInput(session, "station", choices =  weathercan::stations_search(input$search_station, interval = input$interval)[3] ) })
  
  #Render the tables
  output$data_table <- renderTable({
    head(weathercan::stations_search(input$search_station, interval = input$interval))[, c(1:4, 11:12)]
         })
  
  # Zoom to station 
  output$map <- renderLeaflet({
    tryCatch({
     out <- head(weathercan::stations_search(input$search_station, interval = input$interval))
    }, error = function(e) {
      update_modal_text("Error !!")
      return(NULL) 
    })
    
    if (is.null(out) || nrow(out) == 0) {
      return(NULL)
    }
    leaflet(out, options = leafletOptions(attributionControl = FALSE)) %>% addTiles() %>%
      addCircleMarkers(
        lng = ~ lon, lat = ~ lat, popup = ~ as.character(paste("Station ID: ", station_id))
      )
   })
  
  
  observe({
    # Validate data
    observeEvent(input$valid, { 
      update_modal_text("Please wait ! Loading data ...")
      
      # Create string to data
      data <-
        weather_dl(
          station_ids = input$station,
          start = input$daterange2[1],
          end = input$daterange2[2],
          interval = input$interval
        )
      
      # Show result in table
      output$select_table <- renderTable({
        if (is.null(data)) {
          return(NULL) 
        } else {
          return(head(data)) 
        }
      })
      
      # Display the length of available data
      output$prov <- renderText({
        if (is.null(data)) {
          return("No Data") 
        } else {
          return(
            paste( "Selected data contains ", nrow(data), " rows, available from ", data[1, ]$day,
              "/", data[1, ]$month ,  "/",  data[1, ]$year,   "-", data[nrow(data), ]$day, "/",
              data[nrow(data), ]$month , "/",data[nrow(data), ]$year
            )
          )  
        }
      })
      
      # Display precipitation data
      output$prec_map <- renderPlotly({
        print(str(data))
        if (!"precip_amt" %in% colnames(data)) {
          stop("")
        }
        p <- ggplot(data, aes(x = time, y = precip_amt)) +
          geom_line(aes(y = precip_amt), color = "#004955") +
          theme_few(base_size = 10) +
          ylab("Precipitation")
        plotly::ggplotly(p) %>%
          config(displaylogo = F)
      })
      
      # Display Temperature data
      output$temp_map <- renderPlotly({
        print(str(data))
        if (!"temp" %in% colnames(data)) {
          stop("")
        }
        p <- ggplot(data, aes(x = time, y = temp)) +
          geom_line(aes(y = temp), color = "#001fff") +
          theme_few(base_size = 10) +
          ylab("Temperature")
        plotly::ggplotly(p) %>%
          config(displaylogo = F)
      })
      
      
      removeModal()
      
      #Download data
      output$down <- downloadHandler(
        filename = function() {
          paste("data-",  input$station,  "-", input$daterange2[1], "-", input$daterange2[2],".csv",sep = "")
        },
        content = function(file) {
          write.csv(data, file)
        }
      )
    })
  })
  
  
  
  
  
  # ---------------------------------------------------------------------------------------
  #================================ CAN HYDROLOGY ==============================================
 
   observe({
    
  tryCatch({
    # Update input selection
    updateSelectizeInput( session, "streamstation", choices =  tidyhydat::realtime_stations(prov_terr_state_loc = input$province)[1] )
    
    updateSelectizeInput(session, "param", choices =  tidyhydat::param_id[4])
    
    # Display Head of data
    output$station_table <- renderTable({
      head(tidyhydat::realtime_stations(prov_terr_state_loc = input$province))[, c(1:4)]
        })
        }, error = function(e) { 
          showModal( modalDialog(  "Error: No data retrieved for the selected station query!", footer = tagList(actionButton("closeErrorModal", "Close", class = "btn-default") )))
      })
  })
  
  # View locations on map
  output$map_stream <- renderLeaflet({
    out2 <-tidyhydat::realtime_stations(prov_terr_state_loc = input$province)
    if (is.null(out2) || nrow(out2) == 0) {
      return(NULL)
    }
    
    leaflet(out2, options = leafletOptions(attributionControl = FALSE)) %>% addTiles() %>%
      addCircleMarkers(
        lng = ~ LONGITUDE, lat = ~ LATITUDE,
        popup = ~ as.character(paste( "Station ID: ", STATION_NUMBER, "-", STATION_NAME ))
      )
  })
  
  
  observe({
    #Validate data
    observeEvent(input$valid_stream, {
      update_modal_text("Please wait ! Loading data ...")
      
      data <- 
      tryCatch({
        realtime_ws(
          station_number = input$streamstation,
          parameters = as.numeric(filter(param_id, Name_En == input$param)[1]),
          end_date = input$daterange_stream[2],
          start_date = input$daterange_stream[1]
        )
      }, error = function(e) {
        showModal( modalDialog(  "Error: No data retrieved for the selected station query!", footer = tagList(actionButton("closeErrorModal", "Close", class = "btn-default") )))
        return(NULL) 
      })
      
      # Display data on table
      if (!is.null(data) && !inherits(data, "try-error")) {
        output$select_table_stream <- renderTable({
          head(data)[, c(1:5)]
        })
        
       # Show available data in text
        output$prov_stream <- renderText({
          paste("Selected data contains", nrow(data), "rows, available from", data[1, ]$Date, "-", data[nrow(data), ]$Date)
        })
        
        # View locations on map
        output$stream_map <- renderPlotly({
          p <- ggplot(data, aes(x = Date, y = Value)) +
            geom_line(aes(y = Value), color = "yellowgreen") +
            theme_few(base_size = 10) +
            ylab(input$param)
          plotly::ggplotly(p) %>%
            config(displaylogo = F)
        })
        
        removeModal()
      }
      
      
      # Download data
      output$down_stream <- downloadHandler(
        filename = function() {
          paste("data-", input$streamstation, "-", input$daterange_stream[1],"-",input$daterange_stream[2], ".csv",sep = "" )
        },
        content = function(file) {
          write.csv(data, file)
        }
      )
    })
  })
  
  
  observeEvent(input$closeErrorModal, {
    removeModal()
  })
  
  
  
  
  
  #---------------------------------------------------------------------------------------
  #============================= TIDE DATA ================================================
  
  #Search inputs
  observe({ updateSelectizeInput(session, "search_country", choices =  tide_station[4]) })
  
  observe({ updateSelectizeInput(session,  "search_loc", choices =  filter(tide_station, Country == req(input$search_country))[3]) })
  
  # Search ID station
  observe({
    filtered_data <- filter(tide_station, Location == req(input$search_loc))
    
    formatted_choices <- ifelse( nchar( as.character(filtered_data[1]), allowNA = TRUE,type = "width") == 1, paste0("00", filtered_data[1]),
      ifelse(nchar( as.character(filtered_data[1]), allowNA = TRUE,type = "width") == 2, paste0("0", filtered_data[1]), as.character(filtered_data[1])))
    updateSelectizeInput(session, "search_id", choices = req(formatted_choices))
    
   })
  
  # Selection station to delete from local
  observe({ updateSelectInput(session, "del_station_tide",  choices  =  basename(file_list_tide)) })
  # Delete file
  observeEvent(input$del_station_tide_submit, {
    file.remove(paste0(tide_path, input$del_station_tide))
    showNotification("Station has been deleted", type = "message")
  })
  

  # Function to show global map
  global_map_station <- function(tide_station) {
    renderLeaflet({
    leaflet(tide_station, options = leafletOptions(attributionControl = FALSE)) %>% addTiles() %>%
        addMarkers(
        lng = ~ Longitude,lat = ~ Latitude,
        popup = ~ as.character(paste("ID       =", UH., "<br>", 
                                     "Location =", Location, "<br>", 
                                     "Country  =",Country, "<br>", 
                                     "LAT      = ", Latitude, "<br>", 
                                     "LON      = ", Longitude, "<br>", 
                                     "START    = ", Start, "<br>", 
                                     'END      = ', End))
                    )
             })
    }
  
  # Automatic display global station map
   output$map_tide <- global_map_station(tide_station)
  
   # Reset global map
  observeEvent(input$reset, {
    output$map_tide <- global_map_station(tide_station)
  })
  
  # Filter list all station that being selected
  station_all <- data.frame()
  for (i in 1:length(id)) {
    station_alls <- filter(tide_station, UH. == id[i])
    station_all <- rbind(station_all, station_alls)
  }
  
  # Display global station that being selected
  observeEvent(input$selec_sta, {
    output$map_tide <- global_map_station(station_all)
  })
  
  # Display tide table
  output$table_tide <- renderTable({
    filter(tide_station[, c(1, 3:4, 7:8)], Country == input$search_country)
  })
  
 
  
  observe({
    # Validate data
    observeEvent(input$valid_tide, {
      update_modal_text("Please wait ! Loading data ...")
      
      # link to global tide data
      if (req(input$interval_tide) == "h") {
      url <- paste0("https://uhslc.soest.hawaii.edu/data/csv/fast/hourly/",req(input$interval_tide),req(input$search_id) ,".csv" )
      } else {
      url <- paste0("https://uhslc.soest.hawaii.edu/data/csv/fast/daily/",req(input$interval_tide),req(input$search_id) ,".csv" )
      }
      
      data <- tryCatch({
        read.csv(url, stringsAsFactors = FALSE)
      }, error = function(e) {
        showNotification("No data available for the selected station.", type = "error")
        return(NULL)
      })
      
      
      # Rename columns based on the interval type
      if (req(input$interval_tide) == "h") {
        colnames(data) <- c("Year", "Month", "Day", "Hour", "Tide")
      } else {
        colnames(data) <- c("Year", "Month", "Day", "Tide")
      }
      
      data$Tide <- ifelse(data$Tide == -32767, NA, data$Tide)
      # Update global tide map based on the selection 
      out <- filter(tide_station, Country == input$search_country)
      output$map_tide <- global_map_station(out)
      
      # Display the table of available data
      output$select_table_tide <- renderTable({
        head(data)  #
      })
      
      
      removeModal()
      
      # Download CSV format
      output$down_tide <- downloadHandler(
        filename = function() {
          paste("data-", input$search_id, ".csv", sep = "")
        },
        content = function(file) {
          write.csv(data, file)
        }
      )
      
      # Display time series tide data
      output$tide_map <- renderPlotly({
        req(data)
        if (req(input$interval_tide) == "h") {
          data$Date <- as.POSIXct(with(data, paste(Year, Month, Day, Hour, sep = "-")), 
                                  format = "%Y-%m-%d-%H")
        } else {
          data$Date <- as.Date(with(data, paste(Year, Month, Day, sep = "-")), "%Y-%m-%d")
        }
        
        # Plot the data
        p <- ggplot(data, aes(x = Date, y = Tide)) +
          geom_line(color = "steelblue3") +
          theme_few(base_size = 10)
        
        # Render ggplot as interactive plotly
        plotly::ggplotly(p) %>%
          config(displaylogo = FALSE)
      })
     })
   })
  
  
  
  observe({
   
    sort_station_list<- sort_station(as.numeric(input$year_threshold))
    sort_station_list <- reactiveVal(sort_station_list) 
    valid_station_list <- sort_station_list() %>% filter(!is.na(UH.), UH. != "")
    tide_station_rq <- filter(tide_station_rq, UH. %in% valid_station_list$UH.)
    updateActionButton(session, "global_tide_download", paste("Download " , length(sort_station_list()[, 1]) ," stations"), icon =icon("download"))
    
    # output$count_station <-   renderValueBox({
    #   valueBox( "Station", length(sort_station_list()[, 1]), icon = icon("hashtag"),  color = "purple"
    #   )
    # })
    output$map_tide_sortout <- renderLeaflet({
        leaflet(valid_station_list, options = leafletOptions(attributionControl = FALSE)) %>% addTiles() %>%
          addMarkers(
            lng = ~ Longitude,lat = ~ Latitude,    icon = icon('map-pin'),
            popup = ~ as.character(paste("ID       =", UH., "<br>", 
                                         "Location =", Location, "<br>", 
                                         "Country  =", Country, "<br>", 
                                         "LAT      = ", Latitude, "<br>", 
                                         "LON      = ", Longitude, "<br>", 
                                         "START    = ", Start, "<br>", 
                                         'END      = ', End,"<br>", 
                                         "LENGTH YEAR =", round(DifTime, 2)))
        
          )
      })
    
    # tide_station_rq <- filter(tide_station_rq, UH. %in% sort_station_list()$UH.)
    observeEvent(input$global_tide_download, {
      update_modal_text(paste("Downloading data for All Stations"))
 
       tryCatch({ 
        download_global_tide_all(valid_station_list, tide_station_rq)
      }, error = function(e) {
        update_modal_text("Download Error !")

      })
      removeModal()
    })
  
    
    # Filter by year
    observeEvent(input$filter_by_year , {
     
      update_modal_text("Sorting downloaded data by year...")
    
      con <- dbConnect(RSQLite::SQLite(), "database/data.sqlite")
      on.exit(dbDisconnect(con))
      list_database <- dbListTables(con)
      station_tables <- grep("^Global_Tide_", list_database, value = TRUE)
      
      global_all<- list()
     
       for (i in station_tables) {
      table_data  <- dbReadTable(con, i)
      table_name <- gsub("\\D", "", i) # extract ID from Global_Tide
      global_all[[table_name]] <- table_data
      }
      
      

    #  global_all <- lapply(file_list_global, function(file) {
    #   tryCatch({
    #     read.csv(file)
    #     
    #   }, error = function(e) {
    #     message("Error reading file ", file, ": ", e$message)
    #     return(NULL)
    #   })
    # })
    # names(global_all)<- lapply(file_list_global, function(x) sub(global_path, "", x))

   
    
     sort_year<- sort_by_year(global_all)
     stations_greater_threshold <- sort_year[sort_year$Row_Count > req(input$year_threshold_2), ]
     stations_greater_threshold_ids <- gsub("[^0-9]", "", stations_greater_threshold$Data_Name)
     
     sort_station_list <- filter(tide_station, UH. %in% stations_greater_threshold_ids)

     output$remain_year <- renderText({
       paste("Remain ", nrow(sort_station_list), "station")
     })
     
    
     
     output$map_tide_sortout <- renderLeaflet({
       leaflet(sort_station_list, options = leafletOptions(attributionControl = FALSE)) %>% addTiles() %>%
         addMarkers(
           lng = ~ Longitude,lat = ~ Latitude,    icon = icon('map-pin'),
           popup = ~ as.character(paste("ID       =", UH., "<br>", 
                                        "Location =", Location, "<br>", 
                                        "Country  =", Country, "<br>", 
                                        "LAT      = ", Latitude, "<br>", 
                                        "LON      = ", Longitude, "<br>", 
                                        "START    = ", Start, "<br>", 
                                        'END      = ', End,"<br>"))
          
         )
       
      })
     
     removeModal()
      
     dbDisconnect(con)
    })
    
    
    filtered_tide_stations <- reactiveVal()
    
    observeEvent(input$filter_by_ratio, {
      update_modal_text("Re-computing missing days ...")
   
      if (is.null(sort_year)) {
     sort_year<- sort_by_year(global_all)
      } 
      
    stations_to_keep <- sort_year[sort_year$Row_Count > req(input$year_threshold_2), ] 
      
      
    global_all_filtered <- global_all[names(global_all) %in% stations_to_keep$Data_Name]
      
  
    sort_day<- sort_by_day(global_all_filtered)
      
      
    print(sort_day$Missing_Day_Ratio)
    stations_greater_threshold <- sort_day[sort_day$Missing_Day_Ratio <= req(input$missing_threshold), ]
    stations_greater_threshold_ids <- gsub("[^0-9]", "", stations_greater_threshold$Data_Name)
    
    sort_station_list2 <- filter(tide_station, UH. %in% stations_greater_threshold_ids)
   
    con <- dbConnect(RSQLite::SQLite(), "database/data.sqlite")
    on.exit(dbDisconnect(con))
    dbWriteTable(con, "sort_tide_station", sort_station_list2, overwrite = T)
     
    # write.csv(sort_station_list2, file ="sort_tide_station.csv")
    
    output$remain_station <- renderText({
      paste("Remain ", nrow(sort_station_list2), "station")
    })

    output$map_tide_sortout <- renderLeaflet({
      leaflet(sort_station_list2, options = leafletOptions(attributionControl = FALSE)) %>% addTiles() %>%
        addMarkers(
          lng = ~ Longitude,lat = ~ Latitude,    icon = icon('map-pin'),
          popup = ~ as.character(paste("ID       =", UH., "<br>", 
                                       "Location =", Location, "<br>", 
                                       "Country  =", Country, "<br>", 
                                       "LAT      = ", Latitude, "<br>", 
                                       "LON      = ", Longitude, "<br>", 
                                       "START    = ", Start, "<br>", 
                                       'END      = ', End,"<br>"))
        )
      
    })
    
    removeModal()
    dbDisconnect(con)
    
    })
    
    
  }) 
  # Validation with observation
  # observe({
  # file <- req(input$validate_observe)
  # ext <- tools::file_ext(file$datapath)
  # validate(need(ext == "csv", "Upload CSV file "))
  # vali <- read.csv(file$datapath)
  # names(vali) <- c("Date", "Value")
  # 
  # output$table_validate_observe <- renderPlotly({
  #  ggplot(vali, aes(x = Date, y = Value)) +
  #     geom_line(aes(y = Value), color = "steelblue3") +
  #     theme_few(base_size = 10)
  #  
  # })
  # })
  
  
  #--------------------------------------------------------------------------------------------
  #===============================GLOBAL WEATHER ==============================================
  
  # Time zone looking up based on lat and lon inputs
  observe({
    lat <- req(input$search_lat)
    lon <- req(input$search_lon)
    
    if (is.numeric(as.numeric(lat)) && is.numeric(as.numeric(lon))) {
      tryCatch({
        updateTextInput(session, "time_zone", value = tz_lookup_coords(as.numeric(lat), as.numeric(lon), method = "accurate"))
      }, error = function(e) {
        showModal( modalDialog(  "Please correct your coordinates", footer = tagList(actionButton("closeErrorModal", "Close", class = "btn-default") )))
        return(NULL)
      })
    } 
  })
  
  
  # Input stations from local to delete   
  observe({ updateSelectInput(session, "del_station",  choices  =  basename(file_list_rain)) })
  
  # Delete data from local
  observeEvent(input$del_station_submit, {
      file.remove(paste0(rain_path, input$del_station))
      showNotification("Station has been deleted", type = "message")
    })
  
  # Create a list of station ID
  station_all <- data.frame()
  for (i in 1:length(id)) {
    station_alls <- filter(tide_station, UH. == id[i])
    station_all <- rbind(station_all, station_alls)
  }
  
  # Download all data
  observeEvent(input$extract_coord, {
    
    data_climate <- data.frame()
     
    # Create a loop to download all data
     for (i in 1:length(station_all[,1])){

       update_modal_text(paste(" Downloading data for ",station_all$UH.[i],"-", station_all$Location[i]))
      
       # Format Date for data
      station_all$Start <- as.Date(station_all$Start, format = "%m/%d/%Y")
      station_all$End <- as.Date(station_all$End, format = "%m/%d/%Y")
      
      # Link to online station to download 
      url <- paste0("https://archive-api.open-meteo.com/v1/archive?latitude=",
                    station_all$Latitude[i],"&longitude=",
                    station_all$Longitude[i],"&start_date=",
                    max(format(station_all$Start[i], "%Y-%m-%d"),"1940-01-01" ),"&end_date=",  
                    format(station_all$End[i], "%Y-%m-%d"),"&daily=",
                    as.character(input$climate_variable),"&timezone=", "auto","&format=csv")

      data_climate <- tryCatch({
        # Read data by CSV
        read.csv(url)
      }, error = function(e) {
        message("Error downloading data for ", station_all$UH.[i], ": ", e$message) 
        return(NULL)
      })
      if (is.null(data_climate)) {
        next   # If station is not available we jump to next one
      }

      # Save data to local 
      data_climate <- as.data.frame(data_climate[-c(1:2), c(1:2)]) 
      colnames(data_climate) <- c("Date", "Value")
      data_climate$Date <- as.Date(data_climate$Date, format = "%Y-%m-%d")
      data_climate$Value <- ifelse(is.na(data_climate$Value), 0, data_climate$Value)

      write.csv( data_climate, file = paste0("Climate_Data/", station_all$UH.[i], "-", station_all$Location[i], ".csv"))
     }
    
    removeModal()
  })
  

   # Display global station that being selected
    output$map_global_data <- global_map_station(station_all)
  
    # Display data when click on station map
    observeEvent(input$map_global_data_marker_click, {
      click <- input$map_global_data_marker_click
      tolerance <- 0.001  
      
      station_selected <- station_all %>%
        filter(abs(Longitude - click$lng) < tolerance & abs(Latitude - click$lat) < tolerance)
      
      if (nrow(station_selected) == 0) {
        warning("No matching station found for clicked location.")
        return(NULL)
      }
      
      station_id <- station_selected$UH.[1]
   
      selected_rain_file <- file_list_rain[grep(station_id, file_list_rain)]

      if (length(selected_rain_file) == 0) {
        showNotification(paste("No rain data file found for station ID:", station_id), type = "warning")
        return(NULL)
      }
      
      file_path_rain <- file.path(selected_rain_file)
      if (!file.exists(file_path_rain)) {
        showNotification(paste("Rain data file does not exist:", file_path_rain), type = "warning")
        return(NULL)
      }
      
      selected_rain_data <- tryCatch({
        read.csv(file_path_rain)
      }, error = function(e) {
        showNotification(paste("Error reading rain data file:", file_path_rain), type = "warning")
        return(NULL)
      })
      
      if (is.null(selected_rain_data)) {
        return(NULL)  
      }
      
      selected_rain_data$Date <- as.character(selected_rain_data$Date)
      selected_rain_data$Date <- as.Date(selected_rain_data$Date, format = "%Y-%m-%d")
     
      # If station is valid, it will show on plot
       output$rain_plot_global <- renderPlot({
        plot(selected_rain_data$Date, selected_rain_data$Value, type = "l", col = "turquoise3", xlab = "Date", ylab = "Rainfall (mm)",  main = paste("Rain Data for Station", station_id))
      })
    })
  
  
    observe({ 
     # Validate data
    observeEvent(input$valid_global_data, {
      update_modal_text("Please wait ! Loading data in process ..")
      
      tryCatch({
        # Validate data for individual
        url <- paste0("https://archive-api.open-meteo.com/v1/archive?latitude=",
                      req(as.numeric(input$search_lat)),"&longitude=",
                      req(as.numeric(input$search_lon)),"&start_date=",
                      req(format(as.Date(input$daterange_global[1]), "%Y-%m-%d")), "&end_date=",
                      req(format(as.Date(input$daterange_global[2]), "%Y-%m-%d")), "&daily=",
                      req(input$climate_variable),"&timezone=", "auto","&format=csv")
        
        data_climate<- read.csv(url)
        data_climate<- as.data.frame(data_climate[-c(1:2), ][, c(1:2)])
        names(data_climate) <- c("Date", "Value")
        data_climate$Date <- format(data_climate$Date, "%Y/%m/%d")
        data_climate$Value <- ifelse(is.na(data_climate$Value), 0, data_climate$Value)
      
      }, error = function(e) {
        showNotification("Make sure data should be before 1940-01-01", type = "warning")
      })
      
      # Show summary table of data
      output$weather_global_data <- renderTable({
        req(input$search_lat, input$search_lon)  
        data_frame <- data.frame(   HEAD = head(data_climate), TAIL = tail(data_climate))
        return(data_frame) 
      })
      
      # Download data individual
      output$down_global_data <- downloadHandler(
        filename = function() {
          paste("clima-", input$search_lat,"-", input$search_lon,"-",input$daterange_global[1],"-", input$daterange_global[2], ".csv", sep = "")
        },
        content = function(file) {
          write.csv(data_climate, file)
        }
      )
      
      removeModal()
      
       })
    })
  
    observe({
      
      # Check if data is already cached to prevent repeated downloads
      if (exists("gauging_station_data")) {
        a <- gauging_station_data
      } else {
        # Load and process data only if not cached
        url <- "https://bulk.meteostat.net/v2/stations/full.json.gz"
        response <- GET(url)
        
        # Handle the response and extract relevant data
        temp_file <- tempfile(fileext = ".json.gz")
        writeBin(response$content, temp_file)
        
        json_file <- tempfile(fileext = ".json")
        gunzip(temp_file, json_file)
        
        data <- fromJSON(json_file)
        
        # Select relevant columns and filter rows if needed
        a <- data.frame(
          ID = data$id,
          Station = data$name,
          Country = data$country,
          Region = data$region,
          latitude = data$location$latitude,
          longitude = data$location$longitude,
          Start = data$inventory$daily$start,
          End = data$inventory$daily$end
        ) %>% 
          # Filter for specific regions or other criteria if necessary
          filter(!is.na(latitude) & !is.na(longitude))  # Example filter
        
        # Cache the data for future use
        assign("gauging_station_data", a, envir = .GlobalEnv)
      } 
      
      mydrawPolylineOptions <- function (allowIntersection = TRUE, 
                                         drawError = list(color = "#b00b00", timeout = 2500), 
                                         guidelineDistance = 20, metric = TRUE, feet = FALSE, zIndexOffset = 2000, 
                                         shapeOptions = drawShapeOptions(fill = FALSE), repeatMode = FALSE) {
        leaflet::filterNULL(list(allowIntersection = allowIntersection, 
                                 drawError = drawError, guidelineDistance = guidelineDistance, 
                                 metric = metric, feet = feet, zIndexOffset = zIndexOffset,
                                 shapeOptions = shapeOptions,  repeatMode = repeatMode)) }
      
    output$gauging_station <- renderLeaflet({
      # Create leaflet map
      leaflet(req(a), options = leafletOptions(attributionControl = FALSE)) %>%
        addTiles() %>%
        addDrawToolbar(
          polylineOptions = mydrawPolylineOptions(metric=TRUE, feet=FALSE),
          editOptions=editToolbarOptions(selectedPathOptions=selectedPathOptions())
        )%>%
        addMarkers(
          lng = ~ longitude,
          lat = ~ latitude,
          icon = icon('map-pin'),
          popup = ~ as.character(paste("ID       =", ID, "<br>",
                                       "Station =", Station.en, "<br>",
                                       "Country  =", Country, "<br>",
                                       "Region =", Region, "<br>",
                                       "LAT      = ", latitude, "<br>",
                                       "LON      = ", longitude, "<br>",
                                       "START    = ", Start, "<br>",
                                       'END      = ', End,"<br>")),
          clusterOptions = markerClusterOptions()  # Enable clustering for better performance
        )
    })
    
    output$request_count <- renderUI({
      h3(style = "color: red;", "Please Calculate Distance first!")
    })
    
    
    observeEvent(input$distance_gauging_weather, {
      update_modal_text("Computing nearest distance and search longest historical data ...")
      
      con <- dbConnect(RSQLite::SQLite(), "database/data.sqlite")
      on.exit(dbDisconnect(con))
      tide_stations <- dbReadTable(con, "sort_tide_station")
      # tide_stations <- read.csv("sort_tide_station.csv")[, -1]
      names(tide_stations) <- c("ID", "GLOSS", "Location", "Country", "latitude", "longitude", "Start", "End")
      
     
      climate_stations <- a
      
      # Ensure latitude and longitude are numeric in both data frames
      tide_stations$latitude <- as.numeric(tide_stations$latitude)
      tide_stations$longitude <- as.numeric(tide_stations$longitude)
      climate_stations$latitude <- as.numeric(climate_stations$latitude)
      climate_stations$longitude <- as.numeric(climate_stations$longitude)
      
      # Find the nearest climate stations
      #nearest_stations <- find_nearest_stations(tide_stations, climate_stations, req(input$buffer))
      results <- find_nearest_stations(tide_stations, climate_stations, req(input$buffer))
      
      nearest_stations <-results$all_longest_stations  #within buffer
      
      tide_outside_buffer <- results$tide_outside_buffer #outsize buffer
      
      dbWriteTable(con, "list_nearby_station_pr_wl", nearest_stations, overwrite = TRUE)
      on.exit(dbDisconnect(con))
      
      
      # write.csv(nearest_stations, "nearby_station2.csv")
      updateSelectInput(session, "distance_gauging_weather", paste("Distance ", nrow(nearest_stations), " station") )
      
      output$weather_global_data<- renderTable({
        tide_outside_buffer
      })
      
      output$table_title <- renderUI({
        if (nrow(tide_outside_buffer) > 0) {
          tags$h3(paste("There are",nrow(tide_outside_buffer), "stations Outside Buffer"))
        } else {
          tags$h3("Data Availability")
        }
      })
      
 
    
      output$gauging_station <- renderLeaflet({
       
        leaflet(nearest_stations, options = leafletOptions(attributionControl = FALSE)) %>%
          addTiles() %>%
        addDrawToolbar(
            polylineOptions = mydrawPolylineOptions(metric=TRUE, feet=FALSE),
            editOptions=editToolbarOptions(selectedPathOptions=selectedPathOptions())
          ) %>%
          addCircles(
            lng = ~ Climate_Longitude,
            lat = ~ Climate_Latitude,
            radius = req(input$buffer)*1000,  # Radius in meters (15 km)
            color = "red",  
            fillOpacity = 0.2, 
            weight = 1, 
            popup = ~ as.character(paste("Radius: ", req(input$buffer)," km"))  
          ) %>%
          addCircleMarkers(
            lng = ~ Climate_Longitude,
            lat = ~ Climate_Latitude,
            color = "red",
            popup = ~ as.character(paste("ID       =", Climate_ID, "<br>",
                                         "Station =", Climate_Station, "<br>",
                                         "Country  =", Climate_Country, "<br>",
                                         "Region =", Climate_Region, "<br>",
                                         "LAT      = ", Climate_Latitude, "<br>",
                                         "LON      = ", Climate_Longitude, "<br>",
                                         "START    = ", Start, "<br>",
                                         'END      = ', End,"<br>",
                                         'LENGTH DAY      = ', Dataset_Length," Days <br>",
                                         'SAME WL STATION      = ', Tide_Station," <br>",
                                         'SAME WL ID      = ', Tide_ID," <br>"
                                         ))) 
      })
      
      
      removeModal()
      
  
    # Download weather data for each nearest station
      observeEvent(input$download_gauging_weather, {
        # Initialize total segments
        total_segments <- 0
        
        # Check if nearest_stations is available
        if (!is.null(nearest_stations) && nrow(nearest_stations) > 0) {
          # Calculate segments for each nearest station
          for (i in 1:nrow(nearest_stations)) {
            station_id <- nearest_stations$Climate_ID[i]
            start_date <- nearest_stations$Start[i]
            end_date <- nearest_stations$End[i]
            
            num_segments <- calculate_segments(start_date, end_date)
            total_segments <- total_segments + num_segments
          }
          
          # Update the UI output with the total number of requests
          output$request_count <- renderUI({
            h3(paste("There are a total of", total_segments, "API requests"))
          })
        } else {
          # Reset to default message if no nearest stations found
          output$request_count <- renderUI({
            h3("Please Calculate Distance first!")
          })
        }
      })
      
      
      observeEvent(input$download_gauging_weather_submit, {
        all_weather_data <- list()
        
  
        
         tryCatch({
           for (i in 112:nrow(nearest_stations)) {
            update_modal_text(paste("Downloading at ", nearest_stations$Climate_ID[i],"-" , nearest_stations$Climate_Station[i],"-", nearest_stations$Climate_Country[i], "..."))
            
            station_id <- nearest_stations$Climate_ID[i]
            start_date <- as.Date(nearest_stations$Start[i], "%Y-%m-%d")
            end_date <- as.Date(nearest_stations$End[i], "%Y-%m-%d")
            
            # Call the API function
            weather_data <- api_weather_loop(station_id, start_date, end_date)
            
            # Check if data is not empty
            if (!is.null(weather_data) && nrow(weather_data) > 0) {
              all_weather_data[[station_id]] <- weather_data  # Use station ID as the name
              
              # Save to CSV
              # write.csv(weather_data, file = paste0("database/weather_station_", station_id, ".csv"), row.names = FALSE)
              
              # Save to SQLite database
              con <- dbConnect(RSQLite::SQLite(), "database/data.sqlite")
              dbWriteTable(con, name = paste0("station_", station_id), value = weather_data, append = TRUE, row.names = FALSE)
               } else {
              update_modal_text(paste("No data returned for station ID =", station_id))
            }
          }
          
          # Optionally, bind all data together 
          final_weather_data <- do.call(rbind, all_weather_data)
          
          update_modal_text("All weather data downloaded successfully.")
          
          removeModal()
        }, error = function(e) {
          showModal( modalDialog(  "Download Error! Probably exceeded API request limits.", footer = tagList(actionButton("closeErrorModal", "Close", class = "btn-default") )))

        }, finally = {
          dbDisconnect(con)

        })
      })
     
     
    
    })
    })
    
    
    
    
    
  #--------------------------------------------------------------------------------------------
  #===============================  COPULAS MODEL =============================================

  # ++++++++++++++++++++++++++++++++ PROCESSING ++++++++++++++++++++++++++++++++++++++++++++++  
    observe({
      
   
      
      observeEvent(input$remove_station_submit, {
      con <- dbConnect(RSQLite::SQLite(), "database/data.sqlite")
      on.exit(dbDisconnect(con)) 
      tide_ids_to_remove <- req(input$remove_station)
      tide_ids_string <- paste(tide_ids_to_remove, collapse = ", ")
      delete_query <- paste("DELETE FROM list_nearby_station_pr_wl WHERE Tide_ID IN (", tide_ids_string, ")", sep = "")
      dbExecute(con, delete_query)
      showModal( modalDialog(  "Stations have been removed from Database", footer = tagList(actionButton("closeErrorModal", "Close", class = "btn-default") )))
      })
      
      
      
       observeEvent(input$process_data, {
   
      if (req(input$automate_switch) == "Automate"){
        update_modal_text("Automate approach executes data from SQL database ...")
      merged_data <- merging_data_automate(merge_pair_automate)
      } else  {
         update_modal_text("Manual approach executes CSV data from local ...")
      merged_data <- merging_data_mannual(merge_pair_mannual)
      }
       
     merged_data$Charlottetown <- cha_data  
     
      updateSelectizeInput(session,  "station_process", choices =  rbind("Charlottetown",names(merged_data)) )
      
 
      
      
      
    # TRIM
    adjusted_data <- reactiveVal(list())
      
    observeEvent(input$trim_date_submit,{
      
      station_name <- as.character(req(input$station_process))
      
      adjusted_data <- merged_data[[station_name]] %>%
        filter(Date >= input$trim_date[1] & Date <= input$trim_date[2])
      
    
      if (input$adjust_data_variable == "Tide") {
        adjusted_data <- adjusted_data %>%
          mutate(Tide = ifelse(Date >= input$adjust_date[1] & Date <= input$adjust_date[2],
                               Tide + input$adjust_threshold, Tide))
      } else {
        adjusted_data <- adjusted_data %>%
          mutate(Rain = ifelse(Date >= input$adjust_date[1] & Date <= input$adjust_date[2],
                               Rain + input$adjust_threshold, Rain))
      }
      
      current_adjusted <- adjusted_data()
      current_adjusted[[station_name]] <- adjusted_data 
      adjusted_data(current_adjusted)
      
      output$process_plot <- renderPlotly({
        p<-   plot_ly(adjusted_data, x = ~Date) %>%
          add_lines(y = ~Rain, name = 'Rain', line = list(color = req(input$colopickup1)), yaxis = "y1") %>%
          add_lines(y = ~Tide, name = 'Tide', line = list(color = req(input$colopickup2)), yaxis = "y2") %>%
          layout(
            title = list(text = paste("Daily Precipitation and Water Level at", input$station_process,"station")),
            yaxis = list(title = "Rain", side = "left", overlaying = "y2", showline = TRUE, tickfont = list(color = req(input$colopickup1))),
            yaxis2 = list(title = "Tide", side = "right", showline = TRUE, tickfont = list(color = req(input$colopickup2))),
            xaxis = list(title = ""),
            legend = list(orientation = 'h', xanchor = "center", x = 0.5, y = -0.1)
          )
        plotly::ggplotly(p) %>%
          config(displaylogo = F)
      })
    })
      
      
    observeEvent(input$rewrite_submit_original, {
        station_name <- as.character(req(input$station_process))
        con <- dbConnect(RSQLite::SQLite(), "database/data.sqlite")
        on.exit(dbDisconnect(con)) 
        # For Tide
        current_adjusted_data <- adjusted_data()[[station_name]]  # Access the reactive list
        current_adjusted_data$Date <- as.character(current_adjusted_data$Date)
        
        # adjusted_data_list <- adjusted_data()  # Get the current adjusted data
        # adjusted_data_list[[station_name]]$Date <- as.character(adjusted_data_list[[station_name]]$Date)
        
        # adjusted_data$Date <-  as.character(adjusted_data$Date)
        dbWriteTable(con, name = paste0("Global_Tide_", station_name, "_adjusted"), value = current_adjusted_data, overwrite = TRUE, row.names = FALSE)
        

        update_modal_text("A new version of data has been added to database !")
  
        removeModal()
        
      
      })
        

 
    
    con <- dbConnect(RSQLite::SQLite(), "database/data.sqlite")
    on.exit(dbDisconnect(con))
    list_database <- dbListTables(con)
    tide_station_tables <- grep("^Global_Tide_", list_database, value = TRUE)
    
    
    merged_data <- get_station_with_adjusted_data(merged_data, gsub("^Global_Tide_", "", tide_station_tables ))
    # RESET
    
    observeEvent(input$reset_adjust,{
      # merged_data[[as.character(req(input$station_process))]] <-merged_data[[as.character(req(input$station_process))]] 
      output$process_plot <- renderPlotly({
        p<-   plot_ly(merged_data[[as.character(req(input$station_process))]], x = ~Date) %>%
          add_lines(y = ~Rain, name = 'Rain', line = list(color = req(input$colopickup1)), yaxis = "y1") %>%
          add_lines(y = ~Tide, name = 'Tide', line = list(color = req(input$colopickup2)), yaxis = "y2") %>%
          layout(
            title = list(text = paste("Daily Precipitation and Water Level at", input$station_process,"station")),
            yaxis = list(title = "Rain", side = "left", overlaying = "y2", showline = TRUE, tickfont = list(color = req(input$colopickup1))),
            yaxis2 = list(title = "Tide", side = "right", showline = TRUE, tickfont = list(color = req(input$colopickup2))),
            xaxis = list(title = ""),
            legend = list(orientation = 'h', xanchor = "center", x = 0.5, y = -0.1)
          ) 
        plotly::ggplotly(p) %>%
          config(displaylogo = F) 
      })
      
    })
    
    
  # Display daily rain and tide data
   
   
     
   output$global_pair_data <- renderDT({
     merged_data[[as.character(req(input$station_process))]]
   })
  
  output$process_plot <- renderPlotly({
   p<-   plot_ly(merged_data[[as.character(req(input$station_process))]], x = ~Date) %>%
      add_lines(y = ~Rain, name = 'Rain', line = list(color = req(input$colopickup1)), yaxis = "y1") %>%
      add_lines(y = ~Tide, name = 'Tide', line = list(color = req(input$colopickup2)), yaxis = "y2") %>%
      layout(
        title = list(text = paste("Daily Precipitation and Water Level at", input$station_process,"station")),
        yaxis = list(title = "Rain", side = "left", overlaying = "y2", showline = TRUE, tickfont = list(color = req(input$colopickup1))),
        yaxis2 = list(title = "Tide", side = "right", showline = TRUE, tickfont = list(color = req(input$colopickup2))),
        xaxis = list(title = ""),
        legend = list(orientation = 'h', xanchor = "center", x = 0.5, y = -0.1)
      )
   plotly::ggplotly(p) %>%
     config(displaylogo = F)
  })
                                            
     # Display their relationship 
     output$process_plot_cor<-   renderLeaflet({
       station<- filter(tide_station, UH. == req(input$station_process))
       leaflet(station, options = leafletOptions(attributionControl = FALSE)) %>%
         addTiles() %>%
         addMarkers(
           lng = ~ Longitude,
           lat = ~ Latitude,
           popup = ~ as.character(paste("ID       =", UH., "<br>",
                                        "Station =", Location, "<br>",
                                        "Country  =", Country, "<br>",
                                        "LAT      = ", Latitude, "<br>",
                                        "LON      = ", Longitude, "<br>"
           ))) 
     })
 
     
    
     #Display message
      output$message <- renderText({
        paste(
          "Data contains daily Precipitation and Tide for station ID = ",
          "<strong>",
          req(input$station_process),
          "</strong>")
      })
      
     
    removeModal()
 
 
 
 # ++++++++++++++++++++++++++++++++ NORMALISATION  ++++++++++++++++++++++++++++++++++++++++++++++ 
  
   # Perform normalisation
    # observeEvent(input$distance_based, {
       tryCatch({
         update_modal_text("Analyzing with Distance-based Approach ...")
        # Show messages

       
       # Performe quantile normalisation and calculate Tau value for each dataset  
       data_with_distance_global <- list()
       kendall_value_global <- list()
       tau_value <- data.frame(Station = character(), Tau = numeric(), p_value = numeric(), Latitude = numeric(), Longitude = numeric(), stringsAsFactors = FALSE)
       
      
         
       
       for (j in names(merged_data)) {
         data_with_distance_global[j] <- list(distance_quantile_normalization(merged_data[[j]], names(merged_data)[j]))
        
         # Extract Rain and Tide data
         rain_data <- as.numeric(data_with_distance_global[[j]]$Rain)
         tide_data <- as.numeric(data_with_distance_global[[j]]$Tide)
         
         # Check if the data is numeric and has sufficient values
         if (is.null(rain_data) || is.null(tide_data) || length(na.omit(rain_data)) < 2 || length(na.omit(tide_data)) < 2) {
           message(paste("Skipping station:", j, "- insufficient numeric data for Kendall test."))
           next  # Skip to the next station
         }
         # Perform Kendall test
         kendall_value_global[[j]] <- suppressWarnings(cor.test(rain_data, tide_data, method = "kendall"))
         
         tau_values <- kendall_value_global[[j]]$estimate
         p_values  <- kendall_value_global[[j]]$p.value
         filtered_station <- filter(tide_station, UH. == j)
         
         # If there's a match in the tide_station, extract lat and lon
         if (nrow(filtered_station) > 0) {
           lat <- filtered_station$Latitude
           lon <- filtered_station$Longitude
         } else {
           lat <- NA
           lon <- NA
         }
         
         
         tau_value <- rbind(tau_value, data.frame(Station = j, Tau = tau_values, p_value =p_values,  Latitude = lat, Longitude = lon, stringsAsFactors = FALSE))
         
        
       } 
       
       list(tau_value = tau_value, data_with_distance_global = data_with_distance_global)
      
       
      # Display Normalised data in Annual
      output$plot_distance <- renderPlotly({
        p<- plot_ly(data_with_distance_global[[as.character(req(input$station_process))]], x = ~Date) %>%
          add_lines(y = ~Rain, name = 'Rain', line = list(color = req(input$colopickup1)), yaxis = "y1") %>%
          add_lines(y = ~Tide, name = 'Tide', line = list(color = req(input$colopickup2)), yaxis = "y2") %>%
          layout(
            title = list(text = paste("Annual Precipitation and Tide at", req(input$station_process),"station")),
            yaxis = list(title = "Rain", side = "left", overlaying = "y2", showline = TRUE, tickfont = list(color = req(input$colopickup1))),
            yaxis2 = list(title = "Tide", side = "right", showline = TRUE, tickfont = list(color = req(input$colopickup2))),
            xaxis = list(title = ""),
            legend = list(orientation = 'h', xanchor = "center", x = 0.5, y = -0.1)
          )
        plotly::ggplotly(p) %>%
          config(displaylogo = F) 
       })
      
      # Display Kendall Correlation based on Tau and Pvalue
      output$normalise_plot_cor<- renderPlot({
       kendall_test(data_with_distance_global[[as.character(req(input$station_process))]]$Date, data_with_distance_global[[as.character(req(input$station_process))]]$Rain,data_with_distance_global[[as.character(req(input$station_process))]]$Tide, 0.05, 
                  c("pearson", "kendall", "spearman")[2], (input$station_process))
       })
      
      
      tau_bins <- seq(-1.0, 1.0, by = 0.2)  # 10 bin boundaries, defining 9 intervals
      tau_colors <- c( "#1147DB", "#0D6FA8", "#0FC298", "#15A321", "#C2CC0A", "#FF9C08", "#F0095A", "red", "#700B0B", "#330B0B")  # 9 colors
      tau_labels <- c("-1.0 - -0.8", "-0.8 - -0.6", "-0.6 - -0.4", "-0.4 - -0.2", "-0.2 - 0.0", 
                      "0.0 - 0.2", "0.2 - 0.4", "0.4 - 0.6", "0.6 - 0.8", "0.8 - 1.0")
      
      # Display global Tau value on map
      getColor <- function(tau) {
        if (tau <= -0.8) {
          tau_colors[1]
        } else if (tau <= -0.6) {
          tau_colors[2]
        } else if (tau <= -0.4) {
          tau_colors[3]
        } else if (tau <= -0.2) {
          tau_colors[4]
        } else if (tau <= 0.0) {
          tau_colors[5]
        } else if (tau <= 0.2) {
          tau_colors[6]
        } else if (tau <= 0.4) {
          tau_colors[7]
        } else if (tau <= 0.6) {
          tau_colors[8]
        } else if (tau <= 0.8) {
          tau_colors[9]
        } else {
          tau_colors[10]
        }
      }
      getShape <- function(p_value) {
        if (p_value < 0.005) {
          "Significance"
        } else {
          "Insignificance"
        }
      }
      
      
      
      output$normalise_global <- renderLeaflet({

        tau_value_filtered <- tau_value %>%
          filter(!is.na(Latitude) & !is.na(Longitude))
        
        leaflet(data = tau_value_filtered, options = leafletOptions(attributionControl = FALSE)) %>%
          addTiles(group = "OpenStreetMap") %>%  
         
          addCircleMarkers(
            ~Longitude, ~Latitude,
            color = ~sapply(Tau, getColor),
            radius = 8, fillOpacity = 0.5,
            label = ~sapply(p_value, getShape), 
            popup = ~paste( "Station:", Station, "<br>",  "Tau:", round(Tau, 3), "<br>",  "P-value:", format.pval(p_value)
            )
          )%>%
          addLegend(
            position = "topright",
            colors = tau_colors,
            labels = tau_labels,
            title = "Tau Value",
            opacity = 0.7
          )
       })
     
      
     }, error = function(e) {
       showModal( modalDialog("Please perform PRE-PROCESSING data first before NORMALISATION", footer = tagList(actionButton("closeErrorModal", "Close", class = "btn-default") )))
       return(NULL)
     })
   
     
   # }) #Normalisation
  
    removeModal()
   
   # ++++++++++++++++++++++++++++++++ COPULA ANALYSIS  +++++++++++++++++++++++++++++++++++++++++++
   


   observeEvent(input$copula_analysis, {
   
     update_modal_text("Performing Copula Analysis ...")
     # tryCatch({
     
     station_name <- as.character(req(input$station_process))
     error_stations <- c()
     
     rain_data <- data_with_distance_global[[station_name]]$Rain
     tide_data <- data_with_distance_global[[station_name]]$Tide
     
     # Check if both rain_data and tide_data have valid entries
     if (length(rain_data) < 2 || length(tide_data) < 2) {
       warning(paste("Station", station_name, "has insufficient data for Rain and Tide. Skipping this station."))
       error_stations <- c(error_stations, station_name)  # Log the station with an error
     } else {
       # Create data frame
       data <- data.frame(P = rain_data, H = tide_data)
           
           # Selected copula families
           copula_families <- list(
             Gaussian = copula::normalCopula(),
             Clayton = copula::claytonCopula(),
             Frank = copula::frankCopula(),
             Gumbel = copula::gumbelCopula(),
             Joe = copula::joeCopula()
           )
           
           # Function to calculate copula parameters   
           copula_parameters <- list()
           
           for (i in seq_along(copula_families)) {
             copula_name <- names(copula_families)[i]
             copula_function <- copula_families[[i]]
             
             # Calculate parameters
             copula_parameters[[copula_name]] <- parameter(copula_function, data)
           }
           
           # Apply the above parameters back to copula models
           copula_families <- list(
             Gaussian = copula::normalCopula(param = copula_parameters$Gaussian, dim = 2),
             Clayton = copula::claytonCopula(param = copula_parameters$Clayton, dim = 2),
             Frank = copula::frankCopula(param = copula_parameters$Frank, dim = 2),
             Gumbel = copula::gumbelCopula(param = copula_parameters$Gumbel, dim = 2),
             Joe = copula::joeCopula(param = copula_parameters$Joe, dim = 2)
           )
           
       
     }
     
      # List of marginal distributions
      distributions_to_test <- c("norm", "gamma", "weibull", "lnorm", "exp", "gev",  "logistic", "cauchy")
      
      # Perform Copula model based on GEV methods for rain and tide
      copula_models <- lapply(copula_families, fit_and_summarize_copula, data = data, fitRain = "gev", fitTide = "gev", "ml")

      # Calculate likelihoods
      model_likelihoods <- data.frame(Model = names(copula_families),
                                      LogLikelihood = sapply(names(copula_families), function(model_name) {
                                        copula_models[[model_name]]@loglik # Use @ to access the slot in S4 object
                                      }))
      
      # Calculation AIC and BIC
      aic_bic_values <- lapply(copula_models, calculate_aic_bic, data = data)
      names(aic_bic_values) <- names(copula_families)
      
      aic_bic_df <- do.call(rbind, lapply(names(as.list(aic_bic_values)), function(copula) {
        metrics <- aic_bic_values[[copula]]
        data.frame(Copula = copula, AIC = metrics$AIC, BIC = metrics$BIC, stringsAsFactors = FALSE)
      }))
      
      aic_values <- sapply(aic_bic_values, function(x) x["AIC"])
      bic_values <- sapply(aic_bic_values, function(x) x["BIC"])
  
          
      # Function `ks_test_distributions` is used to test significance of Rain and Tide
      rain_ks_results <- ks_test_distributions(data$P, distributions_to_test, 0.01) 
      tide_ks_results <- ks_test_distributions(data$H, distributions_to_test, 0.01) 
      
      # Display result of KS for rain
      output$copula_ks_rain <- renderTable({
        rain_ks_results
      })
      
      # Display result of KS for tide
      output$copula_ks_tide <- renderTable({
        tide_ks_results
      })
     
      # Display result of CDF for rain
      output$cdf_p<- renderPlot({
        plot_cdf_marginal(data$P, "Precipitation")
      })
      
      # Display result of CDF for tide
      output$cdf_h<- renderPlot({
        plot_cdf_marginal(data$H, "Tide")
      })
      
     
      # Plot Likelihoods 
      output$fitting <- renderPlotly({
        ggplot(model_likelihoods, aes(x = Model, y = LogLikelihood, fill = Model)) +
          geom_bar(stat = "identity") +
          theme_minimal() +
          labs(title = "Log Likelihood by Copula Model", x = "Copula Model", y = "Log Likelihood") +
          scale_fill_brewer(palette = "Set3") # Optional color palette
      })
      
      
         
     # Plot AIC and BIC 
      output$aic_bic <- renderPlot({
        aic_bic_long <- aic_bic_df %>%
          tidyr::pivot_longer(cols = c(AIC, BIC), names_to = "Criteria", values_to = "Value") %>%
          mutate(Ring = ifelse(Criteria == "AIC", "Inner", "Outer"))
        
      # Add label positions
      aic_bic_long <- aic_bic_long %>%
          group_by(Criteria) %>%
          mutate(Position = cumsum(abs(Value)) - 0.5 * abs(Value))  # Calculate label positions)
                 
                 # Plot donut chart with values
                 ggplot(aic_bic_long, aes( x = Ring,  y = abs(Value), fill = Copula )) +
                   geom_bar(stat = "identity", position = "stack", width = 1) +
                   coord_polar(theta = "y") +
                   theme_void() +
                   labs(title = "Circular Donut Chart: AIC (Inner) and BIC (Outer)") +
                   scale_fill_brewer(palette = "Set3") +
                   
                   # Add text labels for values
                   geom_text(aes(y = Position, label = round(Value, 2)),  color = "black", size = 4) +
                   
                   theme(legend.title = element_blank()) # Remove legend title
      })
      
      # Display message  
      output$message <- renderUI({
        withMathJax(HTML(
          paste(
            "Smallest AIC is",
            round(min(unlist(aic_values)), 3),
            "for the copula model:",
            "<strong>",
            names(which.min(unlist(aic_values))),
            "</strong>",
            "<br>",
            "Smallest BIC is",
            round(min(unlist(bic_values)), 3),
            "for the copula model:",
            "<strong>",
            names(which.min(unlist(bic_values))),
            "</strong>",
            "<br>"
          )
        ))
      })
      
   
      removeModal()
       
   

 # ++++++++++++++++++++++++++++++++ JOIN RETURN PERIOD  +++++++++++++++++++++++++++++++++++++++++++
      
    # Update selected return period
    updateSelectizeInput(session, "rp_select", choices =  req(input$rp)) 
   
     
    # Perform return period  
     observeEvent(input$rp_submit, {
       
       update_modal_text("Analyzing Joint Return Period ...")
       
       joint_return_periods <- lapply(copula_models, calculate_joint_return_period, req(as.numeric(input$rp)), data)
       
       # Calculate joinr return periods 
       all_joinRP <- list()
       for (rp in req(as.numeric(input$rp))) {
         for (copula_name in names(joint_return_periods)) {
           copula_model <- copula_models[[copula_name]]
           joinRP <- data.frame(
             model = copula_name,
             return_period = rp,
             AND = joint_return_periods[[copula_name]][[as.character(rp)]]$AND,
             OR = joint_return_periods[[copula_name]][[as.character(rp)]]$OR
           )
           all_joinRP[[paste(copula_name, rp, sep = "_")]] <- joinRP
         }
       }
       
       # Joint return periods are stored in this 
       final_joinRP <- do.call(rbind, all_joinRP)
       names(final_joinRP)<- c("Model", "RP", "AND","AND_Prob", "OR", "OR_Prob")
       
      
       # Display result on table
       output$rp_table<- renderTable({
         filter(final_joinRP, Model == req(input$copula_model))
       })
       
       # Filter the results based on Model and RP inputs
       join_point <- filter(final_joinRP, Model == req(input$copula_model) & RP == req(input$rp_select))
       
       # Create surface 3D plot
       tide_max <- max(data$H)
       rainfall_max <- max(data$P)
       
       x_seq <- seq(0, rainfall_max , length.out = 100)
       y_seq <- seq(0, tide_max , length.out = 100)
       
       u_seq <- 1 - x_seq / rainfall_max
       v_seq <- 1 - y_seq / tide_max
       
       uv_grid <- expand.grid(u_seq, v_seq)
       colnames(uv_grid) <- c("u", "v")
       
       uv_grid$u <- pmin(pmax(uv_grid$u, 0.001), 0.999)
       uv_grid$v <- pmin(pmax(uv_grid$v, 0.001), 0.999)
       
       z <- matrix(copula::pCopula(as.matrix(uv_grid), copula_model@copula), nrow = length(u_seq), ncol = length(v_seq))
      
       # Display 3D plot
      output$plot_rp <- renderPlotly({
        fig <- plotly::plot_ly(x = x_seq, y = y_seq, z = z) %>%
          add_surface() %>%
          layout(title = paste(req(input$copula_model), "Joint Probability Return Period:", req(input$rp_select), "years"),
                 scene = list(xaxis = list(title = 'Precipitation (mm)'),
                              yaxis = list(title = 'Tide Level (m)'),
                              zaxis = list(title = 'Joint Probability'))
          ) %>%
          add_markers(x = join_point$AND[1],
                      y = join_point$AND[2],
                      z = join_point$AND_Prob/100,
                      marker = list(color = c('violetred2'), size = 7),
                      name = "AND",
                      text = paste("P = ", round(join_point$AND[1],2), "mm<br>H = ", round(join_point$AND[2],2), "m<br>Prob = ", round(join_point$AND_Prob[1], 3),"%"),
                      textfont = list(size = 12, color = 'black')) %>%
          add_markers(x = join_point$OR[1],
                      y = join_point$OR[2],
                      z = join_point$OR_Prob/100,
                      marker = list(color = c('blue'), size = 7),
                      name = "OR",
                      text = paste("P = ", round(join_point$OR[1],2), "mm<br>H = ", round(join_point$OR[2],2), "m<br>Prob = ", round(join_point$OR_Prob[2], 3), "%"),
                      textfont = list(size = 12, color = 'black'))
        plotly::ggplotly(fig) %>%
          config(displaylogo = F) 
        })
  
      removeModal()
      }) # Submit RP
  
     
     # This is to display contour plot
     observeEvent(input$contour_plot, {
       output$plot_rp_test <- renderPlot({
         showModal(modalDialog(img(src = 'https://loading.io/assets/mod/spinner/spinner/sample.gif'), "Plotting contour ..." ,footer = NULL ))
         contour_rp_plot(data, copula_families[[req(as.character(input$copula_model))]], req(input$copula_model), req(input$rp))
         removeModal()
       })
     })
     
     # }, error = function(e) {
     #   showNotification("Tau value for some Copula model is too small", type = "error")
     # })
     
     
   }) # Submit
      
 }) #processing data  
   
    #IDF Table
    output$table_idf_rp <- renderTable({
        table2a
    })
   

    removeModal()
    

    # =================VIEW MAP =================================
    
    
  
    
    # all_joinRP_stations <- list()
    # 
    # # Loop through each station
    # for (station_name in station_all$UH.) {
    #   
    #   # Data for selected station  
    #   data<- data.frame(P= data_with_distance_global[[as.character(station_name)]]$Rain, H =data_with_distance_global[[as.character(station_name)]]$Tide)
    #   copula_models <- lapply(copula_families, fit_and_summarize_copula, data = data, fitRain = "gev", fitTide = "gev", "ml")
    #   # Calculate joint return periods for this station for each copula model
    #   joint_return_periods <- lapply(copula_models, calculate_joint_return_period, req(as.numeric(input$rp)), data)
    #   
    #   # Store the return period results for each copula and return period
    #   all_joinRP <- list()
    #   for (rp in req(as.numeric(input$rp_view))) {
    #     for (copula_name in names(joint_return_periods)) {
    #       joinRP <- data.frame(
    #         station = station_name,
    #         model = copula_name,
    #         return_period = rp,
    #         AND_Prob = joint_return_periods[[copula_name]][[as.character(rp)]]$AND,
    #         OR_Prob = joint_return_periods[[copula_name]][[as.character(rp)]]$OR
    #       )
    #       all_joinRP[[paste(copula_name, rp, station_name, sep = "_")]] <- joinRP
    #     }
    #   }
    #   
    # }
    # # Combine all stations' results into a single data frame
    # final_joinRP_stations <- merge(final_joinRP_stations, station_all[, c("UH.", "Latitude", "Longitude")], by.x = "Station", by.y = "UH.", all.x = TRUE)
    # names(final_joinRP_stations) <- c("Station", "Model", "RP", "AND_Prob", "OR_Prob")
    # 
    # 
    # output$view_map <- renderLeaflet({
    #   req(input$copula_model_view)
    #   req(input$rp_view)
    #   req(input$variable_view)  # 'Rain' or 'Tide'
    # 
    #   # Filter the data based on user selection
    #   filtered_data <- final_joinRP_stations %>%
    #     filter(Model == input$copula_model_view & RP == input$rp_view)
    # 
    #   # Define the variable to be displayed on the map (either AND or OR)
    #   if (input$variable_view == "Rain") {
    #     filtered_data$display_value <- filtered_data$AND_Prob
    #   } else {
    #     filtered_data$display_value <- filtered_data$OR_Prob
    #   }
    # 
    #   # Create the Leaflet map
    #   leaflet(data = filtered_data) %>%
    #     addTiles() %>%
    #     addCircleMarkers(
    #       lng = ~Longitude, lat = ~Latitude,
    #       color = ~colorQuantile("YlOrRd", display_value)(display_value),  # Use a color scale for probabilities
    #       radius = 5,
    #       popup = ~paste("Station:", Station, "<br>",
    #                      "Model:", Model, "<br>",
    #                      "Return Period:", RP, "<br>",
    #                      "AND Probability:", AND_Prob, "<br>",
    #                      "OR Probability:", OR_Prob)
    #     ) %>%
    #     addLegend("bottomright", pal = colorQuantile("YlOrRd", filtered_data$display_value), values = filtered_data$display_value, title = input$variable_view)
    # })

    
    
}) # Observe
  
} 
