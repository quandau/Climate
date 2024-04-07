library(shinydashboard)
library(shiny)
library(leaflet)
library(tidyhydat)
library(weathercan)
library(plotly)


ui <- dashboardPage(
  dashboardHeader(title = "Climate Portal"),
  dashboardSidebar(sidebarMenu(
    id = "climate",
    img(src = 'https://i0.wp.com/climatesmartlab.upei.ca/files/2022/04/cropped-cls_b.png?fit=200%2C70&ssl=1'),
    br(),
    menuItem(
      "Meteological",
      tabName = "met",
      icon = icon("cloud-sun-rain")
    ),
    menuItem(
      "Hydrometric",
      tabName = "hyd",
      icon = icon("house-tsunami")
    )
  )),
  dashboardBody(tabItems(
    tabItem(
      tabName = "met",
      fluidRow(column(
        3,
        box(
          title = "Search Data",
          width = 12,
          status = "primary",
          solidHeader = TRUE,
          
          textInput("search_station", "Search Station"),
          radioButtons(
            "interval",
            "Interval",
            choices = c("hour", "day", "month"),
            inline = TRUE
          ),
          selectizeInput("station", "Station", choices = NULL),
          dateRangeInput("daterange2", "Date range:"),
          actionButton("valid", "Validate", class = "btn-primary btn-lg"),
          downloadButton("down", "Download")
        )
      ),
      box(
        width = 9,
        column(6,
               
               tableOutput("data_table")),
        column(6,
               
               leafletOutput("map"))
      )),
      box(
        width = 12,
        tableOutput("select_table"),
        textOutput("prov"),
        fluidRow(column(6, plotlyOutput("prec_map")),
                 column(6, plotlyOutput("temp_map")))
      ),
      
    ),
    tabItem(tabName = "hyd",
            
            fluidRow(
              column(
                3,
                box(
                  title = "Search Data",
                  width = 12,
                  status = "primary",
                  solidHeader = TRUE,
                  
                  selectizeInput(
                    "province",
                    "Province",
                    choices = c(
                      "AB",
                      "BC",
                      "MB",
                      "NB",
                      "NL",
                      "NT",
                      "NS",
                      "NU",
                      "ON",
                      "PE",
                      "QC",
                      "SK",
                      "YT"
                    ),
                    selected = "PE"
                  ),
                  selectizeInput("streamstation", "Station", choices = NULL),
                  selectizeInput("param", "Parameter", choices = NULL),
                  dateRangeInput("daterange_stream", "Date range:"),
                  actionButton("valid_stream", "Validate", class = "btn-primary btn-lg"),
                  downloadButton("down_stream", "Download")
                )
              ),
              box(
                width = 9,
                column(width = 6,
                       tableOutput("station_table")),
                column(width = 6,
                       leafletOutput("map_stream"))
              )
              
            ),
            
            fluidRow(
              box(
                width = 12,
                status = "primary",
                column(
                  4,
                  tableOutput("select_table_stream"),
                  textOutput("prov_stream")
                ),
                column(8,
                       plotlyOutput("stream_map"))
              )
            ))
  ))
)


server <- function(input, output, session) {
    observe({
      # updateSelectizeInput(session, "prov", choices = stations_search(input$search_station, interval = input$interval)[1])
      updateSelectizeInput(session,
                           "station",
                           choices =  stations_search(input$search_station, interval = input$interval)[3])
      
    })
    
    
    output$data_table <- renderTable({
      head(stations_search(input$search_station, interval = input$interval))[, c(1:4,  7, 8, 11)]
    })
    
    output$map <- renderLeaflet({
      out <-
        head(stations_search(input$search_station, interval = input$interval))
      if (is.null(out) || nrow(out) == 0) {
        return(NULL)
      }
      leaflet(out, options = leafletOptions(attributionControl = FALSE)) %>% addTiles() %>%
        addCircleMarkers(
          lng = ~ lon,
          lat = ~ lat,
          popup = ~ as.character(paste("Station ID: ", station_id))
          
        )
    })
    
    observe({
      observeEvent(input$valid, {
        showModal(modalDialog(
          "Patience is the key to success " ,
          img(src = 'https://media.tenor.com/1rwOYKmmEN4AAAAj/loading.gif'),
          footer =
            NULL
        ))
        
        data <-
          weather_dl(
            station_ids = input$station,
            start = input$daterange2[1],
            end = input$daterange2[2],
            interval = input$interval
          )
        
        # })
        output$select_table <- renderTable({
          if (is.null(data)) {
            return(NULL)  # Return NULL if data is NULL
          } else {
            return(head(data))  # Render the head of the data if it exists
          }
          
          
        })
        
        output$prov <- renderText({
          if (is.null(data)) {
            return("No Data")  # Render "No Data" if data is NULL
          } else {
            return(
              paste(
                "Selected data contains ",
                nrow(data),
                " rows, available from ",
                data[1,]$day,
                "/",
                data[1,]$month ,
                "/",
                data[1,]$year,
                "-",
                data[nrow(data),]$day,
                "/",
                data[nrow(data),]$month ,
                "/",
                data[nrow(data),]$year
              )
            )  # Render empty text if data exists
          }
        })
        
        
        output$prec_map <- renderPlotly({
          # Print structure of data frame to check column names
          print(str(data))
          
          # Check if 'precip_amt' column exists in data frame
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
        
        output$temp_map <- renderPlotly({
          print(str(data))
          # Check if 'precip_amt' column exists in data frame
          if (!"precip_amt" %in% colnames(data)) {
            stop("")
          }
          p <- ggplot(data, aes(x = time, y = precip_amt)) +
            geom_line(aes(y = temp), color = "#001fff") +
            theme_few(base_size = 10) +
            ylab("Temperature")
          plotly::ggplotly(p) %>%
            config(displaylogo = F)
        })
        
        removeModal()
        
        
        output$down <- downloadHandler(
          filename = function() {
            paste(
              "data-",
              input$station,
              "-",
              input$daterange2[1],
              "-",
              input$daterange2[2],
              ".csv",
              sep = ""
            )
          },
          content = function(file) {
            write.csv(data, file)
          }
        )
        
      })
      
      
      
    })
    
    
    
    
    
    
    
    
    
    ###############################HYDROCLIMATE
    observe({
      updateSelectizeInput(session,
                           "streamstation",
                           choices =  realtime_stations(prov_terr_state_loc = input$province)[1])
      
      updateSelectizeInput(session,
                           "param",
                           choices =  param_id[4])
      
      
      output$station_table <- renderTable({
        head(realtime_stations(prov_terr_state_loc = input$province))[, c(1:4)]
      })
      
    })
    
    output$map_stream <- renderLeaflet({
      out2 <-
        realtime_stations(prov_terr_state_loc = input$province)
      if (is.null(out2) || nrow(out2) == 0) {
        return(NULL)
      }
      leaflet(out2, options = leafletOptions(attributionControl = FALSE)) %>% addTiles() %>%
        addCircleMarkers(
          lng = ~ LONGITUDE,
          lat = ~ LATITUDE,
          popup = ~ as.character(paste(
            "Station ID: ", STATION_NUMBER, "-", STATION_NAME
          ))
          
        )
    })
    
    
    
    observe({
      observeEvent(input$valid_stream, {
        showModal(modalDialog(
          "Patience is the key to success",
          img(src = 'https://media.tenor.com/1rwOYKmmEN4AAAAj/loading.gif'),
          footer = NULL
        ))
        
        data <- tryCatch({
          realtime_ws(
            station_number = input$streamstation,
            parameters = as.numeric(filter(param_id, Name_En == input$param)[1]),
            end_date = input$daterange_stream[2],
            start_date = input$daterange_stream[1]
          )
        }, error = function(e) {
          showModal(
            modalDialog(
              "Error: No data retrieved for the selected station query!",
              footer = tagList(
                actionButton("closeErrorModal", "Close", class = "btn-default")
              )
            )
          )
          return(NULL)  # Return NULL to prevent further execution
        })
        
        # Check if data is not NULL and proceed with rendering
        if (!is.null(data) && !inherits(data, "try-error")) {
          output$select_table_stream <- renderTable({
            head(data)[, c(1:5)]
          })
          
          
          output$prov_stream <- renderText({
            paste(
              "Selected data contains",
              nrow(data),
              "rows, available from",
              data[1,]$Date,
              "-",
              data[nrow(data),]$Date
            )
            
          })
          
          output$stream_map <- renderPlotly({
            p <- ggplot(data, aes(x = Date, y = Value)) +
              geom_line(aes(y = Value), color = "#002777") +
              theme_few(base_size = 10) +
              ylab(input$param)
            plotly::ggplotly(p) %>%
              config(displaylogo = F)
          })
          
          
          
          # Remove modal dialog after rendering
          removeModal()
        }
        
        
        
        
        # Download handler for downloading data as CSV
        output$down_stream <- downloadHandler(
          filename = function() {
            paste(
              "data-",
              input$streamstation,
              "-",
              input$daterange_stream[1],
              "-",
              input$daterange_stream[2],
              ".csv",
              sep = ""
            )
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
    
  }

  

shinyApp(ui = ui, server = server)
