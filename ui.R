library(shinydashboard)
library(shiny)
library(leaflet)
library(tidyhydat)
library(weathercan)
library(plotly)
library(ggplot2)
library(ggthemes)
library(lutz)
library(dplyr)
library(copula)
library(MASS)
library(lubridate)
library(zoo)
library(evd)
library(fitdistrplus)
library(oce)
library(tidyr)
library(ggforce)
library(shinyBS)
library(shinymanager)
library(DBI)
library(RSQLite)
library(httr)
library(jsonlite)
library(R.utils)
library(leaflet.extras)
library(DT)
library(colourpicker)

ui <- dashboardPage(
  dashboardHeader(title = "Climate Smart Lab Portal", titleWidth = 260),
  dashboardSidebar(width = 200,
                   sidebarMenu(
                     id = "climate",
                     menuItem("Home", tabName = "home", icon = icon("home")),
                     menuItem("CAN Weather", tabName = "met", icon = icon("cloud-sun-rain")),
                     menuItem("CAN Hydrology", tabName = "hyd", icon = icon("house-tsunami")),
                     menuItem("GLOBAL Tide", tabName = "tide", icon = icon("house-flood-water-circle-arrow-right")),
                     menuItem("GLOBAL Weather", tabName = "weather", icon = icon("cloud-showers-water")),
                     menuItem("COPULAS Analysis", tabName = "copulas", icon = icon("magnifying-glass-chart")),
                     absolutePanel(
                       bottom = 0, left = 50, 
                       width = 100, height = 100,
                       draggable = FALSE,
                       HTML("<div style='font-size:12px; alignment: center; color:gray;'>Quan Dau (2024)<br>Climate Smart Lab</div>")
                     )
                   )
  ),
  dashboardBody(
    withMathJax(),
    # CAN Weather Tab
    tabItems(
      tabItem(
        tabName = "home",
        HTML("<h2><strong>Research Portal </strong></h2><br>"),
        img(
          src = 'logo.png',
          align = "center",
          height = "40%",
          width = "40%"
        ), align ="center"
      ),
      tabItem(
        tabName = "met",
        fluidRow(
          column(
            3,
            box(
              title = "Search Data",
              width = 12,
              status = "primary",
              solidHeader = TRUE,
              textInput("search_station", "Search Station"),
              radioButtons("interval", "Interval", choices = c("hour", "day", "month"), inline = TRUE),
              selectizeInput("station", "Station", choices = NULL),
              dateRangeInput("daterange2", "Date range:"),
              actionButton("valid", "Validate", icon = icon("check-double"), class = "btn-success btn-lg"),
              downloadButton("down", "Download")
            )
          ),
          box(
            width = 9,
            column(6, tableOutput("data_table")),
            column(6, leafletOutput("map"))
          )
        ),
        box(
          width = 12,
          tableOutput("select_table"),
          textOutput("prov"),
          fluidRow(
            column(6, plotlyOutput("prec_map")),
            column(6, plotlyOutput("temp_map"))
          )
        )
      ),
      # CAN Hydrology Tab
      tabItem(
        tabName = "hyd",
        fluidRow(
          column(
            3,
            box(
              title = "Search Data",
              width = 12,
              status = "primary",
              solidHeader = TRUE,
              selectizeInput(
                "province", "Province", 
                choices = c("AB", "BC", "MB", "NB", "NL", "NT", "NS", "NU", "ON", "PE", "QC", "SK", "YT"), 
                selected = "PE"
              ),
              selectizeInput("streamstation", "Station", choices = NULL),
              selectizeInput("param", "Parameter", choices = NULL),
              dateRangeInput("daterange_stream", "Date range:"),
              actionButton("valid_stream", "Validate", icon = icon("check-double"), class = "btn-success btn-lg"),
              downloadButton("down_stream", "Download")
            )
          ),
          box(
            width = 9,
            column(6, tableOutput("station_table")),
            column(6, leafletOutput("map_stream"))
          )
        ),
        fluidRow(
          box(
            width = 12,
            status = "primary",
            column(4, tableOutput("select_table_stream"), textOutput("prov_stream")),
            column(8, plotlyOutput("stream_map"))
          )
        )
      ),
      # GLOBAL Tide Tab
      tabItem(
        tabName = "tide",
        fluidRow(
          
          column(
            3,
            box(
              title = "Search Data",
              width = 12,
              status = "primary",
              solidHeader = TRUE,
              selectInput("search_country", "Select Country", choices = ""),
              selectInput("search_loc", "Select Location", choices = ""),
              selectInput("search_id", "Select Station ID", choices = ""),
              radioButtons("interval_tide", "Time Step", choices = c("d" , "h"), selected = "d", inline = TRUE),
              actionButton("valid_tide", "Validate", icon = icon("check-double"), class = "btn-info btn-md"),
              downloadButton("down_tide", "Download"),
              actionButton("reset", "Reset", icon = icon("power-off"), class = "btn-warning btn-md"),
              actionButton("selec_sta", " ", icon = icon("location-pin"), class = "btn-success btn-md")
            ),
            box(width = 12, title = "Manage Station from Local",
                tabsetPanel(
                  tabPanel("Validate Data", icon = icon("check"),
                           fileInput("validate_observe", "Choose CSV file",  accept = c("text/csv",  "text/comma-separated-values,text/plain",  ".csv"))
                  ),
                  tabPanel("Remove Station", icon = icon("trash"),
                           column(8,
                                  selectInput("del_station_tide", "Select Station to Delete", choices = "")
                           ),
                           column(4,
                                  actionButton("del_station_tide_submit", "Delete", icon = icon("trash"), class = "btn-danger btn-md")
                           )
                  )
                )
            )
            
          ),
          
          box(width = 9, 
              tabsetPanel(
                tabPanel("Global Station", icon = icon("location"),
                         leafletOutput("map_tide", height = "550px")
                ),
                tabPanel("Sort Station", icon = icon("sort"),
                         column(4,
                                sliderInput("year_threshold", "Sort by Year Before Download", min = 1, max = 100, value = 30, step = 1, post = " Year"), 
                                actionButton("global_tide_download", "", class = "btn-danger btn-sm")
                         ),
                         column(4,
                                sliderInput("year_threshold_2", "Filter by Year After Download", min = 1, max = 100, value = 30, step = 1, post = " Year"), 
                                actionButton("filter_by_year", "Filter Data", icon = icon("broom"), class = "btn-info btn-sm"),
                                textOutput("remain_year")
                         ),
                         column(4,
                                sliderInput("missing_threshold", "Missing Ratio", min = 0, max = 100, value = 5, step = 1, post = " %"), 
                                actionButton("filter_by_ratio", "Filter Data", icon = icon("broom"), class = "btn-warning btn-sm"),
                                textOutput("remain_station")
                         ),
                         leafletOutput("map_tide_sortout", height = "500px")  
                )     
              )
          )
        ),
        box(
          width = 12,
          column(4, tableOutput("table_tide")),
          column(8, plotlyOutput("tide_map"))
        )
      ),
      # GLOBAL Weather Tab
      tabItem(
        tabName = "weather",
        fluidRow(
          column(
            3,
            box(
              title = "Search Daily Data",
              width = 12,
              status = "primary",
              solidHeader = TRUE,
              textInput("search_lat", "Latitude", value = ""),
              textInput("search_lon", "Longitude", value = ""),
              textInput("time_zone", "Time-Zone", value = "", placeholder = "Automatic ..."),
              selectInput("climate_variable", "Select Variable", choices = c("precipitation_sum", "temperature_2m_max", "temperature_2m_min", "temperature_2m_mean", "et0_fao_evapotranspiration", "wind_speed_10m_max", "wind_gusts_10m_max"), selected = "precipitation_sum"),
              dateRangeInput("daterange_global", "Date range:"),
              actionButton("valid_global_data", "Validate", icon = icon("check-double"), class = "btn-info btn-md"),
              downloadButton("down_global_data", "Download"),
              actionButton("extract_coord", "Download ALL", icon = icon("circle-down"), class = "btn-success btn-md")
            ),
            box(width =12, title = "Manage Station from Local",
                column(8,
                       selectInput("del_station", "Select Statation to Delete", choices = "" )),
                column(4,
                       actionButton("del_station_submit", "Delete", icon = icon("trash"), class = "btn-danger btn-md"))
            ),
            uiOutput("table_title"), 
            tableOutput("weather_global_data")
          ),
          box(width = 9, 
              tabsetPanel(
                tabPanel("Weather", icon = icon("sun"),
                         leafletOutput("map_global_data", height = "900px"),
                         absolutePanel(bottom = 10, right = 10, 
                                       width = 400, height = 300,
                                       draggable = TRUE,
                                       plotOutput("rain_plot_global", height = "300px")
                         )
                ),
                tabPanel("Gauging Station", icon = icon("check"),
                         column(1,
                         numericInput("buffer", "Buffer (Km)", value = 50, min = 0, max = 500, step = 1)),
                         column(2,
                         actionButton("distance_gauging_weather", "Calculate Distance", icon = icon("ruler"), class = "btn-warning btn-md")),
                         column(2,
                                actionButton("download_gauging_weather", "Download", icon = icon("download"), class = "btn-danger btn-md")),
                                  bsModal("request_download", "Information before download",  "download_gauging_weather", size = "medium",
                                          uiOutput("request_count"),
                                          actionButton("download_gauging_weather_submit", "Process", icon = icon("download"), class = "btn-danger btn-md")), 
                         leafletOutput("gauging_station", height = "800px")
                         
                        ),
                        )
            )
        )
      ),
      # COPULAS Analysis Tab
      tabItem(
        
        tabName = "copulas", fluidRow(column(
          3,
          box(
            title = "Execute Options",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            background = "light-blue",
            radioButtons(
              "automate_switch",
              "",
              choices = c("Automate", "Manual"),
              selected = "Automate"
            ),
            selectInput("station_process", "Available Stations", choices = "")
          )
        ), column(
          9,
          box(
            title = "Messeges",
            width = 12,
            status = "primary",
            collapsible = T,
            collapsed = F,
            solidHeader = TRUE,
            selectInput(
              "remove_station",
              "Remove Stations (Water Level ID)",
              choices = seq(1, 1000, by = 1),
              selected = c(4, 24, 30, 38, 81, 91, 142, 175, 242, 257, 281, 574, 592, 801, 802),
              multiple = TRUE
            ),
            actionButton("remove_station_submit", "Remove", icon = icon("trash")),
            uiOutput("message")
            
          )
        )), fluidRow(box(
          width = 12,
          tabsetPanel(
            tabPanel(
              "PROCESSING",
              icon = icon("broom"),
              h3("This Step Aims to Join Rain and Tide in Daily Time-Step"),
              fluidRow(
             
                column(
                  2,
                  actionButton(
                    "process_data",
                    "PROCESSING",
                    icon = icon("broom"),
                    class = "btn-danger btn-lg"
                  )
                ),
                column(
                  1,
                  colourInput(
                    "colopickup1",
                    "Precipitation",
                    "#20E086",
                    allowTransparent = TRUE,
                    closeOnClick = TRUE
                  )
                ),
                column(
                  1,
                  colourInput(
                    "colopickup2",
                    "Water Level",
                    "#FF1155",
                    allowTransparent = TRUE,
                    closeOnClick = TRUE
                  )
                  
                ),
              
                column(
                  1,
                  dateRangeInput(
                    "trim_date",
                    "Trim Range"
                  )
                ),
                column(
                  1,
                  dateRangeInput(
                    "adjust_date",
                    "Adjust Range"
                  )
                ),
                column(
                  1,
                  selectInput(
                    "adjust_data_variable",
                    "Variable", choices = c("Rain", "Tide"), selected = "Tide",
                  )
                ),
                column(
                  1,
                  numericInput(
                    "adjust_threshold",
                    "Adjust Threshold", value = 0, min = -10, max = 100
                  )
                ),
                
                # column(
                #   1,
                # radioButtons(
                #   "adjusted_switch",
                #   "Dataset Types",
                #   choices = c("Adjusted", "Original"),
                #   selected = "Adjusted"
                # )
                # ),
                column(
                  1,
                  actionButton(
                    "trim_date_submit",
                    "Trim", icon= icon("scissors"), class = "btn-warning btn-md"
                  )
                ),
                column(
                  1,
                  actionButton(
                    "reset_adjust",
                    "Reset", icon= icon("power-off"),  class = "btn-info btn-md"
                  )
                ),
                column(
                  1,
                  actionButton(
                    "rewrite_submit",
                    "Save", icon= icon("save"),  class = "btn-success btn-md"
                  )
                ),  
                bsModal("request_rewrite", "This creates a new dataset - Original data won't affect.  Are you sure?",  "rewrite_submit", size = "medium",
                        actionButton(
                          "rewrite_submit_original", "Proceed", icon= icon("save"),  class = "btn-danger btn-md"
                        )),
              ),
              fluidRow(
                plotlyOutput("process_plot"),
                column(5, DTOutput("global_pair_data")),
                column(7, leafletOutput("process_plot_cor", height = "550px"))
              )
            ),
            tabPanel(
              "NORMALISATION",
              icon = icon("sort"),
              h3(
                "This Step Appied A Percentile-Normalisation Distance-Based Approach"
              ),
              h5("Select Station Above and Submit to see the Result"),
              actionButton(
                "distance_based",
                "Perform Normalization",
                icon = icon("code-merge"),
                class = "btn-success btn-lg"
              ),
              fluidRow(column(6, plotlyOutput("plot_distance")), column(6, plotOutput(
                "normalise_plot_cor"
              ))),
              fluidRow(leafletOutput("normalise_global", height = "600px"))
            ),
            tabPanel(
              "COPULAS ANALYSIS",
              icon = icon("codepen"),
              h3("This Step Perform with the Copulas Analysis"),
              h5("Select Station Above and Submit to see the Result"),
              actionButton(
                "copula_analysis",
                "Perform Copulas Analysis",
                icon = icon("chart-bar"),
                class = "btn-warning btn-lg"
              ),
              fluidRow(
                box(
                  width = 3,
                  title = "Precipitation",
                  tableOutput("copula_ks_rain")
                ),
                box(
                  width = 3,
                  title = "Tide",
                  tableOutput("copula_ks_tide")
                ),
                box(width = 3, plotlyOutput("fitting")),
                box(width = 3, plotOutput("aic_bic"))
              ),
              fluidRow(column(6, plotOutput("cdf_p")), column(6, plotOutput("cdf_h")))
            ),
            tabPanel(
              "RETURN PERIODS",
              icon = icon("modx"),
              h3("This Step Perform Joint Return Period"),
              h5("Here we assumed GEV approach was applied for both variables"),
              fluidRow(
                column(
                  3,
                  selectInput(
                    "rp",
                    "Select Return Period",
                    choices = c(1, 2, 5, 10, 15, 20, 25, 50, 75, 100, 200, 300, 500, 1000),
                    selected = c(10, 25, 50, 100),
                    multiple = T
                  )
                ),
                column(
                  3,
                  actionButton(
                    "rp_submit",
                    "Calculate Return Period",
                    icon = icon("chart-bar"),
                    class = "btn-danger btn-lg"
                  )
                ),
                column(
                  3,
                  actionButton(
                    "review_idf",
                    "View IDF Table",
                    icon = icon("chart-bar"),
                    class = "btn-info btn-md"
                  ),
                  bsModal(
                    "ViewTable",
                    "Information for Charlottetown",
                    "review_idf",
                    size = "medium",
                    tableOutput("table_idf_rp")
                  )
                ),
                column(
                  3,
                  actionButton(
                    "contour_plot",
                    "Contour Plot",
                    icon = icon("chart-bar"),
                    class = "btn-info btn-md"
                  ),
                )
              ),
              fluidRow(
                column(
                  4,
                  selectInput(
                    "copula_model",
                    "Filter with Copula Model",
                    choices = c("Gaussian", "Clayton", "Frank", "Gumbel", "Joe"),
                    selected = "Gaussian"
                  ),
                  tableOutput("rp_table")
                ),
                column(
                  7,
                  fluidRow(column(
                    3, selectInput("rp_select", "Select Return Period", choices = "")
                  )),
                  plotlyOutput("plot_rp", height = "550px"),
                  plotOutput("plot_rp_test", height = "600px"),
                )
              )
            )
          )
        ))
      )
    )
  )
)
