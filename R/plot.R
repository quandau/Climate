
# ====== PLOT FUNCTIONS =========================================
# These functions are used to provide multiple different plots
# Written by QUAN DAU
# Climate Smart Lab, UPEI
# ---------------------------------------------------------------

# Function to plot data
single_plot <- function(x){
  plot(x, col = "tomato2",type = "l", lty = 1, xlab = "Time", ylab = "Data ")
}

double_plot <- function(date, x ,y){
  par(mfrow = c(2,1))
  plot(date, x, type ="l", col = "blue", ylab ="Variable1",  xlab ="Date", main = paste(head(date, n=1)[1]," - " , tail(date, n= 1)[1]))
  plot(date, y, type ="l", col = "pink", ylab = "Variable2", xlab ="Date")
  
}


# Function to plot the original data
plot_orignal <- function(dataset, originalData, station) {
  plot(
    dataset$Date,
    dataset$WL,
    type = "l",
    ylim = c(-2, 5),
    col = "springgreen2",
    ylab = "Tide (m)",
    xlab = "Date",
    main = paste(
      station,
      " - Original Data:",
      head(originalData$Date, n = 1) ,
      " - ",
      tail(originalData$Date, n = 1)
    )
  )
  if (!is.null(dataset$WL_CGVD2013)) {
    lines(dataset$Date,
          dataset$WL_CGVD2013,
          type = "l",
          col = "brown")
  } else {
    return(dataset)
  }
}


# Function to plot data with different CGVD
plot_cgcd <- function(dataset,
                      originalData,
                      station,
                      datum,
                      note) {
  dataset$WL2 <-
    ifelse(is.na(dataset$WL), dataset$WL_CGVD2013 - datum, dataset$WL)
  plot(
    dataset$Date,
    dataset$WL2,
    type = "l",
    col = "violetred",
    ylab = "Tide (m)",
    xlab = "Date",
    main = paste(
      station,
      " - CGVD to CD ",
      head(originalData$Date, n = 1) ,
      " - ",
      tail(originalData$Date, n = 1)
    )
  )
}



