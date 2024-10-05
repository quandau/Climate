

#=====================  EXTRACT STORM SURGE FROM TIDE PREDICTION MODEL ==============
# These functions are used to extract storm surge from tide prediction model
# Written by QUAN DAU,  2024
# Climate Smart Lab, UPEI
# ----------------------------------------------------------------------------------

# Function to extract storm surge out of tide prediction
stormSurge <-
  function(joinStation,
           station,
           stationName,
           datum) {
    joinStation$Date <-
      as.POSIXct(joinStation$Date, format = "%m/%d/%Y %H:%M")
    
    joinStation$WL2 <-
      ifelse(is.na(joinStation$WL),
             joinStation$WL_CGVD2013 - datum,
             joinStation$WL)
    
    #Defined the available period
    station$Date <-
      as.POSIXct(station$Date, format = "%m/%d/%Y %H:%M")
    
    begin <- head(station$Date, n = 1)
    end <- tail(station$Date, n = 1)
    
    startDate <- which(joinStation == as.character(begin))
    endDate <- which(joinStation == as.character(end))
    joinStation <- joinStation[c(startDate:endDate),]
    
    # Predict with Tide
    datsl <-
      as.sealevel(elevation = joinStation$WL2, time = joinStation$Date)
    mod <- tidem(t = datsl)
    
    #Calibration
    joinStation$Predic <-
      predict(mod, joinStation = joinStation$Date)
    
    
    #Extract Storm Surge
    joinStation$surge <-
      ifelse(joinStation$WL2 - joinStation$Predic < 0,
             NA,
             joinStation$WL2 - joinStation$Predic)
    
    joinStation$Date <- as.POSIXct(joinStation$Date, format = "%m/%d/%Y %H:%M")
    
    # Remove NA values
    stormsurge <- joinStation[complete.cases(joinStation$surge),]
    write.csv(joinStation, file = paste0("Surge/", stationName, ".csv"))
    
    #Filling NA values for the available period
    joinStation$filling <- ifelse(is.na(joinStation$WL2), joinStation$Predic, joinStation$WL2 )
    
    joinStation_NAremoved <- joinStation[complete.cases(joinStation$filling),]
    saveRDS(joinStation_NAremoved, file = paste0("Surge/", stationName, "_Tide_Surge.Rds"))
    
    
    par(mfrow = c(2, 1))
    plot(
      joinStation$Date,
      joinStation$WL2,
      type = "l",
      col = "gray",
      xlab = "Date",
      ylab = "Storm Surge (m)",
      main = paste(stationName, "Storm Surge between ", begin, "-", end)
    )
    lines(
      joinStation$Date,
      joinStation$Predic,
      type = "l",
      col = "violetred1",
      lty = 2
    )
    abline(fit <-
             lm(joinStation$WL2 ~ joinStation$Predic), col = 'pink')
    legend(
      "topleft",
      legend = c("Observation", "Prediction"),
      col = c("gray", "violetred1"),
      lty = 1:2,
      cex = 0.9,
      bg = "transparent"
    )
    legend("topright",
           bty = "n",
           legend = paste("Storm Surge  R^2 =", format(summary(fit)$adj.r.squared, digits = 4)))
    
    plot(
      stormsurge$Date,
      stormsurge$surge,
      type = "l",
      col = "yellow3",
      xlab = "Date",
      ylab = "Storm Surge (m)",
      main = paste(
        stationName,
        "Storm Surge = Observed WL - Predicted Tide (Foreman 1978)"
      )
    )
    
  }



# This function is use to predict tide data
# Using Foreman 1978 method: https://www.researchgate.net/publication/264782849_Manual_for_Tidal_Currents_Analysis_and_Prediction
predictTide <-
  function(joinStation,
           station,
           nameStation,
           calibrate,
           validation,
           
           datum) {
    calibrate$Date <-
      as.POSIXct(calibrate$Date, format = "%m/%d/%Y %H:%M")
    
    validation$Date <-
      as.POSIXct(validation$Date, format = "%m/%d/%Y %H:%M")
    
    
    calibrate$WL2 <-
      ifelse(is.na(calibrate$WL), calibrate$WL_CGVD2013 - datum, calibrate$WL)
    validation$WL2 <-
      ifelse(is.na(validation$WL), validation$WL_CGVD2013 - datum, validation$WL)
    
    datsl <-
      as.sealevel(elevation = calibrate$WL2, time = calibrate$Date)
    mod <- tidem(t = datsl)
    
    #Calibration
    calibrate$Predic <- predict(mod, calibrate = calibrate$Date)
    #Validation
    validate_result <- predict(mod, newdata = validation$Date)
    
    
    
    par(mfrow = c(4, 1))
    
    plot_orignal(joinStation, station, nameStation)
    
    plot(
      calibrate$Date,
      calibrate$WL2,
      type = "l",
      col = "red",
      xlab = "Date",
      ylab = "Tide (m)",
      main = paste(
        "Calibration from ",
        head(calibrate$Date, n = 1) ,
        " - ",
        tail(calibrate$Date, n = 1)
      )
    )
    lines(
      calibrate$Date,
      calibrate$Predic,
      type = "l",
      col = "blue",
      lty = 2
    )
    
    abline(fit <- lm(calibrate$WL2 ~ calibrate$Predic), col = 'pink')
    legend(
      "topright",
      legend = c("Observation", "Prediction"),
      col = c("red", "blue"),
      lty = 1:2,
      cex = 0.9,
      bg = "transparent"
    )
    legend("bottomright",
           bty = "n",
           legend = paste("R^2 is", format(summary(fit)$adj.r.squared, digits = 4)))
    
    plot(
      validation$Date,
      validation$WL2,
      type = "l",
      col = "red",
      xlab = "Date",
      ylab = "Tide (m)",
      main = paste(
        "Validation from ",
        head(validation$Date, n = 1) ,
        " - ",
        tail(validation$Date, n = 1)
      )
    )
    lines(
      validation$Date,
      validate_result,
      type = "l",
      col = "blue",
      lty = 2,
      lwd = 2
    )
    abline(fit <- lm(validation$WL2 ~ validate_result), col = 'pink')
    legend(
      "topright",
      legend = c("Observation", "Prediction"),
      col = c("red", "blue"),
      lty = 1:2,
      cex = 0.9,
      bg = "transparent"
    )
    legend("bottomright",
           bty = "n",
           legend = paste("R^2 is", format(summary(fit)$adj.r.squared, digits = 4)))
    
    
    
    #Prediction for missing period
    joinStation$WL2 <-
      ifelse(is.na(joinStation$WL), joinStation$WL_CGVD2013 - datum, joinStation$WL)
    
    prediction<- filter(joinStation, is.na(WL2))
    
    prediction$Date <-  as.POSIXct(prediction$Date, format = "%m/%d/%Y %H:%M")
    predict_result <- predict(mod, newdata = prediction$Date)
    
    
    pred <- data.frame(Date = prediction$Date, WL = predict_result)
    pred$Date <- as.POSIXct(pred$Date, format = "%m/%d/%Y %H:%M")
    
    joinStation$Date <- as.POSIXct(joinStation$Date, format = "%m/%d/%Y %H:%M")
    newdata <- left_join(joinStation, pred, by = "Date")
    
    plot(
      newdata$Date,
      newdata$WL2,
      type = "l",
      col = "gray",
      main = paste("FILLING MISSING DATA - CGVD2013 (m) = ", datum)
    )
    lines(newdata$Date,
          newdata$WL.y,
          type = "l",
          col = "violet")
    legend(
      "topright",
      legend = c("Observation", "Prediction"),
      col = c("gray", "violet"),
      lty = 1,
      cex = 0.9,
      bg = "transparent"
    )
  }
