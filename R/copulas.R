
# ==============  COPULAS FUNCTIONS  =====================
# These functions are used to handle with Copulas Analysis
# Written by QUAN DAU - 2024
# Climate Smart Lab, UPEI
# --------------------------------------------------------

# ========================================================
# Function to create data (precipitation vs Tide)
extract_data <- function(data_station) {
  data.frame(P = data_station$Sum_Rain, H = data_station$Max_Tide)
}
# ========================================================


# ========================================================
#Find parameter for copulas families
parameter <- function(copula_function, data) {
  model_fit <- copula::fitCopula(copula_function, data = pobs(data), method = "ml")
  estimated_param <- model_fit@copula@parameters
  return(estimated_param)
}
# ========================================================



# ========================================================
# Fit Copula Approach
fit_and_summarize_copula <- function(copula, data, fitRain, fitTide, fitMethod) {
  # Define the distribution parameters for the margins
  params_rain <- list(loc = mean(data$P), scale = sd(data$P),  shape = 0)  
  params_tide <- list(loc = mean(data$H), scale = sd(data$H),  shape = 0)  
  
  # Create the margins object
  margins <- copula::mvdc(copula = copula, 
                  margins = c(fitRain, fitTide), 
                  paramMargins = list(params_rain, params_tide))
  
  # Convert data to pseudo-observations
  u <- pobs(data)
  
  # Fit the Copula model using the pseudo-observations
  copula_model <- copula::fitCopula(copula, u, method = fitMethod)  

  return(copula_model)
}
# ========================================================



# ========================================================
# Determine the best Copulas Model
calculate_aic_bic <- function(copula_model, data) {
  log_likelihood <- logLik(copula_model)
  n_params <- length(coef(copula_model))
  n <- nrow(data)
  
  aic <- -2 * as.numeric(log_likelihood) + 2 * n_params
  bic <- -2 * as.numeric(log_likelihood) + log(n) * n_params
  
  return(list(AIC = aic, BIC = bic))
}
# ========================================================



# ========================================================
# Function to compute K-S statistic 
compute_ks_stat <- function(data, dist_name, significant_level) {
  data <- na.omit(data)
  n <- length(data)
  
  # Shift data to ensure all values are positive
  shift_value <- max(0, -min(data)) + 1e-6
  shifted_data <- data + shift_value
  
  # Sort shifted data for empirical distribution function 
  sorted_shifted_data <- sort(shifted_data)
  
  # Fit the distribution and get parameters
  fit <- switch(dist_name,
                "norm" = fitdistrplus::fitdist(shifted_data, "norm"),
                "gamma" = fitdistrplus::fitdist(shifted_data, "gamma"),
                "weibull" = fitdistrplus::fitdist(shifted_data, "weibull"),
                "lnorm" = fitdistrplus::fitdist(shifted_data, "lnorm"),
                "exp" = fitdistrplus::fitdist(shifted_data, "exp"),
                "gev" = evd::fgev(shifted_data, std.err = FALSE),
                "logistic" = fitdistrplus::fitdist(shifted_data, "logis"),
                "cauchy" = fitdistrplus::fitdist(shifted_data, "cauchy"),
                stop("Distribution not supported")
  )
  
  # Handle the GEV distribution separately
  if (dist_name == "gev") {
    # Extract GEV parameters
    params <- fit$estimate
    
    # Define CDF
    cdf_function <- function(x) evd::pgev(x, loc = params["loc"], scale = params["scale"], shape = params["shape"])
    
    # Compute log-likelihood and AIC 
    log_likelihood <- sum(log(evd::dgev(shifted_data, loc = params["loc"], scale = params["scale"], shape = params["shape"])))
    num_params <- length(params)
    aic_value <- 2 * num_params - 2 * log_likelihood
    
  } else {
    # Extract parameters for non-GEV distributions
    params <- fit$estimate
    
    # Define the theoretical CDF based on the fitted distribution
    cdf_function <- switch(dist_name,
                           "norm" = function(x) pnorm(x, mean = params["mean"], sd = params["sd"]),
                           "gamma" = function(x) pgamma(x, shape = params["shape"], rate = params["rate"]),
                           "weibull" = function(x) pweibull(x, shape = params["shape"], scale = params["scale"]),
                           "lnorm" = function(x) plnorm(x, meanlog = params["meanlog"], sdlog = params["sdlog"]),
                           "exp" = function(x) pexp(x, rate = params["rate"]),
                           "logistic" = function(x) plogis(x, location = params["location"], scale = params["scale"]),
                           "cauchy" = function(x) pcauchy(x, location = params["location"], scale = params["scale"])
    )
    
    # For non-GEV
    log_likelihood <- fit$loglik
    num_params <- length(params)
    aic_value <- 2 * num_params - 2 * log_likelihood
  }
  
  # Compute empirical CDF and theoretical CDF for each point in sorted data
  empirical_cdf <- ecdf(sorted_shifted_data)
  theoretical_cdf <- cdf_function(sorted_shifted_data)
  
  # Compute the K-S statistic: max |F_exp(x) - F_obs(x)|
  ks_statistic <- max(abs(theoretical_cdf - empirical_cdf(sorted_shifted_data)))
  
  # Calculate the critical value for the given significance level
  ks_critical_value <- sqrt(-0.5 * log(significant_level / 2) / n)
  
  # Compare with critical value and determine if significant
  significant <- ks_statistic > ks_critical_value
  
  return(list(statistic = ks_statistic, aic = aic_value, fit = fit, significant = significant))
}

# ========================================================



# ========================================================
# Function to run K-S tests for multiple distributions
ks_test_distributions <- function(data, distributions, significant_level) {
  results <- data.frame(Distribution = character(), Statistic = numeric(), AIC = numeric(), Significant = logical(),  stringsAsFactors = FALSE)
  
  for (dist in distributions) {
    result <- tryCatch({
      ks_result <- compute_ks_stat(data, dist, significant_level)
      data.frame(Distribution = dist, Statistic = ks_result$statistic, AIC = ks_result$aic, Significant = ks_result$significant)
    }, error = function(e) {
      warning(paste("Error processing distribution:", dist, ":", e$message))
      return(data.frame(Distribution = dist, Statistic = NA, AIC = NA,  Significant = NA))
    })
    
    results <- rbind(results, result)
  }
  
  return(results)
}

# ========================================================

plot_fits <- function(data, distributions,significant_level) {
  par(mfrow = c(3, 3))  # Adjust layout to fit all plots
  
  for (dist in distributions) {
    ks_result <- compute_ks_stat(data, dist, significant_level)
    fit <- ks_result$fit
    params <- fit$estimate
    
    # Define the theoretical CDF function
    cdf_function <- switch(dist,
                           "norm" = function(x) pnorm(x, mean = params["mean"], sd = params["sd"]),
                           "gamma" = function(x) pgamma(x, shape = params["shape"], rate = params["rate"]),
                           "weibull" = function(x) pweibull(x, shape = params["shape"], scale = params["scale"]),
                           "lnorm" = function(x) plnorm(x, meanlog = params["meanlog"], sdlog = params["sdlog"]),
                           "exp" = function(x) pexp(x, rate = params["rate"]),
                           "gev" = function(x) evd::pgev(x, loc = params["loc"], scale = params["scale"], shape = params["shape"]),
                           "logistic" = function(x) plogis(x, location = params["location"], scale = params["scale"]),
                           "cauchy" = function(x) pcauchy(x, location = params["location"], scale = params["scale"])
    )
    
    # Plot the empirical and theoretical CDF
    plot(ecdf(data), main = paste("Fit for", dist), xlab = "Data", ylab = "CDF", col = "black")
    curve(cdf_function(x), add = TRUE, col = "red")
  }
  
  par(mfrow = c(1, 1))  # Reset layout
}


# ========================================================
# Plot CDF marginal 
plot_cdf_marginal <- function(data, variable) {
  
  # Shift data to ensure all values are positive
  shift_value <- max(0, -min(data)) + 1e-6
  shifted_data <- data + shift_value
  
  # Sort shifted data for empirical distribution function calculation
  sorted_shifted_data <- sort(shifted_data)
  
  # Fit the distribution to shifted data
  fit_norm <- fitdistrplus::fitdist(shifted_data, "norm")
  fit_gamma <- fitdistrplus::fitdist(shifted_data, "gamma")
  fit_weibull <- fitdistrplus::fitdist(shifted_data, "weibull")
  fit_lnorm <- fitdistrplus::fitdist(shifted_data, "lnorm") 
  fit_exp <- fitdistrplus::fitdist(shifted_data, "exp")
  fit_gev <- evd::fgev(shifted_data, std.err = FALSE)
  fit_logistic <- fitdistrplus::fitdist(shifted_data, "logis")
  fit_cauchy <- fitdistrplus::fitdist(shifted_data, "cauchy")
  
  # Create empirical CDF for shifted data
  empirical_cdf_shifted <- ecdf(shifted_data)
  
  # Plot empirical CDF
  plot(empirical_cdf_shifted, main = paste("CDF ", variable), xlab = "Data", ylab = "CDF", lwd = 2)
  
  # Add theoretical CDFs
  curve(pnorm(x - shift_value, mean = fit_norm$estimate["mean"], sd = fit_norm$estimate["sd"]), col = "tomato2", add = TRUE, lwd = 2)
  curve(pgamma(x - shift_value, shape = fit_gamma$estimate["shape"], rate = fit_gamma$estimate["rate"]), col = "royalblue1", add = TRUE, lwd = 2)
  curve(pweibull(x - shift_value, shape = fit_weibull$estimate["shape"], scale = fit_weibull$estimate["scale"]), col = "seagreen3", add = TRUE, lwd = 2)
  curve(plnorm(x - shift_value, meanlog = fit_lnorm$estimate["meanlog"], sdlog = fit_lnorm$estimate["sdlog"]), col = "deeppink3", add = TRUE, lwd = 2)
  curve(pexp(x - shift_value, rate = fit_exp$estimate["rate"]), col = "goldenrod3", add = TRUE, lwd = 2)
  curve(evd::pgev(x - shift_value, loc = fit_gev$estimate["loc"], scale = fit_gev$estimate["scale"], shape = fit_gev$estimate["shape"]), col = "brown", add = TRUE, lwd = 2)
  curve(plogis(x - shift_value, location = fit_logistic$estimate["location"], scale = fit_logistic$estimate["scale"]), col = "blue", add = TRUE, lwd = 2)
  curve(pcauchy(x - shift_value, location = fit_cauchy$estimate["location"], scale = fit_cauchy$estimate["scale"]), col = "purple", add = TRUE, lwd = 2)
  
  # Update the legend to include logistic and cauchy distributions
  legend("bottomright", legend = c("Empirical", "Normal", "Gamma", "Weibull", "Lognormal", "Exponential", "GEV", "Logistic", "Cauchy"),
         col = c("black", "tomato2", "royalblue1", "seagreen3", "deeppink3", "goldenrod3", "brown", "blue", "purple"), lwd = 2)
}


# ========================================================



# ========================================================
# Function to test relationship based on Kendall
kendall_test <- function(date, x, y, alpha_limit, name_method,stationName){
  # Perform Kendall's Tau test
  kendall_test <- cor.test(x, y, method = name_method)
  
  # Extract Kendall's Tau and p-value
  tau_value <- kendall_test$estimate
  p_value <- kendall_test$p.value

  
  tau_n <- kendall_test$estimate
  n <- length(x)
  #Equations here https://agupubs.onlinelibrary.wiley.com/doi/10.1029/2022EF002655
  tau <- sqrt((9 * n * (n - 1)) / (2 * (2 * n + 5))) * abs(tau_n)
  p_value <- 2 * (1 - pnorm(tau))

  alpha <- alpha_limit
  lm_model <- lm(y ~ x)
  if (p_value < alpha) {
    note <- paste(" The test at ",stationName," : ", as.Date(head(date, n = 1)) ,
                  " - ", as.Date(tail(date, n = 1)), " is SIGNIFICANCE at ", 
                  alpha_limit, "\n Correlation =", round(tau_value, 3), " | p-value = ", format.pval(p_value, digits = 3))
  } else {
    note <- paste(" The test at ",stationName," : ", as.Date(head(date, n = 1)) ,
                  " - ", as.Date(tail(date, n = 1)), " is NOT significant at ", 
                  alpha_limit, "\n Correlation =", round(tau_value, 3), " | p-value = ", format.pval(p_value, digits = 3))
  }
  
  # Create a scatter plot of rainfall vs tide
  plot(x, y, 
       main = note,
       xlab = "Precipitation",
       ylab = "Tide",
       pch = 15,
       cex.main = 2,
       col = "gray")
  
  abline(lm_model, col = "blue", lwd = 2)

}
# ========================================================


# ========================================================
# Calculate Joint return Period
calculate_joint_return_period <- function(fitted_copula, return_period_years, data) {
  # Fit GEV distributions for rainfall (P) and tide (H)
  p_fit <- evd::fgev(data$P)
  h_fit <- evd::fgev(data$H)
   
  
  # Extract GEV parameters for rainfall
  loc_P <- p_fit$estimate["loc"]
  scale_P <- p_fit$estimate["scale"]
  shape_P <- p_fit$estimate["shape"]
  
  # Extract GEV parameters for tide
  loc_H <- h_fit$estimate["loc"]
  scale_H <- h_fit$estimate["scale"]
  shape_H <- h_fit$estimate["shape"]
  
  # Define CDF and quantile functions for the GEV distributions
  marginal_P_cdf <- function(x) pgev(x, loc = loc_P, scale = scale_P, shape = shape_P)
  marginal_H_cdf <- function(x) pgev(x, loc = loc_H, scale = scale_H, shape = shape_H)
  marginal_P_qf <- function(p) qgev(p, loc = loc_P, scale = scale_P, shape = shape_P)
  marginal_H_qf <- function(p) qgev(p, loc = loc_H, scale = scale_H, shape = shape_H)
  
  # Initialize a list to store the joint return periods
  joint_return_periods <- list()
  
  # Copula model
  copula_model <- fitted_copula@copula
  
  # Loop over each return period
  for (rp in return_period_years) {
    ep <- 1 / rp  # Exceedance probability
    
    # Marginal quantile thresholds
    x1_threshold <- marginal_P_qf(1 - ep)
    x2_threshold <- marginal_H_qf(1 - ep)
    
    # Marginal exceedance probabilities
    u1 <- 1 - marginal_P_cdf(x1_threshold)
    u2 <- 1 - marginal_H_cdf(x2_threshold)
    
    # Check if u1 and u2 are within (0, 1) before calculating joint probabilities
    if (u1 > 0 & u1 < 1 & u2 > 0 & u2 < 1) {
      
      # Joint exceedance probabilities using the copula model
      joint_ep_and <- copula::pCopula(c(u1, u2), copula_model)
      joint_ep_or <- u1 + u2 - joint_ep_and
      
      # Convert exceedance probabilities back to quantiles
      u_rp_and <- marginal_P_qf(1 - joint_ep_and)
      v_rp_and <- marginal_H_qf(1 - joint_ep_and)
      u_rp_or <- marginal_P_qf(1 - joint_ep_or)
      v_rp_or <- marginal_H_qf(1 - joint_ep_or)
      
      # Store the results for the current return period
      joint_return_periods[[as.character(rp)]] <- list(
        AND = list(
          joint = list(
            value = c(P = u_rp_and, H = v_rp_and),
            `Prob(%)` = joint_ep_and * 100
          )
        ),
        OR = list(
          joint = list(
            value = c(P = u_rp_or, H = v_rp_or),
            `Prob(%)` = joint_ep_or * 100
          )
        )
      )
      
    } else {
      # If probabilities are out of bounds
      warning(paste("Exceedance probabilities for return period", rp, "are out of bounds"))
    }
  }
  
  return(joint_return_periods )
}

# ========================================================

plot_joint_return_periods <- function(copula_models, return_period_years, data) {
  # Fit GEV distributions for rainfall (P) and tide (H)
  p_fit <- evd::fgev(data$P)
  h_fit <- evd::fgev(data$H)
  
  # Prepare joint return periods
  joint_return_periods <- lapply(copula_models, calculate_joint_return_period, return_period_years, data)
  
  # Initialize a data frame to store AND and OR points for each copula model
  points_df <- data.frame()
  
  # Loop through each return period and extract AND/OR points
  for (rp in return_period_years) {
    for (copula_name in names(joint_return_periods)) {
      joinRP <- joint_return_periods[[copula_name]][[as.character(rp)]]
      and_values <- unlist(joinRP$AND$joint$value)  # Ensure values are unlisted correctly
      or_values <- unlist(joinRP$OR$joint$value)    # Unlist OR values
      
      points_df <- rbind(points_df, data.frame(
        Model = copula_name,
        Return_Period = rp,
        P_AND = and_values['P'],
        H_AND = and_values['H'],
        P_OR = or_values['P'],
        H_OR = or_values['H']
      ))
    }
  }
  
  # Extract maximum values for scaling
  rainfall_max <- max(data$P, na.rm = TRUE)
  tide_max <- max(data$H, na.rm = TRUE)
  
  # Create a grid for copula input
  x_seq <- seq(0, rainfall_max, length.out = 50)
  y_seq <- seq(0, tide_max, length.out = 50)
  
  # Calculate the joint probability density for the surface
  z <- outer(x_seq, y_seq, Vectorize(function(x, y) {
    u <- pgev(x, loc = p_fit$estimate["loc"], scale = p_fit$estimate["scale"], shape = p_fit$estimate["shape"])
    v <- pgev(y, loc = h_fit$estimate["loc"], scale = h_fit$estimate["scale"], shape = h_fit$estimate["shape"])
    copula::pCopula(cbind(u, v), copula_models[[1]]@copula)  # Correct S4 object handling
  }))
  
  # Create the 3D plot
  fig <- plotly::plot_ly(x = x_seq, y = y_seq, z = z, type = "surface", colorscale = "Viridis") %>%
    layout(title = "3D Joint Probability Surface Plot",
           scene = list(
             xaxis = list(title = 'Precipitation (mm)'),
             yaxis = list(title = 'Tide Level (m)'),
             zaxis = list(title = 'Joint Probability')
           ))
  
  # Add AND points
  fig <- fig %>%
    add_markers(data = points_df,
                x = ~P_AND, y = ~H_AND, z = ~0, # Set z to 0 for visibility in the plot
                marker = list(color = 'red', size = 5, opacity = 0.8),
                name = "AND Points")
  
  # Add OR points
  fig <- fig %>%
    add_markers(data = points_df,
                x = ~P_OR, y = ~H_OR, z = ~0, # Set z to 0 for visibility in the plot
                marker = list(color = 'blue', size = 5, opacity = 0.8),
                name = "OR Points")
  
  return(fig)
}






# ========================================================
# Function for Copula density
copula_density <- function(x, y, copula_model, data) {
  # Fit distributions to data
  p_fit <- evd::fgev(data$H)
  h_fit <- evd::fgev(data$H)
  
  # Compute CDF values for x and y based on fitted distributions
  u <- cbind(evd::pgev(x, loc = p_fit$estimate["loc"], scale = p_fit$estimate["scale"], shape = p_fit$estimate["shape"]), 
             evd::pgev(y, loc = h_fit$estimate["loc"], scale = h_fit$estimate["scale"], shape = h_fit$estimate["shape"]))
  
  # Compute the copula density
  copula::dCopula(u, copula_model@copula)
}
# ========================================================



# ========================================================
# Function to plot 3D 
plot_3d_surface <- function(copula_model, data, return_periods, model_name, joint_return_periods) {
  # Check that P and H are numeric
  if (!is.numeric(data$P) || !is.numeric(data$H)) {
    stop("Precipitation (P) and Tide Level (H) columns must be numeric.")
  }
  
  # Convert to numeric if necessary
  data$P <- as.numeric(data$P)
  data$H <- as.numeric(data$H)
  
  # Check for NA values after conversion
  if (any(is.na(data$P)) || any(is.na(data$H))) {
    stop("Conversion to numeric introduced NA values. Check your data.")
  }
  
  rainfall_max <- max(data$P, na.rm = TRUE)
  tide_max <- max(data$H, na.rm = TRUE)
  
  for (rp in return_periods) {
    x_seq <- seq(0, rainfall_max, length.out = 50)
    y_seq <- seq(0, tide_max, length.out = 50)
    
    u_seq <- x_seq / rainfall_max
    v_seq <- y_seq / tide_max
    
    uv_grid <- expand.grid(u_seq, v_seq)
    colnames(uv_grid) <- c("u", "v")
    
    uv_grid$u <- pmin(pmax(uv_grid$u, 0.001), 0.999)
    uv_grid$v <- pmin(pmax(uv_grid$v, 0.001), 0.999)
    
    # Check that uv_grid$u and uv_grid$v are numeric
    if (!is.numeric(uv_grid$u) || !is.numeric(uv_grid$v)) {
      stop("uv_grid must contain numeric values.")
    }
    
    z <- matrix(copula::pCopula(as.matrix(uv_grid), copula_model@copula), nrow = length(u_seq), ncol = length(v_seq))
    
    fig <- plotly::plot_ly(x = x_seq, y = y_seq, z = z, contours = list(z = list(show = TRUE))) %>%
      add_surface(colorscale = "Picnic") %>%
      layout(title = paste(model_name, "Joint Probability Return Period:", rp, "years"),
             scene = list(xaxis = list(title = 'Precipitation (mm)'),
                          yaxis = list(title = 'Tide Level (m)'),
                          zaxis = list(title = 'Joint Probability'))
      )
    
    # Add AND points (if available)
    if (!is.null(joint_return_periods[[as.character(rp)]]$AND)) {
      and_data <- joint_return_periods[[as.character(rp)]]$AND
      and_z <- mapply(function(p, h) {
        u <- pmin(pmax((p / rainfall_max), 0.001), 0.999)
        v <- pmin(pmax((h / tide_max), 0.001), 0.999)
        copula_density(u, v, copula_model, data)
      }, and_data$P, and_data$H)
      
      fig <- fig %>%
        add_markers(x = and_data$P, y = and_data$H, z = and_z,
                    marker = list(color = 'red', size = 7),
                    name = "AND",
                    text = paste("P = ", round(and_data$P, 2), "mm<br>H = ", round(and_data$H, 2), "m<br>Prob = ", round(and_z, 2)),
                    textposition = "top center",
                    textfont = list(size = 12, color = 'black'))
    }
    
    # Add OR points (if available)
    if (!is.null(joint_return_periods[[as.character(rp)]]$OR)) {
      or_data <- joint_return_periods[[as.character(rp)]]$OR
      or_z <- mapply(function(p, h) {
        u <- pmin(pmax((p / rainfall_max), 0.001), 0.999)
        v <- pmin(pmax((h / tide_max), 0.001), 0.999)
        P_u <- pCopula(cbind(u, 0), copula_model@copula)
        P_v <- pCopula(cbind(0, v), copula_model@copula)
        P_uv <- pCopula(cbind(u, v), copula_model@copula)
        P_or <- P_u + P_v - P_uv
        return(P_or)
      }, or_data$P, or_data$H)
      
      fig <- fig %>%
        add_markers(x = or_data$P, y = or_data$H, z = or_z,
                    marker = list(color = 'blue', size = 7),
                    name = "OR",
                    text = paste("P = ", round(or_data$P, 2), "mm<br>H = ", round(or_data$H, 2), "m<br>Prob = ", round(or_z, 2)),
                    textposition = "top center",
                    textfont = list(size = 12, color = 'black'))
    }
    
    return(fig)
  }
}







# ========================================================


# ========================================================
# Function to plot contours 1
plot_contours_separated <- function(copula_model, data, return_periods, model_name, joint_return_periods) {
  # Define scaling factors
  tide_max <- max(data$H)
  rainfall_max <- max(data$P)
  
  # Define colors and symbols for different return periods
  colors <- c("indianred1", "turquoise3", "palegreen3", "lightslateblue")
  symbols <- c(16, 17, 18, 15)  # Different point shapes
  
  par(mfrow = c(1, 2))

  # Plot for AND scenario
  plot(NULL, xlim = c (min(data$P) , max(data$P)+50), 
       ylim = c(min(data$H), max(data$H) + 2), 
       xlab = "Precipitation (mm)", ylab = "Tide Level (m)",
       main = paste(model_name, "AND Scenario"))
  
  # Iterate over return periods to add contours and points
  for (i in seq_along(return_periods)) {
    rp <- return_periods[i]
    
    # Create a grid for x and y
    x_seq <- seq(0, rainfall_max, length.out = 100)  
    y_seq <- seq(0, tide_max, length.out = 100)     
    
    # Normalize the data to [0, 1] range for copula CDF calculation
    u_seq <- x_seq / rainfall_max
    v_seq <- y_seq / tide_max
    
    # Create a grid of u, v pairs
    uv_grid <- expand.grid(u_seq, v_seq)
    colnames(uv_grid) <- c("u", "v")
    
    # Clip values to avoid extreme probabilities
    uv_grid$u <- pmin(pmax(uv_grid$u, 0.001), 0.999)
    uv_grid$v <- pmin(pmax(uv_grid$v, 0.001), 0.999)
    
    # Compute copula CDF over the grid
    z_and <- matrix(copula::pCopula(as.matrix(uv_grid), copula_model@copula), 
                nrow = length(u_seq), 
                ncol = length(v_seq))
    
    # Add contours with probability values
    contour(x_seq, y_seq, z_and, add = TRUE, col = "darkgray", lwd = 1, labcex = 1, nlevels = 8)
  
    
    # Add AND points
    if (!is.null(joint_return_periods[[as.character(rp)]]$AND)) {
      points(joint_return_periods[[as.character(rp)]]$AND$P, 
             joint_return_periods[[as.character(rp)]]$AND$H, 
             col = colors[i], pch = symbols[i], cex = 1.5)
      # Add text labels beside points
      text(joint_return_periods[[as.character(rp)]]$AND$P, 
           joint_return_periods[[as.character(rp)]]$AND$H, 
           labels = paste("(", round(joint_return_periods[[as.character(rp)]]$AND$P, 1), ", ", 
                          round(joint_return_periods[[as.character(rp)]]$AND$H, 1), ")", sep = ""),
           pos = 4, cex = 0.8)
    }
  }

  legend("topleft", legend = paste("RP:", return_periods), col = colors, pch = symbols, title = "Return Period")
  
  # Plot for OR scenario
  plot(NULL,xlim = c (min(data$P) , max(data$P)+50), 
       ylim = c(min(data$H), max(data$H)+2), 
       xlab = "Precipitation (mm)", ylab = "Tide Level (m)",
       main = paste(model_name, "OR Scenario"))
  
  # Iterate over return periods to add contours and points
  for (i in seq_along(return_periods)) {
    rp <- return_periods[i]
    
    # Create a grid for x and y
    x_seq <- seq(0, rainfall_max, length.out = 100)  
    y_seq <- seq(0, tide_max, length.out = 100)    
    
    # Normalize the data to [0, 1] range for copula CDF calculation
    u_seq <- x_seq / rainfall_max
    v_seq <- y_seq / tide_max
    
    # Create a grid of u, v pairs
    uv_grid <- expand.grid(u_seq, v_seq)
    colnames(uv_grid) <- c("u", "v")
    
    # Clip values to avoid extreme probabilities
    uv_grid$u <- pmin(pmax(uv_grid$u, 0.001), 0.999)
    uv_grid$v <- pmin(pmax(uv_grid$v, 0.001), 0.999)
    
    # Compute copula CDF over the grid
    # z <- matrix(pCopula(as.matrix(uv_grid), copula_model@copula), 
    #             nrow = length(u_seq), 
    #             ncol = length(v_seq))
    
    z_or <- outer(u_seq, v_seq, function(u, v) u + v) - z_and
    
    # Add contours with probability values
    contour(x_seq, y_seq, z_or, add = TRUE, col = "darkgray", labcex = 1, lwd = 1,  nlevels = 8)
    
    # Add OR points
    if (!is.null(joint_return_periods[[as.character(rp)]]$OR)) {
      points(joint_return_periods[[as.character(rp)]]$OR$P, 
             joint_return_periods[[as.character(rp)]]$OR$H, 
             col = colors[i], pch = symbols[i], cex = 1.5)
      # Add text labels beside points
      text(joint_return_periods[[as.character(rp)]]$OR$P, 
           joint_return_periods[[as.character(rp)]]$OR$H, 
           labels = paste("(", round(joint_return_periods[[as.character(rp)]]$OR$P, 1), ", ", 
                          round(joint_return_periods[[as.character(rp)]]$OR$H, 1), ")", sep = ""),
           pos = 4, cex = 0.8)
    }
  }
  
  legend("topleft", legend = paste("RP:", return_periods), col = colors, pch = symbols, title = "Return Period")
  
  # Reset plotting layout
  par(mfrow = c(1, 1))
}
# ========================================================



# ========================================================
# Function to plot contours 2
contour_rp_plot <- function(data, copulaModel, copulaName, rp) {
  # Fit the marginal distributions
  p_fit <- evd::fgev(data$P)
  h_fit <- evd::fgev(data$H)
  
  # Extract the parameters from the fitted distributions
  loc_P <- p_fit$estimate["loc"]
  scale_P <- p_fit$estimate["scale"]
  shape_P <- p_fit$estimate["shape"]
  
  loc_H <- h_fit$estimate["loc"]
  scale_H <- h_fit$estimate["scale"]
  shape_H <- h_fit$estimate["shape"]
  
  # Define the marginal CDFs
  marginal_P_cdf <- function(x)  pgev(x, loc = loc_P, scale = scale_P, shape = shape_P)
  marginal_H_cdf <- function(x)  pgev(x, loc = loc_H, scale = scale_H, shape = shape_H)
  
  # Fit with Copulas
  fitted_copula <- copula::fitCopula(copulaModel,  data = pobs(data),  method = "ml")
  
  # Extract the copula object from the fitted model
  copula_model <- fitted_copula@copula
  
  # Define grid of Rainfall (P) and Storm Tides (H) values
  P_values <- seq(min(data$P), max(data$P)+50, length.out = 100)
  H_values <- seq(min(data$H), max(data$H)+0.5, length.out = 100)
  
  # Initialize matrices to store return period values
  RP_matrix_AND <- matrix(NA, nrow = length(P_values), ncol = length(H_values))
  RP_matrix_OR <- matrix(NA, nrow = length(P_values), ncol = length(H_values))
  
  # Iterate over all combinations of P and H
  for (i in seq_along(P_values)) {
    for (j in seq_along(H_values)) {
      P <- P_values[i]
      H <- H_values[j]
      
      # Calculate marginal CDF values
      u1 <- 1-marginal_P_cdf(P)
      u2 <- 1-marginal_H_cdf(H)
      
      # Calculate joint exceedance probabilities using copula
      joint_ep_and <- copula::pCopula(c(u1, u2), copula_model) # AND Condition 
      joint_ep_or <- u1 + u2 - joint_ep_and           # OR Condition 
      
      # Convert probabilities to return periods
      RP_matrix_AND[i, j] <- ifelse(joint_ep_and > 0, 1 / joint_ep_and, NA)
      RP_matrix_OR[i, j] <- ifelse(joint_ep_or > 0, 1 / joint_ep_or, NA)
      
    }
  }
  
  # Define return periods and their corresponding thresholds
  thresholds <- list(AND = rp, OR = rp)
  
  # Plot the first contour plot (AND case)
  par(mfrow = c(2, 1))
  
  # First plot (AND case)
  plot(data$P, data$H, col = "gray", pch = 2, 
       xlim = range(P_values), ylim = range(H_values),
       xlab = "Rainfall (mm)", ylab = "Storm tides (m)", 
       main = paste("AND using", copulaName, "model"))
  
  contour(P_values, H_values, RP_matrix_AND, levels = thresholds$AND,
          col = "royalblue3", lty = 1, lwd = 2, labcex = 1, add = TRUE)
  
  legend("topright", legend = paste("AND Condition: ", rp, " years"),
         col = c("royalblue3"), lty = 1, lwd = 2)
  
  # Second plot (OR case)
  plot(
    data$P, data$H,
    col = "gray", pch = 2, 
    xlim = range(P_values),
    ylim = range(H_values),
    xlab = "Rainfall (mm)",
    ylab = "Storm tides (m)",
    main = paste("OR using", copulaName, "model")
  )
  
  contour(
    P_values,
    H_values,
    RP_matrix_OR,
    levels = thresholds$OR,
    col = "violetred3",
    lty = 1,
    lwd = 2,
    labcex = 1,
    add = TRUE
  )
  
  legend(
    "topright",
    legend = paste("OR Condition: ", rp," years"),
    col = c("violetred3"),
    lty = 1,
    lwd = 2
  )
  
}
# ========================================================


#Function to calculate Risk Probability
# ========================================================
compute_failure_probabilities <- function(joint_return_periods, design_life_years, return_periods, copula_models) {
  df <- data.frame()
  
  for (rp in return_periods) {
    for (copula_name in names(joint_return_periods)) {
      # Access the actual copula model 
      copula_model <- copula_models[[copula_name]]  
      
      rp_values <- joint_return_periods[[copula_name]][[as.character(rp)]]
      
      for (design_life in design_life_years) {
        
        rp_and_P <- as.numeric(rp_values$AND$P)
        rp_and_H <- as.numeric(rp_values$AND$H)
        rp_or_P <- as.numeric(rp_values$OR$P)
        rp_or_H <- as.numeric(rp_values$OR$H)
        
        #  Scaled data
        u_and <- pmin(pmax(rp_and_P / max(data$P), 0.001), 0.999)
        v_and <- pmin(pmax(rp_and_H / max(data$H), 0.001), 0.999)
        u_or <- pmin(pmax(rp_or_P / max(data$P), 0.001), 0.999)
        v_or <- pmin(pmax(rp_or_H / max(data$H), 0.001), 0.999)
        
        # Calculate joint probabilities
        joint_probability_and <- copula::pCopula(c(u_and, v_and), copula_model)
        joint_probability_or <- copula::pCopula(c(u_or, v_or), copula_model)
        
        # Calculate failure probabilities
        # https://doi.org/10.1007/s11269-022-03321-y
        fp_and <- 1 - (1 - joint_probability_and)^design_life
        fp_or <- 1 - (1 - joint_probability_or)^design_life
        
        # Ensure failure probabilities are within [0, 100%]
        fp_and <- pmin(pmax(fp_and * 100, 0), 100)
        fp_or <- pmin(pmax(fp_or * 100, 0), 100)
        
        # Append the results to the dataframe
        df <- rbind(df, data.frame(
          Copula = copula_name,
          Return_Period = rp,
          Design_Life = design_life,
          Scenario = c("AND", "OR"),
          Failure_Probability = c(fp_and, fp_or)
        ))
      }
    }
  }
  
  return(df)
}
# ========================================================

# Function to plot Risk Probability
plot_failure_probabilities <- function(df) {
  ggplot(df, aes(x = Design_Life, y = Failure_Probability, color = factor(Return_Period), linetype = Scenario)) +
 
    geom_line(linewidth = 1) +
    labs(
      x = "Design Life (Years)",
      y = "Failure Probability (%)",
      color = "Return Period (Years)",
      linetype = "Scenario",
      title = "Failure Probability"
    ) +
    theme_minimal(base_size =  20) +
    scale_color_manual(values = c("10" = "steelblue3", "25" = "darkolivegreen4", "50" = "darkgoldenrod3", "100" = "violetred3")) +
    scale_linetype_manual(values = c("AND" = "solid", "OR" = "dashed")) +
    facet_wrap(~ Copula, scales = "free_y") +
    theme(legend.position = "bottom")
  
}

# ========================================================

# Function to compute failure probabilities with standard deviation and confidence intervals
compute_failure_probabilities_with_ci <- function(joint_return_periods, design_life_years, return_periods, copula_models) {
  df <- data.frame()
  
  for (rp in return_periods) {
    for (copula_name in names(joint_return_periods)) {
      copula_model <- copula_models[[copula_name]]  
      rp_values <- joint_return_periods[[copula_name]][[as.character(rp)]]
      
      for (design_life in design_life_years) {
        
        # Scaled data for AND and OR
        u_and <- pmin(pmax(rp_values$AND$P / max(data$P), 0.001), 0.999)
        v_and <- pmin(pmax(rp_values$AND$H / max(data$H), 0.001), 0.999)
        u_or  <- pmin(pmax(rp_values$OR$P / max(data$P), 0.001), 0.999)
        v_or  <- pmin(pmax(rp_values$OR$H / max(data$H), 0.001), 0.999)
        
        # Calculate joint probabilities
        joint_probability_and <- copula::pCopula(c(u_and, v_and), copula_model)
        joint_probability_or  <- copula::pCopula(c(u_or, v_or), copula_model)
        
        # Calculate failure probabilities
        fp_and <- 1 - (1 - joint_probability_and)^design_life
        fp_or  <- 1 - (1 - joint_probability_or)^design_life
        
        # Convert to percentage
        fp_and <- fp_and * 100
        fp_or  <- fp_or * 100
        
        # Calculate standard deviation based on u_and and u_or
        sd_and <- sd(fp_and) / sqrt(length(fp_and)) * 100
        sd_or  <- sd(fp_or) / sqrt(length(fp_or)) * 100
        
        # Calculate 95% confidence intervals using standard deviation
        ci_and_lower <- fp_and - 1.96 * sd_and
        ci_and_upper <- fp_and + 1.96 * sd_and
        ci_or_lower  <- fp_or  - 1.96 * sd_or
        ci_or_upper  <- fp_or  + 1.96 * sd_or
        
        # Append results to the dataframe
        df <- rbind(df, data.frame(
          Copula = copula_name,
          Return_Period = rp,
          Design_Life = design_life,
          Scenario = "AND",
          Failure_Probability = fp_and,
          Failure_Probability_Lower = max(ci_and_lower, 0),
          Failure_Probability_Upper = min(ci_and_upper, 100)
        ))
        
        df <- rbind(df, data.frame(
          Copula = copula_name,
          Return_Period = rp,
          Design_Life = design_life,
          Scenario = "OR",
          Failure_Probability = fp_or,
          Failure_Probability_Lower = max(ci_or_lower, 0),
          Failure_Probability_Upper = min(ci_or_upper, 100)
        ))
      }
    }
  }
  
  return(df)
}


plot_failure_probabilities_with_ci <- function(df) {
  ggplot(df, aes(x = Design_Life, y = Failure_Probability, color = factor(Return_Period), linetype = Scenario)) +
     geom_ribbon(aes(ymin = Failure_Probability_Lower, ymax = Failure_Probability_Upper, fill = factor(Return_Period)), alpha = 0.1) +
    geom_line(linewidth = 1) +
    labs(
      x = "Design Life (Years)",
      y = "Failure Probability (%)",
      color = "Return Period (Years)",
      linetype = "Scenario",
      fill = "Return Period (Years)",
      title = "Failure Probability with Confidence Limits"
    ) +
    theme_minimal(base_size =  20) +
    scale_color_manual(values = c("10" = "steelblue3", "25" = "darkolivegreen4", "50" = "darkgoldenrod3", "100" = "violetred3")) +
    scale_fill_manual(values = c("10" = "steelblue3", "25" = "darkolivegreen4", "50" = "darkgoldenrod3", "100" = "violetred3")) +
    scale_linetype_manual(values = c("AND" = "solid", "OR" = "dashed")) +
    facet_wrap(~ Copula, scales = "free_y") +
    theme(legend.position = "bottom")
}

