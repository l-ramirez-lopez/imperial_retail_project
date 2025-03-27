# load modules
library(nnet)
library(dplyr)
library(lubridate)
library(caret)

setwd("/home/mcaluya/bao/rma/Project/demand_forecast")

train_data <- readRDS("data/train_data.rds")
test_data <- readRDS("data/test_data.rds")
unscale <- readRDS("model/unscale.rds")

nn_model <- readRDS("model/nn_model.rds")
results_df <- readRDS("data/results_df.rds")

# current forecast function will run for all skus if horizon will be changed

# default forecast window to 4 weeks
generate_forecast <- function(forecast_horizon=4) {
  # weeks ahead to forecast
  #forecast_horizon <- 4
  
  # create holder for consolidated forecasts
  all_forecasts <- data.frame()
  
  # unique sku listing
  sku_list <- unique(test_data$sku)
  # for reference
  train_cols <- colnames(train_data)
  
  # function to update dummy vars
  update_time_dummies <- function(df, date) {
    # reinitialise all to 0
    df[ , grep("^month_", names(df))] <- 0
    df[ , grep("^weekno_", names(df))] <- 0
    
    # define based on current values
    month_col <- paste0("month_", month(date))
    weekno_col <- paste0("weekno_", week(date))
    
    # reassign
    if (month_col %in% names(df)) df[[month_col]] <- 1
    if (weekno_col %in% names(df)) df[[weekno_col]] <- 1
    
    return(df)
  }
  
  # forecast for each sku then append
  for (sku_id in sku_list) {
    # filter for current sku and get latest record
    last_data <- test_data %>% filter(sku == sku_id) %>% tail(1)
    
    # generate sequence of future weeks based on max week
    forecasts_scaled <- numeric(forecast_horizon)
    forecast_weeks <- seq(max(last_data$week) + 7, by = 7, length.out = forecast_horizon)
    
    for (i in 1:forecast_horizon) {
      if (i == 1) {
        # for first future week, use latest week as input
        last_data_fixed <- as.data.frame(matrix(0, nrow = 1, ncol = length(train_cols)))
        colnames(last_data_fixed) <- train_cols
        common_cols <- intersect(colnames(last_data), train_cols)
        last_data_fixed[1, common_cols] <- last_data[1, common_cols]
        
        # forecast
        pred_scaled <- predict(nn_model, newdata = last_data_fixed)
        
      }
      else {
        # for succeeding future weeks
        forecast_date <- forecast_weeks[i]
        last_data$week <- forecast_date
        last_data <- update_time_dummies(last_data, forecast_date)
        
        # build up row input
        last_data_fixed <- as.data.frame(matrix(0, nrow = 1, ncol = length(train_cols)))
        colnames(last_data_fixed) <- train_cols
        common_cols <- intersect(colnames(last_data), train_cols)
        last_data_fixed[1, common_cols] <- last_data[1, common_cols]
        
        # forecast
        pred_scaled <- predict(nn_model, newdata = last_data_fixed)
      }
      
      # store forecast
      forecasts_scaled[i] <- pred_scaled
      
      # update values in prep for next future week
      last_data$log_weekly_sales.2 <- last_data$log_weekly_sales.1
      last_data$log_weekly_sales.1 <- pred_scaled
      last_data$sales_delta <- expm1(last_data$log_weekly_sales.1) / expm1(last_data$log_weekly_sales.2) - 1
      last_data$price.2 <- last_data$price.1
      last_data$price.1 <- last_data$price
      last_data$price_delta <- last_data$price / last_data$price.1 - 1
    }
    
    # function to unscale
    weekly_sales_min <- unscale$log_sales_min
    weekly_sales_max <- unscale$log_sales_max
    unscale_sales <- function(x) x * (weekly_sales_max - weekly_sales_min) + weekly_sales_min
    
    # store all forecast for current sku
    forecast_df <- data.frame(
      week = forecast_weeks,
      sku = sku_id,
      forecast = round(expm1(unscale_sales(forecasts_scaled)))
    )
    
    # append to consolidated forecasts
    all_forecasts <- bind_rows(all_forecasts, forecast_df)
  }
  
  return(all_forecasts)
}

# add confidence interval with level adjustable
add_confidence_interval <- function(forecast_df, results_df, level = 0.95) {
  forecast_df <- as.data.frame(forecast_df)
  results_df <- as.data.frame(results_df)
  
  z <- qnorm((1 + level) / 2)
  
  residuals_df <- results_df %>%
    dplyr::mutate(residual = actual - predicted) %>%
    group_by(sku) %>%
    summarise(sd_res = sd(residual, na.rm = TRUE), .groups = "drop")
  
  forecast_df <- forecast_df %>%
    left_join(residuals_df, by = "sku") %>%
    mutate(
      lower = round(pmax(forecast - z * sd_res, 0)),
      upper = round(forecast + z * sd_res)
    ) %>%
    select(-sd_res)
  
  return(forecast_df)
}

all_forecasts <- add_confidence_interval(generate_forecast(), results_df) 

saveRDS(all_forecasts, "output/forecasts.rds")
