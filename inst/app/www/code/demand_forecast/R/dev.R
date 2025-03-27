# load modules
library(nnet)
library(dplyr)
library(lubridate)
library(caret)

# load data
setwd("/home/mcaluya/bao/rma/Project")

data("data_processed")

data <- datap # read.csv("data_processed.csv")




# data prep/feature eng

# sort by sku, week
data$week <- as.Date(data$week)
data <- data[order(data$sku, data$week), ]
data$weekno <- week(data$week)

# log sales
data$log_weekly_sales <- log1p(data$weekly_sales)

# create lag features
data <- data %>%
  group_by(sku) %>%
  mutate(weekly_sales.1 = lag(weekly_sales,1),
         weekly_sales.2 = lag(weekly_sales,2)) %>%
  ungroup() %>%
  na.omit()

# create delta for sales and price
data$sales_delta <- data$weekly_sales / data$weekly_sales.1 - 1
data$price_delta <- data$price / data$price.1 - 1

#data[is.na(data)] <- 0

# create lag for logged sales
data$log_weekly_sales.1 <- log1p(data$weekly_sales.1)
data$log_weekly_sales.2 <- log1p(data$weekly_sales.2)

# create dummy vars for sku and weekno
sku_dummies <- model.matrix(~ factor(sku) - 1, data)
colnames(sku_dummies) <- gsub("factor\\(sku\\)", "sku_", colnames(sku_dummies))
data <- cbind(data, sku_dummies)

weekno_dummies <- model.matrix(~ factor(weekno) - 1, data)
colnames(weekno_dummies) <- gsub("factor\\(weekno\\)", "weekno_", colnames(weekno_dummies))
data <- cbind(data, weekno_dummies)

# pre-process

# set feature vars
feature_vars <- c("log_weekly_sales.1", "log_weekly_sales.2", "price", "price.1", "price.2", "sales_delta", "price_delta", "feat_main_page", "trend",
                  grep("^month_|^functionality_|^color_|^vendor_|^sku_|^weekno_", names(data), value = TRUE))
all_vars <- c("log_weekly_sales", feature_vars)

# scale vars
preproc <- preProcess(data[, all_vars], method = "range")
scaled_data <- predict(preproc, data[, all_vars])

# keep as is for reference
scaled_data$week <- data$week
scaled_data$sku <- data$sku

# train test split - use latest 4 weeks as test
wks <- 4
train_data <- scaled_data %>%
  group_by(sku) %>%
  filter(row_number() <= n() - wks) %>%
  ungroup()

test_data <- scaled_data %>%
  group_by(sku) %>%
  filter(row_number() > n() - wks) %>%
  ungroup()

# build formula
nn_formula <- as.formula(paste("log_weekly_sales ~", paste(feature_vars, collapse = " + ")))

# fit neural net
nn_model <- nnet(nn_formula,data = train_data, size = 3, linout = TRUE, decay=0.01, maxit=150)

# grid using caret - fluctuating too much
# nn_model <- train(
#   nn_formula,
#   data = train_data,
#   method = "nnet",
#   linout = TRUE,
#   tuneGrid = expand.grid(size = c(2, 3, 4, 5), decay = c(0.001, 0.01, 0.1)),
#   trControl = trainControl(method = "cv", number = 5),
#   maxit = 200,
#   trace = FALSE,
#   na.action = na.omit
# )
# print(nn_model$bestTune)


# function to unscale
weekly_sales_min <- min(data$log_weekly_sales)
weekly_sales_max <- max(data$log_weekly_sales)
unscale_sales <- function(x) x * (weekly_sales_max - weekly_sales_min) + weekly_sales_min

# predict on test, unscale then inverse log
nn_predictions_scaled <- predict(nn_model, newdata = test_data)
actual_sales <- expm1(unscale_sales(test_data$log_weekly_sales))
nn_predictions <- round(expm1(unscale_sales(nn_predictions_scaled)))

# eval metrics
rmse_nn <- sqrt(mean((actual_sales - nn_predictions)^2))
mae_nn <- mean(abs(actual_sales - nn_predictions))
non_zero_mask <- actual_sales != 0
mape_nn <- mean(abs((actual_sales[non_zero_mask] - nn_predictions[non_zero_mask]) /
                      actual_sales[non_zero_mask])) * 100

ss_res <- sum((actual_sales - nn_predictions)^2)
ss_tot <- sum((actual_sales - mean(actual_sales))^2)
r_squared <- 1 - (ss_res / ss_tot)

cat("R-squared:", round(r_squared, 4), "\n")
cat("RMSE:", round(rmse_nn, 2), "\n")
cat("MAE:", round(mae_nn, 2), "\n")
cat("MAPE:", round(mape_nn, 2), "%\n")

# store for reference
results_df <- data.frame(
  week = test_data$week,
  sku = test_data$sku,
  actual = actual_sales,
  predicted = nn_predictions
)


# forecast

# weeks ahead to forecast
forecast_horizon <- 4
all_forecasts <- data.frame()

tmp <- data.frame()
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
  
  # loop for each future week
  # for (i in 1:forecast_horizon) {
  #   
  #   # construct future week variables
  #   forecast_date <- forecast_weeks[i]
  #   last_data$week <- forecast_date
  #   # call function to update dummy vars
  #   last_data <- update_time_dummies(last_data, forecast_date)
  #   
  #   # create blank row based on model data to mitigate missing vars
  #   last_data_fixed <- as.data.frame(matrix(0, nrow = 1, ncol = length(train_cols)))
  #   colnames(last_data_fixed) <- train_cols
  #   common_cols <- intersect(colnames(last_data), train_cols)
  #   last_data_fixed[1, common_cols] <- last_data[1, common_cols]
  #   
  #   # predict using trained model
  #   pred_scaled <- predict(nn_model, newdata = last_data_fixed)
  #   forecasts_scaled[i] <- pred_scaled
  #   
  #   # update lags and deltas
  #   last_data$log_weekly_sales.2 <- last_data$log_weekly_sales.1
  #   last_data$log_weekly_sales.1 <- pred_scaled
  #   last_data$sales_delta <- expm1(last_data$log_weekly_sales.1) / expm1(last_data$log_weekly_sales.2) - 1
  #   last_data$price.2 <- last_data$price.1
  #   last_data$price.1 <- last_data$price
  #   last_data$price_delta <- last_data$price / last_data$price.1 - 1
  # }
  
  # store all forecast for current sku
  forecast_df <- data.frame(
    week = forecast_weeks,
    sku = sku_id,
    forecast = round(expm1(unscale_sales(forecasts_scaled)))
  )
  
  # append to consolidated forecasts
  all_forecasts <- bind_rows(all_forecasts, forecast_df)
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
      lower = pmax(forecast - z * sd_res, 0),
      upper = forecast + z * sd_res
    ) %>%
    select(-sd_res)
  
  return(forecast_df)
}
# add confidence intervals to the forecast
all_forecasts_ci <- add_confidence_interval(all_forecasts, results_df)


# dataviz

plot_forecast_with_actuals(data, all_forecasts_ci, sku_ids = "all")

plot_forecast2(data, all_forecasts_ci, sku_ids = c("1","12","23","34","40"))

plot_forecast_with_actuals(data, all_forecasts_ci, sku_ids = c("1","12"), separate=T, aggregate_all = FALSE)

plot_forecast_with_actuals(data, all_forecasts_ci, sku_ids = c("1","12"), separate=TRUE, aggregate_all = FALSE)

plot_forecast_with_actuals(data, all_forecasts_ci, sku_ids = c("1","12"), separate=TRUE, aggregate_all = TRUE)

