# load modules
library(nnet)
library(dplyr)
library(lubridate)
library(caret)
library(neuralnet)
setwd("/home/mcaluya/bao/rma/Project/demand_forecast")

# set feature vars
feature_vars <- c("log_weekly_sales.1", "log_weekly_sales.2", "price", "price.1", "price.2", "sales_delta", "price_delta", "feat_main_page", "trend",
                  grep("^month_|^functionality_|^color_|^vendor_|^sku_|^weekno_", names(data), value = TRUE))
all_vars <- c("log_weekly_sales", feature_vars)

train_data <- readRDS("data/train_data.rds")
test_data <- readRDS("data/test_data.rds")
unscale <- readRDS("model/unscale.rds")

# build formula
nn_formula <- as.formula(paste("log_weekly_sales ~", paste(feature_vars, collapse = " + ")))

# fit neural net
nn_model <- nnet(nn_formula,data = train_data, size = 3, linout = TRUE, decay=0.01, maxit=150)

library(neuralnet)

# train_data[is.na(train_data)] <- 0
# nn_model <- neuralnet(
#   nn_formula,
#   data = train_data,
#   hidden = 3,
#   linear.output = TRUE,
#   err.fct = 'sse',
#   #act.fct = 'tanh',
#   stepmax=1e5
# )

# grid using caret - fluctuating too much
# nn_model <- train(
#   nn_formula,
#   data = train_data,
#   method = "nnet",
#   linout = TRUE,
#   tuneGrid = expand.grid(size = c(2, 3, 4, 5), decay = c(0.001, 0.01, 0.1)),
#   trControl = trainControl(method = "cv", number = 5),
#   maxit = 150,
#   trace = FALSE,
#   na.action = na.omit
# )
# print(nn_model$bestTune)


# function to unscale
weekly_sales_min <- unscale$log_sales_min
weekly_sales_max <- unscale$log_sales_max
unscale_sales <- function(x) x * (weekly_sales_max - weekly_sales_min) + weekly_sales_min

# predict on test, unscale then inverse log
nn_predictions_scaled <- predict(nn_model, newdata = test_data) #nnet
#nn_predictions_scaled <- compute(nn_model, test_data)$net.result #neuralnet
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

saveRDS(nn_model, "model/nn_model.rds")
saveRDS(results_df, "data/results_df.rds")