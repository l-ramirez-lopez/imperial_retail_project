# load modules
library(nnet)
library(dplyr)
library(lubridate)
library(caret)
setwd("/home/mcaluya/bao/rma/Project/demand_forecast")

# load data
data <- read.csv("data/data_processed.csv")

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

full_data <- data

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
train_test_split <- function(scaled_data, wks=4){
  train_data <- scaled_data %>%
    group_by(sku) %>%
    filter(row_number() <= n() - wks) %>%
    ungroup()
  
  test_data <- scaled_data %>%
    group_by(sku) %>%
    filter(row_number() > n() - wks) %>%
    ungroup()
  
  return(list(train = train_data, test = test_data))
}

split <- train_test_split(scaled_data)
train_data <- split$train
test_data <- split$test

# save for reuse
saveRDS(full_data,"data/full_data.rds")
saveRDS(train_data,"data/train_data.rds")
saveRDS(test_data,"data/test_data.rds")
saveRDS(preproc,"model/preproc.rds")

unscale <- list(
  log_sales_min = min(data$log_weekly_sales),
  log_sales_max = max(data$log_weekly_sales)
)
saveRDS(unscale,"model/unscale.rds")
