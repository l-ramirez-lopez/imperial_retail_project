#' @docType data
#' @name all_forecasts_ci
#' @aliases all_forecasts_ci
#' @title Forecasts with Confidence Intervals for 44 SKUs
#' @keywords datasets
#' @description
#' This dataset contains weekly forecasts and associated 95% confidence intervals
#' for 44 different SKUs over a series of future weeks.
#'
#' Each row represents a forecasted value for a specific SKU in a given week.
#' The forecasts include the predicted sales value (`forecast`) and the associated
#' lower and upper bounds of the confidence interval.
#'
#' @format A data frame with one row per SKU-week combination and the following columns:
#' The `week` column is a Date object indicating the first day of the forecasted 
#' week. The `sku` column is an integer identifier for the SKU, ranging from 1 
#' to 44. The `forecast` column contains the predicted sales value for that SKU 
#' in the given week. The `lower` and `upper` columns represent the lower and 
#' upper bounds, respectively, of the 95% confidence interval for the forecast.
#' 
#' @details
#' Forecasts were generated using a time series model trained on historical weekly sales data.
#' The confidence intervals are assumed to reflect model uncertainty under normal operating conditions.
#'
#' This dataset is intended for use in evaluating forecast accuracy, visualising future trends,
#' and comparing SKU-level performance.
#'
#' @examples
#' head(all_forecasts_ci)
#'
#' # Plot forecast for SKU 1
#' library(ggplot2)
#' ggplot(subset(all_forecasts_ci, sku == 1), aes(x = week, y = forecast)) +
#'   geom_line() +
#'   geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
#'   labs(title = "Forecast with 95% CI for SKU 1", y = "Sales", x = "Week")
#'
#' @source Internal forecasting pipeline
NULL
