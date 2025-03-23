#' @docType data
#' @name data_w_competitor
#' @aliases data_w_competitor
#' @title Retail weekly sales with competitor columns
#' @keywords datasets
#' @description
#' This dataset builds upon the previously introduced retail dataset by adding
#' several new columns derived from the existing data. These additional fields
#' provide insights into competitor-related information and revenue calculations.
#'
#' As with the other retail datasets (e.g., \code{\link{data_raw}} and
#' \code{\link{data_processed}}), this data contains weekly sales for 44
#' stock-keeping units (SKUs) from a tech-gadget e-commerce retailer over
#' 100 weeks (October 2016 to September 2018). Thus, it includes 4,400 rows
#' in total. Beyond the original variables, it introduces:
#'
#' \describe{
#'   \item{\code{Color}}{Character or factor. Main color attribute of the SKU.}
#'   \item{\code{Vendor}}{Character or factor. Supplier or vendor of the SKU.}
#'   \item{\code{Revenue}}{Numeric. Calculated revenue for the SKU in a given week (e.g., \code{weekly_sales} × \code{price}).}
#'   \item{\code{com_price}}{Numeric. A hypothetical or observed competitor's price for comparison or analysis.}
#' }
#'
#' @format A data frame with 4,400 rows and at least these additional columns:
#'   \code{Color}, \code{Vendor}, \code{Revenue}, and \code{com_price}, along with
#'   the base columns from the raw/processed datasets.
#' @usage data(data_w_competitor)
#' @source Cohen, M. C., Gras, P. E., Pentecoste, A., & Zhang, R. (2022).
#'   \emph{Demand Prediction in Retail - A Practical Guide to Leverage Data and Predictive Analytics}.
#'   Springer Series in Supply Chain Management 14, 1–155.
NULL
