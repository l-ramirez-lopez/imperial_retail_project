#' @docType data
#' @name data_processed
#' @aliases data_processed
#' @title Retail weekly sales (processed dataset)
#' @keywords datasets
#' @description
#'
#' The concepts and demand prediction methods covered in the book
#' *Demand Prediction in Retail - A Practical Guide to Leverage Data and Predictive Analytics*
#' (Cohen, Gras, Pentecoste, & Zhang, 2022) are illustrated using this processed retail dataset.
#'
#' This dataset corresponds to the same tech-gadget e-commerce retailer data used in \code{data_raw},
#' but with various pre-processing steps applied, including the handling of missing data,
#' the addition of lag prices, and the transformation of certain features into dummy variables.
#' It covers 44 stock-keeping units (SKUs) over a period of 100 weeks (October 2016 to September 2018),
#' for a total of 4,400 rows.
#'
#' Below is an example of the typical columns you might expect:
#' \describe{
#'   \item{\code{week}}{Integer. Week identifier (1 to 100).}
#'   \item{\code{SKU}}{Character or factor. SKU identifier (44 unique SKUs).}
#'   \item{\code{weekly_sales}}{Numeric. Total weekly sales for the SKU in that week.}
#'   \item{\code{featured_on_main_page}}{Logical or factor. Indicates whether the SKU was featured on the main homepage (TRUE/FALSE).}
#'   \item{\code{price}}{Numeric. Unit price of the SKU. May be adjusted for missing data.}
#'   \item{\code{lag_price_1}, \code{lag_price_2}, ...}{Numeric. Example of lagged price features added during pre-processing.}
#'   \item{\code{color_red}, \code{color_blue}, ...}{Numeric or factor. Dummy variables created from the original \code{color} feature.}
#'   \item{\code{vendor}, \code{functionality}}{Character or factor. Supplier and product type categories (possibly recoded from raw data).}
#'   \item{\code{...}}{Other pre-processed features as relevant.}
#' }
#'
#' More details on this dataset and the pre-processing steps can be found in the referenced book.
#' The raw, unprocessed version of the dataset is available in \code{\link{data_raw}}.
#'
#' @format A data frame with 4,400 rows (100 weeks × 44 SKUs) and additional columns introduced by pre-processing steps.
#' @usage data(data_processed)
#' @source Cohen, M. C., Gras, P. E., Pentecoste, A., & Zhang, R. (2022).
#'   \emph{Demand Prediction in Retail - A Practical Guide to Leverage Data and Predictive Analytics}.
#'   Springer Series in Supply Chain Management 14, 1–155.
NULL
