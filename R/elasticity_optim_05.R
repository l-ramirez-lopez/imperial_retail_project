#' @docType data
#' @name elasticity_optim_05
#' @aliases elasticity_optim_05
#' @title Elasticity Optimisation Results
#' @keywords datasets
#'
#' @description
#' These are the optimisation results computed by Seliyan using a adtock model with a 
#' lambda parameter of 0.5. They show the 
#' best-fit competitor price elasticity, own-price elasticity, and MAPE 
#' for each of 44 stock-keeping units (SKUs).
#'
#' @format A data frame with 44 rows and 4 columns:
#' \describe{
#'   \item{\code{sku}}{Integer. The SKU identifier.}
#'   \item{\code{competitor_elasticity}}{Numeric. The best-fit competitor price elasticity for the SKU.}
#'   \item{\code{elasticity}}{Numeric. The best-fit own-price elasticity for the SKU.}
#'   \item{\code{mape}}{Numeric. Mean absolute percentage error of the fitted model for that SKU.}
#' }
#'
#' @usage data(elasticity_optim_05)
#'
#' @examples
#' \dontrun{
#'   # After loading the package:
#'   data(elasticity_optim_05)
#'   head(elasticity_optim_05)
#' }
NULL
