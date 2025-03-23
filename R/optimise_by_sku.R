#' Parameter Optimization by SKU
#'
#' The \code{optimise_by_sku()} function loops through each SKU in a data frame,
#' sets initial guesses and bounds for parameters, then applies a constrained
#' optimization (\code{optim()}) that minimizes the mean absolute percentage error
#' (MAPE) from \code{\link{sales_optim}}. It returns a data frame of best-fitting
#' parameter values and MAPE for each SKU.
#'
#' @param df A data frame containing at least:
#'   \describe{
#'     \item{\code{sku}}{An identifier for each SKU.}
#'     \item{\code{price}}{Observed product price.}
#'     \item{\code{com_price}}{Competitor price.}
#'     \item{\code{month}}{Month of the year (1--12).}
#'     \item{\code{trend}}{Time index for the trend component.}
#'     \item{\code{Adstock}}{Advertising carryover effect (from \code{\link{adstock}}).}
#'     \item{\code{weekly_sales}}{Actual observed sales.}
#'   }
#'
#' @return A data frame with one row per SKU, containing:
#'   \itemize{
#'     \item \code{sku}: The SKU identifier.
#'     \item \code{competitor_elasticity}: Best-fit competitor price elasticity.
#'     \item \code{elasticity}: Best-fit own price elasticity.
#'     \item \code{mape}: The minimized mean absolute percentage error.
#'   }
#'
#' @details
#' The function initializes a table of parameter guesses and bounds, then for each SKU:
#' \enumerate{
#'   \item Updates the constant term using the SKU's mean weekly sales.
#'   \item Calls \code{optim(..., fn = sales_optim)} with L-BFGS-B bounds.
#'   \item Extracts and stores best-fit parameters and MAPE.
#' }
#'
#' @examples
#' \dontrun{
#' # Suppose df_w_competitor is a data frame with the required columns
#' results <- optimise_by_sku(df_w_competitor)
#' head(results)
#' }
#'
#' @export

optimise_by_sku <- function(df) {
  
  ini_params <- data.frame(
    matrix(
      NA, 17, 3, 
      dimnames = list(
        c("constant", 
          "trend", 
          "adstock_effect",  
          "competitor_price_elast",  
          "elasticity",
          paste0("month_", seq(1, 12))
        ), 
        c("initial_guess", "lower_bounds", "upper_bounds")
      )
    )
  )
  
  ini_params$initial_guess <- c(
    NA,  # to be upated later with the mean (reasonable constant)
    1,  # (must be stable)
    1,  
    1,  
    1,  
    rep(1, 12)  
  )
  ini_params$lower_bounds <- c(
    0, 0.8, 0, -10, -10, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
  )
  ini_params$upper_bounds <- c(
    Inf, 2, Inf, 10, 10, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2
  )
  
  results <- data.frame(
    matrix(
      NA, length(unique(df$sku)), 4, 
      dimnames = list(
        NULL, c("sku", "competitor_elasticity", "elasticity", "mape")
      )
    )
  )
  results$sku <- sort(unique(df$sku))
  for (i in sort(unique(df$sku))) {
    sku_df <- df[df$sku == i, ]
    cat(paste("Optimising SKU:", i, "\n"))
    
    # update the constant!
    ini_params["constant", "initial_guess"] <-  mean(
      sku_df$weekly_sales, na.rm = TRUE
    )
    
    # Run constrained optimization
    result <- optim(
      par = ini_params$initial_guess, 
      fn = sales_optim, 
      price = sku_df$price, 
      com_price = sku_df$com_price,
      monthofyear = sku_df$month, 
      year = sku_df$trend, 
      adstock = sku_df$Adstock, 
      sales = sku_df$weekly_sales, 
      method = "L-BFGS-B",
      lower = ini_params$lower_bounds, 
      upper = ini_params$upper_bounds
    )
    
    best_params <- result$par
    
    results$competitor_elasticity[results$sku == i] <- best_params[4]
    results$elasticity[results$sku == i] <- best_params[5]
    results$mape[results$sku == i] <- result$value
  }
  return(results)
}

