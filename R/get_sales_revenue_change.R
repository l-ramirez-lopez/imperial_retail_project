#' Calculate Sales and Revenue Changes Due to Price Adjustment
#'
#' \code{get_sales_revenue_change} computes the impact of a percentage price change on 
#' an SKU's sales quantity and revenue, given elasticity parameters from prior optimization.
#'
#' @param sku An identifier (numeric or character) specifying which SKU to analyze.
#' @param price_change_pct A numeric value indicating the percentage price change 
#'   to be applied. For example, \code{5} corresponds to a 5\% price increase, 
#'   and \code{-10} corresponds to a 10\% price decrease.
#' @param sku_optimisation_results A data frame (e.g., the output of 
#'   \code{\link{optimise_by_sku}}) containing at least two columns: 
#'   \itemize{
#'     \item \code{sku}: SKU identifier (matching \code{sku} above).
#'     \item \code{elasticity}: Own-price elasticity value for each SKU.
#'     \item \code{competitor_elasticity}: Competitor-price elasticity for each SKU.
#'   }
#' @param df A data frame of historical data containing (at a minimum):
#'   \describe{
#'     \item{\code{sku}}{SKU identifier.}
#'     \item{\code{week}}{A date or date-like string representing the week.}
#'     \item{\code{price}}{Numeric, the current (observed) price for the SKU.}
#'     \item{\code{weekly_sales}}{Numeric, the observed weekly sales volume.}
#'   }
#'
#' @return A list with two major components:
#' \itemize{
#'   \item \code{results}: A data frame with one row containing:
#'     \describe{
#'       \item{\code{date}}{The date.}
#'       \item{\code{new_sales_qty}}{The new (predicted) sales volume after the price change.}
#'       \item{\code{old_revenue}}{Revenue before the price change (\code{current_sales * current_price}).}
#'       \item{\code{new_revenue}}{Predicted revenue after the price change.}
#'       \item{\code{sales_qty_change_pct}}{Percentage change in sales volume.}
#'       \item{\code{revenue_change_pct}}{Percentage change in revenue.}
#'       \item{\code{new_price}}{The new price (current price multiplied by \code{1 + price_change_pct/100}).}
#'     }
#'   \item \code{price_change_pct}: The input \code{price_change_pct}.
#'   \item \code{price_elasticity}: The SKU's own-price elasticity extracted from \code{sku_optimisation_results}.
#'   \item \code{competitor_price_elasticity}: The SKU's competitor price elasticity extracted 
#'     from \code{sku_optimisation_results}.
#' }
#'
#' @details
#' The function applies a basic own-price elasticity formula:
#' \deqn{
#'   \text{new\_quantity} = \text{old\_quantity} \times \left(\frac{\text{new\_price}}{\text{old\_price}}\right)^{-\text{elasticity}}
#' }
#' to estimate how sales volume changes in response to a price change. 
#' It then computes the new revenue (\emph{new sales} times \emph{new price}), 
#' and calculates the percentage changes relative to the original values.
#'
#'
#' @examples
#' \dontrun{
#' # Suppose you have a data frame 'df_w_competitor' with columns
#' # (sku, week, price, weekly_sales), and a data frame
#' # 'elasticity_results' containing sku, elasticity, competitor_elasticity.
#'
#' result_list <- calculate_sales_revenue_change(
#'   sku = 1,
#'   price_change_pct = 5,
#'   sku_optimisation_results = elasticity_results,
#'   df = df_w_competitor
#' )
#'
#' # Inspect the returned information:
#' result_list$results
#' }
#'
#' @export
#' 

get_sales_revenue_change <- function(sku, price_change_pct, sku_optimisation_results, df) {
  # Retrieve elasticity for the given SKU from the optimization results
  elasticity_value <- sku_optimisation_results %>%
    filter(sku == !!sku) %>%
    pull(elasticity)
  # Retrieve competitor elasticity for the given SKU from the optimization results
  comp_elasticity_value <- sku_optimisation_results %>%
    filter(sku == !!sku) %>%
    pull(competitor_elasticity)
  
  # Ensure SKU exists in both datasets
  if (length(elasticity_value) == 0) {
    stop(paste("Error: SKU", sku, "not found in sku_optimisation_results."))
  }
  
  # Filter data for the given SKU
  sku_data <- df %>% filter(sku == !!sku)
  
  # Convert to Date format
  sku_data$week <- as.Date(sku_data$week, format = "%d/%m/%Y")
  
  # Extract current price and sales for the SKU
  current_price <- sku_data$price
  current_sales <- sku_data$weekly_sales
  
  # Compute new price after applying price change percentage
  new_price <- current_price * (1 + price_change_pct / 100)
  
  # Compute new sales quantity using price elasticity formula
  new_sales_qty <- current_sales * (new_price / current_price) ^ (-elasticity_value)
  
  # Compute revenue before and after price change
  old_revenue <- current_sales * current_price
  new_revenue <- new_sales_qty * new_price
  
  # Compute percentage change in sales quantity and revenue
  sales_qty_change_pct <- ((new_sales_qty - current_sales) / current_sales) * 100
  revenue_change_pct <- ((new_revenue - old_revenue) / old_revenue) * 100
  
  sku_data <- list(
    results = data.frame(
      date = sku_data$week, 
      new_sales_qty = new_sales_qty,
      old_revenue = old_revenue,
      new_revenue = new_revenue,
      sales_qty_change_pct = sales_qty_change_pct,
      revenue_change_pct = revenue_change_pct,
      new_price = new_price
    ),
    price_change_pct = price_change_pct,
    price_elasticity = elasticity_value,
    competitor_price_elasticity = comp_elasticity_value
  )
  # Return the updated data frame
  return(sku_data)
}

