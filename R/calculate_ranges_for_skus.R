#' Calculate Revenue, Price, and Sales Ranges for Selected SKUs
#'
#' @param mdata A data frame containing SKU information (e.g., one with columns \code{SKU}, \code{price}, etc.).
#' @param selected_rows A vector of row indices (e.g., \code{input$my_elasticity_rows_selected}) identifying which SKUs are chosen.
#' @param my_price_change Numeric. The user's chosen percentage price change (e.g., \code{input$my_price_change}).
#' @param my_dropdown_elast A character string indicating which lambda value was selected (e.g., \code{input$my_dropdown_elast}).
#' @param my_variable A character string indicating another selected variable (e.g., \code{input$my_variable}).
#' @param my_lambdas A data frame or list that maps user-facing lambda values to internal aliases (e.g., with columns \code{lambda_user} and \code{alias}).
#' @param my_variables A data frame or list that maps user-facing variable names to internal aliases (e.g., columns \code{variables_user} and \code{alias}).
#' @param sku_price_change_lims A list containing min/max data for each SKU-lambda combination (e.g., \code{sku_price_change_lims[["sku_1"]][["my_lambda_alias"]]}).
#' @param melasticity An object or data needed by \code{get_sales_revenue_change} for elasticity calculations.
#' @param get_sales_revenue_change A function of the form \code{function(sku, price_change_pct, melasticity, mdata) \{\}} that returns a list with a \code{results} data frame including columns \code{new_revenue}, \code{new_sales_qty}, \code{new_price}, etc.
#'
#' @return A named list with three data frames: \code{revenue}, \code{price}, and \code{sales}. Each includes columns \code{sku}, \code{date}, \code{x}, \code{min}, and \code{max}.
#'
#' @examples
#' \dontrun{
#' result_list <- calculate_ranges_for_skus(
#'   mdata = my_data,
#'   selected_rows = input$my_elasticity_rows_selected,
#'   my_price_change = input$my_price_change,
#'   my_dropdown_elast = input$my_dropdown_elast,
#'   my_variable = input$my_variable,
#'   my_lambdas = my_lambdas,
#'   my_variables = my_variables,
#'   sku_price_change_lims = sku_price_change_lims,
#'   melasticity = melasticity,
#'   get_sales_revenue_change = get_sales_revenue_change
#' )
#'
#' head(result_list$revenue)
#' }
#' @export
calculate_ranges_for_skus <- function(
    mdata,
    selected_rows,
    my_price_change,
    my_dropdown_elast,
    my_variable,
    my_lambdas,
    my_variables,
    sku_price_change_lims,
    melasticity,
    get_sales_revenue_change
) {
  
  if (is.null(selected_rows)) {
    return(NULL)
  }
  # 1. Identify which SKUs are selected
  colnames(melasticity) <- c("sku", "competitor_elasticity", "elasticity", "mape")
  user_sku <- melasticity$sku[selected_rows]
  
  # 2. Map user-facing input to internal aliases (lambda, variable)
  mlambda <- my_lambdas$alias[my_lambdas$lambda_user == my_dropdown_elast]
  mvar    <- my_variables$alias[my_variables$variables_user == my_variable]
  # (If mvar is used later in the code, you can reference it as needed)
  
  # Prepare empty lists to store output
  revenue <- list()
  price   <- list()
  sales   <- list()

  # 3. Loop over each selected SKU
  for (i in user_sku) {
    # Extract min/max data for this SKU-lambda from 'sku_price_change_lims'
    ith_sku <- sku_price_change_lims[[paste0("sku_", i)]][[mlambda]]
    
    # Compute new sales, revenue, etc. after price change
    ith_pc <- get_sales_revenue_change(
      sku = i,
      price_change_pct = my_price_change,
      sku_optimisation_results = melasticity,
      df = mdata
    )
    
    # Build data frames for each measure
    ith_revenue <- data.frame(
      sku = i,
      date = ith_pc$results$date,
      x = ith_pc$results$new_revenue,
      min = ith_sku$min$results$new_revenue,
      max = ith_sku$max$results$new_revenue
    )
    
    ith_sales <- data.frame(
      sku = i,
      date = ith_pc$results$date,
      x = ith_pc$results$new_sales_qty,
      min = ith_sku$min$results$new_sales_qty,
      max = ith_sku$max$results$new_sales_qty
    )
    
    ith_price <- data.frame(
      sku = i,
      date = ith_pc$results$date,
      x = ith_pc$results$new_price,
      min = ith_sku$min$results$new_price,
      max = ith_sku$max$results$new_price
    )
    
    # Append to the lists
    revenue[[length(revenue) + 1]] <- ith_revenue
    price[[length(price) + 1]]     <- ith_price
    sales[[length(sales) + 1]]     <- ith_sales
  }
  
  # 4. Combine each list into a single data frame
  revenue_df <- do.call(rbind, revenue)
  price_df   <- do.call(rbind, price)
  sales_df   <- do.call(rbind, sales)
  
  cat("tables ok!\n")
  
  # 5. Return a list of data frames
  return(list(
    revenue = revenue_df,
    price   = price_df,
    sales   = sales_df
  ))
}
