#' Run Feature-based Promotional Sales Model
#'
#' Fits a log-linear regression model to evaluate the impact of a promotional feature on sales.
#'
#' @param df A dataframe containing sales and product data.
#' @param sku_col Name of the column containing SKU identifiers. Default is "sku".
#' @param sales_col Name of the column with weekly sales data. Default is "weekly_sales".
#' @param price_col Name of the column with product price data. Default is "price".
#' @param revenue_col Name of the column with revenue data. Default is "Revenue".
#' @param date_col Name of the column with date/week information. Default is "week".
#' @param feature_col Name of the binary column indicating the presence of a promotional feature. Default is "feat_main_page".
#' @param color_col Name of the column indicating product color. Default is "Color".
#' @param vendor_col Name of the column indicating vendor information. Default is "Vendor".
#' @param month_cols Character vector indicating month indicator columns (e.g., c("month_1", "month_2")).
#' @param trend_col Name of the column representing a trend variable (e.g., a numeric sequence for weeks).
#' @param min_obs Minimum number of observations required per SKU for model fitting. Default is 10.
#' @param min_featured Minimum number of observations with the feature present. Default is 3.
#' @param min_not_featured Minimum number of observations without the feature present. Default is 3.
#'
#' @return A dataframe with promotional lift analysis and performance metrics per SKU.
#' @details
#' This function performs SKU-level log-linear modeling to estimate the sales lift from a promotional feature.
#' It returns coefficients, statistical significance, incremental sales, revenue estimates, and confidence intervals.
#'
#' @examples
#' result <- run_feature_scanpro_model(
#'   df = sales_data,
#'   sku_col = "product_id",
#'   sales_col = "units_sold",
#'   price_col = "unit_price",
#'   revenue_col = "total_revenue",
#'   date_col = "sale_week",
#'   feature_col = "promo_feature",
#'   color_col = "item_color",
#'   vendor_col = "supplier",
#'   month_cols = paste0("month_", 1:12),
#'   trend_col = "week_trend"
#' )
#' 
#' @export

run_feature_scanpro_model <- function(
    df,
    sku_col = "sku",
    date_col = "week",
    sales_col = "weekly_sales",
    price_col = "price",
    revenue_col = "Revenue",
    feature_col = "feat_main_page",
    color_col = "Color",
    vendor_col = "Vendor",
    functionality_prefix = "functionality_",
    month_cols = paste0("month_", 2:12),
    trend_col = "trend",
    min_total_obs = 10,
    min_feature_obs = 3
) {
  df[[date_col]] <- as.Date(df[[date_col]])
  scanpro_results <- data.frame()
  skus <- unique(df[[sku_col]])
  
  for (current_sku in skus) {
    sku_data <- df[df[[sku_col]] == current_sku & df[[sales_col]] > 0, ]
    
    functionality_cols <- grep(functionality_prefix, names(df), value = TRUE)
    sku_func_data <- sku_data[1, functionality_cols]
    product_type_col <- functionality_cols[which(sku_func_data == 1)]
    product_type <- if (length(product_type_col) > 0) {
      gsub(paste0(functionality_prefix, "\\d+\\."), "", product_type_col[1])
    } else {
      "Unknown"
    }
    
    color <- unique(sku_data[[color_col]])[1]
    vendor <- unique(sku_data[[vendor_col]])[1]
    
    if (nrow(sku_data) >= min_total_obs) {
      n_featured <- sum(sku_data[[feature_col]] == 1)
      n_not_featured <- sum(sku_data[[feature_col]] == 0)
      
      if (n_featured >= min_feature_obs && n_not_featured >= min_feature_obs) {
        predictors <- paste(
          feature_col, paste0("log(", price_col, ")"),
          paste(month_cols, collapse = " + "), trend_col,
          sep = " + "
        )
        
        formula <- as.formula(paste("log(", sales_col, ") ~", predictors))
        model <- lm(formula, data = sku_data)
        
        if (is.null(model)) next
        
        model_summary <- summary(model)
        coefs <- coef(model_summary)
        
        if (feature_col %in% rownames(coefs)) {
          feature_coef <- coefs[feature_col, "Estimate"]
          feature_se <- coefs[feature_col, "Std. Error"]
          feature_pvalue <- coefs[feature_col, "Pr(>|t|)"]
          
          price_elasticity <- if (paste0("log(", price_col, ")") %in% rownames(coefs)) {
            coefs[paste0("log(", price_col, ")"), "Estimate"]
          } else {
            NA
          }
          
          feature_multiplier <- exp(feature_coef)
          feature_lift_pct <- (feature_multiplier - 1) * 100
          lower_ci <- exp(feature_coef - 1.96 * feature_se) - 1
          upper_ci <- exp(feature_coef + 1.96 * feature_se) - 1
          
          baseline_data <- sku_data[sku_data[[feature_col]] == 0, ]
          featured_data <- sku_data[sku_data[[feature_col]] == 1, ]
          
          baseline_sales <- mean(baseline_data[[sales_col]])
          actual_featured_sales <- mean(featured_data[[sales_col]])
          model_featured_sales <- baseline_sales * feature_multiplier
          incremental_sales <- baseline_sales * (feature_multiplier - 1)
          actual_incremental_sales <- actual_featured_sales - baseline_sales
          
          avg_price <- mean(sku_data[[price_col]])
          baseline_price <- mean(baseline_data[[price_col]])
          featured_price <- mean(featured_data[[price_col]])
          
          avg_revenue <- mean(sku_data[[revenue_col]])
          baseline_revenue <- mean(baseline_data[[revenue_col]])
          featured_revenue <- mean(featured_data[[revenue_col]])
          incremental_revenue <- baseline_revenue * (feature_multiplier - 1)
          actual_incremental_revenue <- featured_revenue - baseline_revenue
          
          predicted_values <- exp(predict(model, newdata = sku_data))
          actual_values <- sku_data[[sales_col]]
          mape <- mean(abs((actual_values - predicted_values) / actual_values) * 100, na.rm = TRUE)
          
          sku_result <- data.frame(
            sku = current_sku,
            product_type = product_type,
            vendor = vendor,
            color = color,
            dates = list(sku_data[[date_col]]),
            n_obs = nrow(sku_data),
            n_featured = n_featured,
            n_not_featured = n_not_featured,
            baseline_sales, actual_featured_sales, model_featured_sales,
            baseline_price, featured_price, avg_price,
            baseline_revenue, featured_revenue, avg_revenue,
            feature_coef, feature_se, feature_multiplier,
            promo_lift_pct = feature_lift_pct,
            lower_ci_pct = lower_ci * 100,
            upper_ci_pct = upper_ci * 100,
            incremental_sales, actual_incremental_sales,
            incremental_revenue, actual_incremental_revenue,
            price_elasticity,
            promo_significant = feature_pvalue < 0.05,
            r_squared = model_summary$r.squared,
            adj_r_squared = model_summary$adj.r.squared,
            mape
          )
          
          scanpro_results <- rbind(scanpro_results, sku_result)
        }
      }
    }
  }
  
  scanpro_results <- scanpro_results %>% arrange(desc(promo_lift_pct))
  
  cat("Average MAPE:", mean(scanpro_results$mape, na.rm = TRUE), "%\n")
  cat("Average R-squared:", mean(scanpro_results$r_squared, na.rm = TRUE), "\n")
  cat("Average Adjusted R-squared:", mean(scanpro_results$adj_r_squared, na.rm = TRUE), "\n")
  
  return(scanpro_results)
}

