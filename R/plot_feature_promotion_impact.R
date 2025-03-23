#' Plot Feature Promotion Impact
#'
#' Generates visualizations and summary statistics highlighting the impact of featured promotions on sales metrics across different SKUs.
#'
#' @param scanpro_results A dataframe containing model results, typically obtained from a SKU-level promotion impact analysis. Expected to include columns: sku, promo_significant, baseline_sales, model_featured_sales, promo_lift_pct, lower_ci_pct, upper_ci_pct, incremental_sales, baseline_revenue, featured_revenue, and incremental_revenue.
#' @param df Original data frame containing weekly sales and pricing data used for modeling. Currently unused but retained for potential future extensions.
#'
#' @return A list containing:
#' \describe{
#'   \item{sales_chart}{A ggplot object showing average weekly sales with and without feature promotions for top SKUs.}
#'   \item{incremental_chart}{A ggplot object displaying incremental sales with 95\% confidence intervals.}
#'   \item{revenue_chart}{A ggplot object depicting incremental revenue with 95\% confidence intervals.}
#'   \item{significant_results}{Dataframe of SKUs with statistically significant promotion effects.}
#'   \item{summary_stats}{Dataframe summarizing key metrics, such as average promotion lift and incremental sales.}
#' }
#'
#' @examples
#' results <- plot_feature_promotion_impact(scanpro_results, df)
#' results$sales_chart
#' results$incremental_chart
#'
#' @import ggplot2
#' @import dplyr
#' @import tidyr
#' @export

plot_feature_promotion_impact <- function(scanpro_results, df) {
  # Filter for significant results
  significant_results <- scanpro_results %>%
    filter(promo_significant == TRUE)
  
  # Group by SKU to get unique SKUs with mean values for each metric
  unique_significant_results <- significant_results %>%
    group_by(sku) %>%
    summarise(
      baseline_sales = mean(baseline_sales),
      model_featured_sales = mean(model_featured_sales),
      promo_lift_pct = mean(promo_lift_pct),
      lower_ci_pct = mean(lower_ci_pct),
      upper_ci_pct = mean(upper_ci_pct),
      incremental_sales = mean(incremental_sales),
      baseline_revenue = mean(baseline_revenue),
      featured_revenue = mean(featured_revenue),
      incremental_revenue = mean(incremental_revenue),
      # Calculate CIs for incremental sales
      incremental_sales_lower_ci = mean(incremental_sales * (1 + (lower_ci_pct - promo_lift_pct) / promo_lift_pct)),
      incremental_sales_upper_ci = mean(incremental_sales * (1 + (upper_ci_pct - promo_lift_pct) / promo_lift_pct)),
      # Calculate CIs for incremental revenue
      incremental_revenue_lower_ci = mean(incremental_revenue * (1 + (lower_ci_pct - promo_lift_pct) / promo_lift_pct)),
      incremental_revenue_upper_ci = mean(incremental_revenue * (1 + (upper_ci_pct - promo_lift_pct) / promo_lift_pct)),
      .groups = 'drop'
    )
  
  # For visualization, focus on top SKUs by promotion lift
  top_skus <- head(
    unique_significant_results %>% arrange(desc(promo_lift_pct)) %>% pull(sku), 
    min(15, nrow(unique_significant_results))
  )
  
  plot_data <- unique_significant_results %>%
    filter(sku %in% top_skus) %>%
    # Store original SKU value before converting to factor
    mutate(
      sku_original = sku,
      # Create unique labels to prevent duplicate factor levels
      sku_label = paste0(sku),
      # Create factor with these unique labels
      sku = factor(sku_label, levels = sku_label[order(promo_lift_pct, decreasing = TRUE)])
    )

  # Convert to long format for side-by-side bar chart
  plot_data_long <- plot_data %>%
    dplyr::select(sku, baseline_sales, model_featured_sales) %>%
    pivot_longer(
      cols = c(baseline_sales, model_featured_sales),
      names_to = "sales_type", 
      values_to = "sales"
    ) %>%
    mutate(
      sales_type = factor(
        sales_type, 
        levels = c("baseline_sales", "model_featured_sales"),
        labels = c("No Feature", "Featured")
      )
    )
  
  app_theme <- theme(
    plot.background = element_rect(fill = "transparent", color = NA),
    axis.ticks = element_line(color = "white"), 
    # panel.background = element_rect(fill = "transparent", color = NA),
    panel.grid.major = element_line(color = "gray70"),
    panel.grid.minor = element_line(color = "gray70"),
    text = element_text(color = "white"),
    axis.text = element_text(color = "white"),
    axis.title = element_text(color = "white"),
    strip.text = element_text(color = "white"),
    legend.text = element_text(color = "white"),
    legend.title = element_text(color = "white"),
  )
  
  
  # Create bar chart comparing non-featured and featured sales
  sales_chart <- ggplot(plot_data_long, aes(x = sku, y = sales, fill = sales_type)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    scale_fill_manual(values = c("No Feature" = "#E69F00", 
                                 "Featured" = "dodgerblue")) +
    labs(
      # title = "Feature Promotion Impact: Sales Comparison",
      #    subtitle = "Average weekly sales when featured vs. not featured on main page",
      x = "SKU",
      y = "Average Weekly Sales",
      fill = "Feature status"
    ) +
    theme(
      # axis.text.y = element_text(size = 9),
      legend.position = "top", 
      legend.background = element_blank(),       # Removes the legend background
      legend.key = element_blank()  
    ) +
    app_theme +
    coord_flip()
  
  # Create incremental sales chart with 95% CI
  incremental_chart <- ggplot(plot_data, aes(x = reorder(as.factor(sku), incremental_sales), y = incremental_sales)) +
    geom_bar(stat = "identity", fill = rgb(1, 0.2, 0)) +
    geom_errorbar(aes(ymin = incremental_sales_lower_ci, ymax = incremental_sales_upper_ci), 
                  width = 0.3, color = "black", alpha = 0.4) +
    geom_text(aes(label = round(incremental_sales, 1)), hjust = -0.1, size = 3) +
    coord_flip() +
    labs(
      # title = "Incremental Sales from Feature Promotion",
      # subtitle = "Additional units sold when featured on main page (with 95% confidence intervals)",
      x = "SKU",
      y = "Incremental Sales (Units)"
    ) +
    app_theme 
  
  # Create incremental revenue chart with 95% CI
  revenue_chart <- ggplot(plot_data, aes(x = reorder(as.factor(sku), incremental_revenue), y = incremental_revenue)) +
    geom_bar(stat = "identity", fill = "darkgreen") +
    geom_errorbar(aes(ymin = incremental_revenue_lower_ci, ymax = incremental_revenue_upper_ci), 
                  width = 0.3, color = "black", alpha = 0.4) +
    geom_text(aes(label = paste0("$", round(incremental_revenue, 2))), hjust = -0.1, size = 3) +
    coord_flip() +
    labs(
      # title = "Incremental Revenue from Feature Promotion",
      #    subtitle = "Additional revenue when featured on main page (with 95% confidence intervals)",
      x = "SKU",
      y = "Incremental Revenue ($)"
    ) +
    app_theme

  
  # Create a table of summary statistics
  summary_stats <- data.frame(
    Metric = c(
      "Number of SKUs with significant promotion effect",
      "Average promotion lift across all significant SKUs",
      "Highest promotion lift",
      "Lowest promotion lift",
      "Average baseline sales (units)",
      "Average promoted sales (units)",
      "Average incremental sales (units)"
    ),
    Value = c(
      length(unique(significant_results$sku)),  # Count unique SKUs
      paste0(round(mean(unique_significant_results$promo_lift_pct, na.rm = TRUE), 2), "%"),
      paste0(round(max(unique_significant_results$promo_lift_pct, na.rm = TRUE), 2), "%"),
      paste0(round(min(unique_significant_results$promo_lift_pct, na.rm = TRUE), 2), "%"),
      round(mean(unique_significant_results$baseline_sales, na.rm = TRUE), 2),
      round(mean(unique_significant_results$model_featured_sales, na.rm = TRUE), 2),
      round(mean(unique_significant_results$incremental_sales, na.rm = TRUE), 2)
    )
  )
  
  return(
    list(
      sales_chart = sales_chart, 
      incremental_chart = incremental_chart,
      revenue_chart = revenue_chart,
      significant_results = significant_results,
      summary_stats = summary_stats
      )
  )
}


