#' Plot Forecasts with Actual Sales for Selected SKUs
#'
#' This function creates interactive line plots using `ggplot2` and `plotly` to 
#' compare forecasted sales (with confidence intervals) against actual sales 
#' for one or more SKUs. Users can optionally aggregate all SKUs and display 
#' a combined plot.
#'
#' @param full_data A data.frame containing the actual sales data with at least
#'   the columns: `week`, `sku`, and `weekly_sales`.
#' @param forecast_df_ci A data.frame containing forecasted values and
#'   confidence intervals. Required columns: `week`, `sku`, `forecast`,
#'   `lower`, `upper`.
#' @param sku_ids A character vector or numeric vector of SKUs to include in 
#'   the plot. Use `"all"` (default) to select all SKUs.
#' @param aggregate_all Logical. If `TRUE` and multiple SKUs are selected, 
#'   their values will be aggregated into a single combined plot. Defaults to `TRUE`.
#'
#' @return An interactive plotly subplot object showing actuals vs forecasts 
#'   with uncertainty bands, arranged by SKU.
#'
#' @examples
#' \dontrun{
#' plot_forecast_with_actuals(
#'   full_data = actual_data,
#'   forecast_df_ci = forecast_data,
#'   sku_ids = c(101, 102),
#'   aggregate_all = FALSE
#' )
#' }
#' 
#' @export
plot_forecast <- function(
    full_data, 
    forecast_df_ci, 
    sku_ids = "all", 
    aggregate_all = TRUE
) {
  
  original_all <- tolower(sku_ids[1]) == "all"
  
  if (tolower(sku_ids[1]) == "all") {
    sku_ids <- unique(forecast_df_ci$sku)
  }
  
  # filter actuals
  actuals <- full_data %>%
    filter(sku %in% sku_ids) %>%
    dplyr::select(week, sku, weekly_sales)
  
  # get forecasts
  forecasts <- forecast_df_ci %>%
    filter(sku %in% sku_ids) %>%
    dplyr::select(week, sku, forecast, lower, upper)
  
  
  if (aggregate_all && length(sku_ids) > 1) {
    actuals <- actuals %>%
      group_by(week) %>%
      summarise(weekly_sales = sum(weekly_sales, na.rm = TRUE), .groups = "drop") %>%
      mutate(type = "actual", sku = "all")
    
    forecasts <- forecasts %>%
      group_by(week) %>%
      summarise(
        weekly_sales = sum(forecast, na.rm = TRUE),
        lower = sum(lower, na.rm = TRUE),
        upper = sum(upper, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(type = "forecast", sku = "all")
  } else {
    actuals <- actuals %>% mutate(type = "actual")
    forecasts <- forecasts %>% rename(weekly_sales = forecast) %>% mutate(type = "forecast")
  }
  
  # Combine
  actuals$week <- as.Date(actuals$week)
  combined <- bind_rows(actuals, forecasts)
  
  
  if (!"lower" %in% colnames(forecasts)) {
    forecasts$upper <- forecasts$lower <- NA
  }
  
  
  if (!aggregate_all) {
    combined <- combined[combined$sku %in% as.numeric(sku_ids), ] 
  }
  
  y_lims <- range(combined[, c("weekly_sales", "upper", "lower")], na.rm = T)
  x_lims <- range(combined$week, na.rm = T)
  
  global_y_title <- "Sales"
  global_x_title <- "Time"
  
  plot_list <- list()
  ids <- unique(combined$sku)
  for (i in seq_along(ids)) {
    sub_sku <- combined[combined$sku == ids[i], ]  
    p <- ggplot(sub_sku, aes(x = week, y = weekly_sales)) +
      geom_line(
        data = sub_sku %>% filter(type == "actual"),
        aes(x = week, y = weekly_sales),
        colour = "black", size = 1
      ) +
      geom_line(
        data = sub_sku %>% filter(type == "forecast"),
        aes(x = week, y = weekly_sales),
        colour = "red", size = 1
      ) +
      geom_ribbon(
        data = forecasts[forecasts$sku == ids[i], ],
        aes(x = week, ymin = lower, ymax = upper),
        fill = "red", alpha = 0.2, inherit.aes = FALSE
      ) + xlim(x_lims) + ylim(y_lims) + 
      facet_wrap( ~ sku, ncol = 1) +
      theme(
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
        panel.spacing = unit(1.5, "lines") 
      )
    
    plot_list[[i]] <- ggplotly(p) %>% layout(showlegend = FALSE)
  }
  n_plots <- length(plot_list)
  
  # Determine layout rows
  nrows <- if (n_plots <= 4) 1 else ceiling(n_plots / 4)
  
  # Combine into subplot grid
  p_all <- subplot(
    plot_list,
    nrows = nrows,
    shareX = TRUE,
    shareY = TRUE,
    titleX = FALSE,
    titleY = FALSE,
    margin = 0.02
  )
  
  # Add global axis titles
  p_all <- layout(
    p_all,
    margin = list(l = 80, r = 20, b = 80, t = 40),
    annotations = list(
      list(
        text = global_y_title,
        font = list(size = 14),
        x = -0.08,
        y = 0.5,
        xref = "paper",
        yref = "paper",
        xanchor = "center",
        yanchor = "middle",
        showarrow = FALSE,
        textangle = -90
      ),
      list(
        text = global_x_title,
        font = list(size = 14),
        x = 0.5,
        y = -0.12,
        xref = "paper",
        yref = "paper",
        xanchor = "center",
        yanchor = "top",
        showarrow = FALSE
      )
    )
  )
  return(p_all)
}

