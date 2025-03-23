data("data_w_competitor", "data_raw", "data_processed")

df <- datar_w_cp %>%
  mutate(week_dt = as.Date(week, origin = "1970-01-01")) %>%
  mutate(
    year = year(week_dt),   
    month = month(week_dt)
  )

df_lambda03 <- adstock(df, lambda = 0.3)
df_lambda05 <- adstock(df, lambda = 0.5)
df_lambda07 <- adstock(df, lambda = 0.7)


# Run SKU optimization
elasticity_optim_03 <- optimise_by_sku(df_lambda03)
elasticity_optim_05 <- optimise_by_sku(df_lambda05)
elasticity_optim_07 <- optimise_by_sku(df_lambda07)

# save(elasticity_optim_03, file = "data/elasticity_optim_03.rda", compress = "bzip2")
# save(elasticity_optim_05, file = "data/elasticity_optim_05.rda", compress = "bzip2")
# save(elasticity_optim_07, file = "data/elasticity_optim_07.rda", compress = "bzip2")

# Call the function


for (j in 1:44) {
  result <- get_sales_revenue_change(j, -10, elasticity_optim_07, df)
  
  plot(result$results$date, result$results$new_revenue, type = "l")
  
  for(i in seq(-40, 40, 5)) { 
    result <- get_sales_revenue_change(j, i, elasticity_optim_07, df)
    lines(result$results$date, result$results$new_revenue, type = "l", col = rgb(1, 0.5, 0.5, 0.50))
  }
 Sys.sleep(1) 
}

# plot 
# new_price
# new_revenue
# new_sales_qty
