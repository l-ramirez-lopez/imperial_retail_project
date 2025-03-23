<<<<<<< HEAD
=======
library(cloud.ui)

>>>>>>> 7564aaad4ab926c27f2909d6071934eea1a94b97
library(showtext)
library(thematic)
library(bslib)

library(shiny)
library(shinyjs)
library(shinyalert)
library(shinyWidgets)
library(shinycssloaders)
library(data.table)
library(DT)
library(plotly)
library(rmarkdown)
<<<<<<< HEAD
library(knitr)
library(dplyr)
library(lubridate)
library(forecast)
library(tseries)
library(vars)
library(stargazer)
library(purrr)
library(data.table)


min_price_change <- -40
max_price_change <- 40

data("data_w_competitor", "data_raw", "data_processed")
data("elasticity_optim_03", "elasticity_optim_05", "elasticity_optim_07")

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

my_lambdas <- data.frame(
  alias = c(
    "lambda03", 
    "lambda05", 
    "lambda07"
  ),
  lambda_user = c(
    "0.3 (low)",
    "0.5 (moderate)",
    "0.7 (high)"
  )
)

my_variables <- data.frame(
  alias = c(
    "new_price", "new_revenue", "new_sales_qty"
  ),
  variables_user = c(
    "Price",
    "Revenue",
    "Sales"
  )
)

   



datap 
datar
datar_w_cp

## Compute all the limits for the graphics of price elasticity
sku_price_change_lims <- NULL 
for (i in sort(unique(datap$sku))) {
  sku_price_change_lims[[i]] <- list()
  sku_price_change_lims[[i]][["lambda03"]] <- NULL
  sku_price_change_lims[[i]][["lambda05"]] <- NULL
  sku_price_change_lims[[i]][["lambda07"]] <- NULL

  sku_price_change_lims[[i]][["lambda03"]][["min"]] <- get_sales_revenue_change(
    i, min_price_change, elasticity_optim_03, df_lambda03
  )
  sku_price_change_lims[[i]][["lambda03"]][["max"]] <- get_sales_revenue_change(
    i, max_price_change, elasticity_optim_03, df_lambda03
  )
  
  sku_price_change_lims[[i]][["lambda05"]][["min"]] <- get_sales_revenue_change(
    i, min_price_change, elasticity_optim_05, df_lambda05
  )
  sku_price_change_lims[[i]][["lambda05"]][["max"]] <- get_sales_revenue_change(
    i, max_price_change, elasticity_optim_05, df_lambda05
  )
  
  sku_price_change_lims[[i]][["lambda07"]][["min"]] <- get_sales_revenue_change(
    i, min_price_change, elasticity_optim_07, df_lambda07
  )
  sku_price_change_lims[[i]][["lambda07"]][["max"]] <- get_sales_revenue_change(
    i, max_price_change, elasticity_optim_07, df_lambda07
  )
}
names(sku_price_change_lims) <- paste0("sku_", sort(unique(datap$sku)))


# 
# datap <- read.csv("inst/app/www/data/data_processed.csv")
# datar <- read.csv("inst/app/www/data/data_raw.csv")
# datar_w_cp <- read.csv("inst/app/www/data/data_processed with competitor price v2.csv")
# 
# 
# save(datap, file = "data/data_processed.rda", compress = "bzip2")
# save(datar, file = "data/data_raw.rda", compress = "bzip2")
# save(datar_w_cp, file = "data/data_w_competitor.rda", compress = "bzip2")

# datap <- read.csv("www/data/data_processed.csv")
# datar <- read.csv("www/data/data_raw.csv")
# datar_w_cp <- read.csv("www/data/data_processed with competitor price.csv")


# save(elasticity_optim_03, file = "data/elasticity_optim_03.rda", compress = "bzip2")
# save(elasticity_optim_05, file = "data/elasticity_optim_05.rda", compress = "bzip2")
# save(elasticity_optim_07, file = "data/elasticity_optim_07.rda", compress = "bzip2")



library(ggplot2)
library(dplyr)

datar_w_cp %>%
  group_by(week) %>%
  summarise(total_sales = sum(weekly_sales)) %>%
  ggplot(aes(x = week, y = total_sales)) +
  geom_line(color = "#63b046") +
  labs(title = "Total Weekly Sales Over Time",
       x = "Week",
       y = "Total Sales")


aa <- datar_w_cp %>%
  group_by(Functionality) %>%
  summarise(avg_sales = mean(weekly_sales)) %>%
  ggplot(aes(x = reorder(Functionality, avg_sales), y = avg_sales)) +
  geom_col(fill = "#63b046") +
  coord_flip() +
  labs(title = "Average Weekly Sales by Product Functionality",
       x = "Functionality",
       y = "Average Weekly Sales")


ggplotly(aa)
=======

url <- "http://0.0.0.0:8001/"
#url <- "http://127.0.0.1:8000/"

# maximum waiting time for first call to the API (in mins)
max_wait <- 0.10

# latency between attempts (in mins)
wlatency <- 0.05

app_mode <- "cloud.io"
#app_mode <- "dbsearch"

dbsearch_n_samples <- 150
>>>>>>> 7564aaad4ab926c27f2909d6071934eea1a94b97
