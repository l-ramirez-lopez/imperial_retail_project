library(cloud.ui)

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

url <- "http://0.0.0.0:8001/"
#url <- "http://127.0.0.1:8000/"

# maximum waiting time for first call to the API (in mins)
max_wait <- 0.10

# latency between attempts (in mins)
wlatency <- 0.05

app_mode <- "cloud.io"
#app_mode <- "dbsearch"

dbsearch_n_samples <- 150
