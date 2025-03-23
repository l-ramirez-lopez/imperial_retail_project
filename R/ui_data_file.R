#' Generate Data Source and Project Information UI
#'
#' This function creates UI components to display information about the data source
#' (including a link to the associated book) and project details (with GitHub repository link).
#' It's designed to provide contextual information for users of the Shiny application.
#'
#' @return A Shiny [fluidRow()] containing HTML elements that display:
#' \itemize{
#'   \item Data source reference with link to the Springer book
#'   \item Project information with link to GitHub repository
#' }
#' 
#' @importFrom shiny fluidRow HTML
#' 
#' @examples
#' \dontrun{
#' # Example usage in Shiny app UI
#' ui <- fluidPage(
#'   ui_data_file()
#' )
#' }
#' @export

ui_data_file <- function() {
  fluidRow(
    HTML("<h1 style='color:#FFFFFF'><strong>Data source</strong></h1>"),
    HTML(
      paste0(
        "The dataset used for this dashboard is provided in the book:<br><br>",
        '<a href="https://link.springer.com/book/10.1007/978-3-030-85855-1" target="_blank">',
        "Cohen, M. C., Gras, P.E., Pentecoste, A., & Zhang, R. (2022). ",
        "Demand Prediction in Retail - A Practical Guide to Leverage Data and Predictive Analytics. ",
        "Springer Series in Supply Chain Management 14, 1–155.",
        "</a>"
      )
    ), 
    HTML("<h1 style='color:#FFFFFF; margin-top:60px;'><strong>Our project</strong></h1>"),
    HTML(
      paste0(
        "The GitHub repository for this project can be visited at:<br><br>",
        '<a href="https://github.com/l-ramirez-lopez/imperial_retail_project" target="_blank">',
        "https://github.com/l-ramirez-lopez/imperial_retail_project",
        "</a><br><br>"
      ) 
    )
  )
}
