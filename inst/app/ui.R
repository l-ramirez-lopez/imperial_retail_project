library(shiny)
library(bslib)
library(plotly)
library(shinyWidgets)
library(shinyjs)




imperial_logo <- paste0(
  "<img src='business-school-logo.png'",  
  "height='40px'", 
  "style='position: absolute; right: 20px; top: 10px;' />"
)

app_theme <- bs_theme(
  version = 5,
  bg = "#101010",
  fg = "#B3B3B3",
  primary = "#E69F00",
  secondary = "#E69F00",
  success = "#009E73",
  font_scale = 0.95,
  base_font = font_google("Lato"),
  code_font = font_google("Lato")
  # "font-size-base" = "0.75rem", "enable-rounded" = FALSE
)


browser_title <- "> r&ma"
service_title <- "> r&ma"

# app_side_bar <- sidebar(
#   hr(), 
#   tags$a(
#     href = "https://github.com/l-ramirez-lopez/imperial_retail_project",
#     "Visit our project in GitHub",
#     target = "_blank"
#   )
# )

app_nav_bar <- page_navbar(
  # sidebar = app_side_bar,
  title = HTML(paste0(service_title, imperial_logo)),
  theme = app_theme,
  
  # --- Summary Tab
  nav_panel(
    "Overview",
    plotlyOutput("my_overview")
  ),
  
  # --- File Summary Tab
  nav_panel(
    "Demand forecasting",
    uiOutput("demand_forecasting")
  ),
  
  # --- Build Application Tab
  nav_panel(
    "Feature promotion",
    uiOutput("feature_promotion")
  ),
  
  # --- Build Application Tab
  nav_panel(
    "Price elasticity and scenario testing",
    fluidRow(
      # ---- Box 1: Dropdown + Table ----
      column(
        width = 4,
        card(
          card_header("Lambda parameter"),
          card_body(
            selectInput(
              inputId = "my_dropdown_elast", 
              label = "Choose a lambda parameter for the adstock model:",
              choices = my_lambdas$lambda_user, 
              selected = "0.7 (high)"
            )
          ), 
          height = "275px"
        )
      ), 
      column(
        width = 4,
        card(
          card_header("SKU and variable"),
          card_body(
            sliderInput(
              inputId = "my_price_change",
              label = "Pick a value for the price change:",
              min = min_price_change,
              max = max_price_change,
              value = 0,  # Center value
              step = 1
            ),
            selectInput(
              inputId = "my_variable", 
              label = "Choose a variable to be shown:",
              choices = c("Price", "Revenue", "Sales"), 
              selected = "Price"
            )
          ), 
          
        )
      )
    ),
      
    
    fluidRow(
      # ---- Box 1: Dropdown + Table ----
      column(
        width = 4,
        card(
          card_header("SKU price elasticity"),
            dataTableOutput("my_elasticity", width = "70%")
          )
        )
      ),
      
    fluidRow(
      # ---- Box 2: Placeholder Content ----
      column(
        width = 12,
        card_header("Box 2 Title"),
        card_body(
          plotlyOutput("plot_elast")
        )
      ),
      
      # ---- Box 3: Placeholder Content ----
      column(
        width = 4,
        card(
          card_header("Box 3 Title"),
          card_body(
            "More placeholder text or any UI you want."
          )
        )
      )
    )
  ), 
  # --- Build Application Tab
  nav_panel(
    "REAMDE!",
    uiOutput("readme")
  )
)

service_title <- paste0("<span class='navbar-brand'>", service_title, "</span>")

ui <- tagList(
  tags$head(
    tags$title(browser_title),
    
    # Load the Cabin Sketch Google Font
    tags$link(
      href = "https://fonts.googleapis.com/css2?family=Ubuntu+Mono&display=swap", 
      rel = "stylesheet"
    ),
    
    # Add custom CSS for the title using the correct navbar-brand class
    tags$style(HTML("
    .navbar-brand {
      font-family: 'Ubuntu Mono', monospace;  /* Apply Ubuntu Mono to the title */
      font-size: 20px;  /* Adjust the font size as needed */
      color: #E69F00;  /* Adjust the title color if needed */
    }
      
      /* Override the sidebar width */
      div {
        --_sidebar-width: 400px !important;
      }
    "))
  ),
  app_nav_bar
)

