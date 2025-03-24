# Example data placeholders (replace with your actual data)
my_lambdas <- data.frame(lambda_user = c("0.7 (high)", "0.5 (medium)", "0.3 (low)"))
min_price_change <- -10
max_price_change <- 10

imperial_logo <- paste0(
  "<img src='business-school-logo.png' ",
  "height='40px' ",
  "style='position: absolute; left: 20px; top: 10px; z-index: 0;' />"
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
)

browser_title <- "> r&ma"
service_title <- tags$a(
  "> SKU dashboard",
  href = "https://github.com/l-ramirez-lopez/imperial_retail_project",
  target = "_blank",
  style = "color: #E69F00; text-decoration: none;"
)

app_nav_bar <- page_navbar(
  title = service_title,
  theme = app_theme,
  
  nav_panel(
    "Overview",
    fluidRow(
      column(
        width = 4,
        card(
          card_header("Average weekly sales by product functionality"),
          card_body(
            plotlyOutput("my_overview")
          )
        )
      )
    )
  ),
  
  nav_panel(
    "Demand forecasting",
    uiOutput("demand_forecasting")
  ),
  
  nav_panel(
    "Feature promotion",
    fluidRow(
      column(
        width = 4,
        card(
          card_header("Feature promotion impact: Sales comparison"),
          card_body(plotlyOutput("plot_sales"))
        )
      ), 
      column(
        width = 4,
        card(
          card_header("Incremental sales from feature promotion"),
          card_body(plotlyOutput("plot_incremental"))
        )
      ), 
      column(
        width = 4,
        card(
          card_header("Incremental revenue from feature promotion"),
          card_body(plotlyOutput("plot_revenue"))
        )
      )
    )
  ),
  
  nav_panel(
    "Price elasticity and scenario testing",
    fluidRow(
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
              value = 0,
              step = 1
            ),
            selectInput(
              inputId = "my_variable", 
              label = "Choose a variable to be shown:",
              choices = c("Price", "Revenue", "Sales"), 
              selected = "Price"
            )
          )
        )
      )
    ),
    fluidRow(
      column(
        width = 4,
        card(
          card_header("SKU price elasticity"),
          dataTableOutput("my_elasticity", width = "70%")
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        card_header("Box 2 Title"),
        card_body(plotlyOutput("plot_elast"))
      ),
      column(
        width = 4,
        card(
          card_header("Box 3 Title"),
          card_body("More placeholder text or any UI you want.")
        )
      )
    )
  ), 
  nav_panel(
    "REAMDE!",
    uiOutput("readme")
  )
)

service_title <- paste0("<span class='navbar-brand'>", service_title, "</span>")

ui <- tagList(
  tags$head(
    tags$title(browser_title),
    
    # Load the Ubuntu Mono Google Font
    tags$link(
      rel = "stylesheet",
      href = "https://cdn.materialdesignicons.com/5.4.55/css/materialdesignicons.min.css"
    ),
    
    # Custom CSS for pinned bars and spacing adjustments
    tags$style(HTML("
      /* Style for the top stripe */
      .custom-top-bar {
        background-color: #101010;
        color: #B3B3B3;
        padding: 10px 20px;
        position: fixed;
        top: 0;
        width: 100%;
        z-index: 9999;
      }
      
      /* Style for the bottom stripe */
      .custom-bottom-bar {
        background-color: #101010;
        color: #B3B3B3;
        padding: 10px 20px;
        position: fixed;
        bottom: 0;
        width: 100%;
        z-index: 9999;
      }
      
      /* Additional body padding to avoid overlapping by fixed elements */
      body {
        padding-top: 80px;    /* Increase if the top stripe is taller */
        padding-bottom: 60px; /* For the bottom stripe */
      }
    "))
  ),
  
  # Top stripe
  tags$div(
    class = "custom-top-bar",
    fluidRow(
      column(
        width = 4,
        HTML(paste0(imperial_logo))
      ),
      column(
        width = 4,
        tags$p("Retail and marketing analytics - Final project", style = "text-align: center; margin: 0;")
      ),
      column(
        width = 4,
        tags$p("Group 01", style = "text-align: right; margin: 0;")
      )
    )
  ),
  
  # Wrap the navbar in a container that applies a margin-top
  tags$div(
    style = "margin-top: 50px;",  #/* This pushes the navbar down below the top stripe */
      app_nav_bar
  ),
  
  # Bottom stripe
  tags$div(
    class = "custom-bottom-bar",
    fluidRow(
      column(
        width = 4,
        tags$a(
          href = "https://github.com/l-ramirez-lopez/imperial_retail_project",
          target = "_blank",
          tags$i(
            class = "mdi-github mdi v-icon notranslate",
            style = "font-size: 24px; height: 24px; width: 24px;"
          )
        )
      ),
      column(
        width = 4,
        tags$p("Some middle text", style = "text-align: center; margin: 0;")
      ),
      column(
        width = 4,
        tags$p("Contact Info", style = "text-align: right; margin: 0;")
      )
    )
  )
)
