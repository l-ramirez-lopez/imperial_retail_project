# Define server logic ----
shinyServer(function(input, output, session) {
  
  # Load shinyjs
  shinyjs::useShinyjs()
  
  observe({
    output$readme <- renderUI({
      ui_data_file()
    })
  }, priority = -2000) 
  
  output$my_overview <- renderPlotly({
    ggplotly(aa)
  })

  
  ####################################################################
  ## --- PRICE ELASTICITY ----
  ####################################################################
  selected_data <- reactive({
    req(input$my_dropdown_elast)
    if (input$my_dropdown_elast == "0.3 (low)") {
      rs <- list(mdata = df_lambda03, melasticity = elasticity_optim_03)
    } else if (input$my_dropdown_elast == "0.5 (moderate)") {
      rs <-list(mdata = df_lambda05, melasticity = elasticity_optim_05)
    } else {
      rs <-list(mdata = df_lambda07, melasticity = elasticity_optim_07)
    }
    rs$melasticity <- rs$melasticity[
      order(rs$melasticity$elasticity, decreasing = TRUE), 
    ]
    rs
  })
  

  output$my_elasticity <- renderDataTable({
    dat <- selected_data()
    melasticity <- dat$melasticity[order(dat$melasticity$elasticity, decreasing = TRUE), ]
    colnames(melasticity) <- c("SKU", "Competitor elasticity", "Elasticity", "MAPE")
    
    DT::datatable(
      melasticity,
      rownames = FALSE,
      options = list(
        pageLength = 44,
        autoWidth = TRUE,
        searching = FALSE,
        columnDefs = list(
          list(width = '150px', targets = "_all")
        )
      ),
      # Use DT’s own selection approach
      selection = list(
        mode = "multiple",  
        selected = 1       
      ),
      class = 'stripe hover order-column'
    ) %>%
      DT::formatRound(
        columns = colnames(melasticity)[-1],
        digits = 3
      )
  })
  
  tables_elasticity <- reactive({
    # Render a simple random table
    
    req(input$my_price_change)
    dat <- selected_data()
    
    # We can only do the calculation if the table has at least row 1
    # (i.e., if user hasn't unselected all rows, or hasn't changed anything yet).
    # input$my_elasticity_rows_selected is available here
    if (length(input$my_elasticity_rows_selected) == 0) return()
    
    result_list <- calculate_ranges_for_skus(
      mdata = dat$mdata,
      selected_rows = input$my_elasticity_rows_selected,
      my_price_change = input$my_price_change,
      my_dropdown_elast = input$my_dropdown_elast,
      my_variable = input$my_variable,
      my_lambdas = my_lambdas,
      my_variables = my_variables,
      sku_price_change_lims = sku_price_change_lims,
      melasticity = dat$melasticity,
      get_sales_revenue_change = get_sales_revenue_change
    )
    result_list
  }) |> bindEvent(
    list(input$my_price_change, input$my_elasticity_rows_selected)
  )
  
  observe({
    mtablese <- tables_elasticity()
    req(mtablese)
    output$plot_elast <- renderPlotly({
      to_plot <- mtablese[[tolower(input$my_variable)]]
      to_plot$sku <- paste0("SKU: ", to_plot$sku)
      
      # 2) Now define the factor with those same values
      to_plot$sku <- factor(to_plot$sku, levels = unique(to_plot$sku))
      
      melasplot <- ggplot(to_plot, aes(x = date, y = x)) +
        # Ribbon for min-max
        geom_ribbon(aes(ymin = min, ymax = max), fill = "dodgerblue", alpha = 0.2) +
        # Line for the main 'x' value
        geom_line() +
        # Facet by SKU, with up to 4 columns
        labs(
          x = "Date",
          y = input$my_variable  # or a string like "My Variable" if not reactive
        ) +
        facet_wrap(sku~., scales = "free_y") +
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
      
      plotly_elast <- ggplotly(melasplot)
      plotly_elast 
      # # Inspect the layout to see the facet domains
      # layout(plotly_elast)
      # 
      # # You would then loop through the facets (subplots) in plotly_elast$x$layout
      # # and assign each a uniform domain. For example:
      # for(i in seq_along(plotly_elast$x$layout)){
      #   # Hypothetically, set a uniform y-domain for each subplot
      #   # Note: Actual code here depends on how facets are arranged.
      #   plotly_elast$x$layout[[paste0("yaxis", i)]]$domain <- c(0, 0.25) 
      # }
      # plotly_elast <- layout(plotly_elast, paper_bgcolor = 'rgba(0,0,0,0)', plot_bgcolor = 'rgba(0,0,0,0)')
      
    })
  })
  ####################################################################
  ## --- FEATURE PROMOTION ----
  ####################################################################
  
  observe({
    scanpro_results <- run_feature_scanpro_model(datar_w_cp)
    promotion_analysis <- plot_feature_promotion_impact(scanpro_results, datar_w_cp)
    psales <- ggplotly(promotion_analysis$sales_chart)
    pincremental <- ggplotly(promotion_analysis$incremental_chart)
    prevenue <- ggplotly(promotion_analysis$revenue_chart)
    
    output$plot_sales <- renderPlotly({psales})
    output$plot_incremental <- renderPlotly({pincremental})
    output$plot_revenue <- renderPlotly({prevenue})
  })
  
})
