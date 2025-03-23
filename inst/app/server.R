# Define server logic ----
shinyServer(function(input, output, session) {
  
  # Load shinyjs
  shinyjs::useShinyjs()
  
  # Send app_mode to the client as a JavaScript variable
  # Update the hidden input with the value of app_mode
  observe({
    updateTextInput(session, "app_mode_js", value = app_mode)
  })
  
})
