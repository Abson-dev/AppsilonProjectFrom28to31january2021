if (interactive()){
  source("server.R")
  source("ui.R")
  # Create Shiny object
  shinyApp(ui = ui, server = server)
}
