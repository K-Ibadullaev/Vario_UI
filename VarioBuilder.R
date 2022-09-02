library(shiny)
source('VarGUI.R', local = TRUE)
source('VarServer.R')


shinyApp(
  ui = VarGUI,
  server = VarServer
)