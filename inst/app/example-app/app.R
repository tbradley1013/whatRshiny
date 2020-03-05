# devtools::install_github("tbradley1013/whatRshiny")
library(whatRshiny)
library(shiny)

shinyApp(ui = whatRshiny:::app_ui(), server = whatRshiny:::app_server)