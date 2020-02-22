# Module UI
  
#' @title   mod_final_jeapordy_ui and mod_final_jeapordy_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_final_jeapordy
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_final_jeapordy_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput("final_cat")
  )
}
    
# Module Server
    
#' @rdname mod_final_jeapordy
#' @export
#' @keywords internal
    
mod_final_jeapordy_server <- function(input, output, session, game_info, rv){
  ns <- session$ns
  final_q <- reactive({
    game_info() %>% 
      filter(round == 3)
  })
  
  output$final_cat <- renderUI({
    
  })
}
    
## To be copied in the UI
# mod_final_jeapordy_ui("final_jeapordy_ui_1")
    
## To be copied in the server
# callModule(mod_final_jeapordy_server, "final_jeapordy_ui_1")
 
