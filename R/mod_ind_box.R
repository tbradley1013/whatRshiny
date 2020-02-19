# Module UI
  
#' @title   mod_ind_box_ui and mod_ind_box_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_ind_box
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_ind_box_ui <- function(id){
  ns <- NS(id)
  uiOutput(ns("block"), inline = TRUE, class = "questions")
}
    
# Module Server
    
#' @rdname mod_ind_box
#' @export
#' @keywords internal
    
mod_ind_box_server <- function(input, output, session, r, row, col, round){
  ns <- session$ns
  
  question <- reactive({
    req(r$clue_seq)
    # browser()
    
    dplyr::filter(r$clue_seq, round == round, row == row, col == col)
  })
  
  output$block <- renderUI({
    req(question())
    
    info <- r$game_board[r$game_board$n == question()$n, ]
    
    value <- get_value(row)
    
    tags$span(
      class = "question-cell",
      style = "width:200px;",
      tags$a(
        id = ns("question_box"),
        href = "#",
        tags$span(
          class = "question-box", 
          paste0("$", value)
        )
      )
    )
  })
  
  observeEvent(input$question_box, {
    dialog <- modalDialog(
      title = "Question",
      footer = modalButton()
    )
    
    showModal(dialog)
  })
}

get_value <- function(row){
  switch(
    row,
    `1` = 200,
    `2` = 400, 
    `3` = 600, 
    `4` = 800,
    `5` = 1000
  )
} 
    
## To be copied in the UI
# mod_ind_box_ui("ind_box_ui_1")
    
## To be copied in the server
# callModule(mod_ind_box_server, "ind_box_ui_1")
 
