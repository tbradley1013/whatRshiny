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
    
mod_ind_box_server <- function(input, output, session, game_info, selected_row, selected_col, selected_round){
  ns <- session$ns
  
  question <- reactive({
    req(game_info())
    # browser()
    
    dplyr::filter(game_info(), round == selected_round, row == selected_row, col == selected_col)
  })
  
  output$block <- renderUI({
    req(question())
    
    if (is.na(question()$n)){
      value <- ""
    } else {
      value <- paste0("$", get_value(selected_row))
    }
    
    actionButton(
      inputId = ns("question_box"),
      label = value,
      class = "q-box"
    )
  })
  
  observeEvent(input$question_box, {
    updateActionButton(session, inputId = ns("question_box"), label = "")
    
    dialog <- modalDialog(
      h5(
        question()$clue
      ),
      textAreaInput(
        inputId = "user_answer",
        label = "Answer",
      ),
      div(
        actionButton(
          inputId = "submit_answer",
          label = "Answer",
        )
      ),
      footer = modalButton("Cancel")
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
 
