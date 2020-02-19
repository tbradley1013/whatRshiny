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
    
mod_ind_box_server <- function(input, output, session, game_info, selected_row, selected_col, selected_round, rv){
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
      out <- shinyjs::disabled(
        actionButton(
          inputId = ns("question_box"),
          label = value,
          class = "q-box"
        )
      )
    } else {
      value <- paste0("$", get_value(selected_row))
      out <- actionButton(
        inputId = ns("question_box"),
        label = value,
        class = "q-box"
      )
    }
    
    
    return(out)
    
  })
  
  observeEvent(input$question_box, {
    updateActionButton(session = session, inputId = "question_box", label = "")
    shinyjs::disable("question_box")
    
    dialog <- modalDialog(
      h5(
        question()$clue
      ),
      textInput(
        inputId = "user_answer",
        label = "Answer",
      ),
      div(
        actionButton(
          inputId = ns("submit_answer"),
          label = "Answer",
          width = "47%"
        ),
        actionButton(
          inputId = ns("stay_silent"),
          label = "Stay Silent",
          width = "47%"
        ),
        style = "width:300px;margin:0 auto;"
      ),
      footer = NULL
      # footer = modalButton("Cancel")
    )
    
    showModal(dialog)
  })
  
  
  observeEvent(input$stay_silent, {
    removeModal()
    
    rv$n <- rv$n + 1
  })
  
  observeEvent(input$submit_answer, {
    removeModal()
    
    rv$n <- rv$n + 1
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
 
