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

      value <- paste0("$", get_value(selected_row, selected_round))

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
      h3(
        question()$clue
      ),
      div(
        shinyjs::hidden(
          textInput(
            inputId = ns("user_answer"),
            label = "Answer"
          ) 
        ),
        style = "width:300px;margin:0 auto;"
      ),
      div(
        actionButton(
          inputId = ns("buzz_in"),
          label = "Buzz In!",
          width = "47%",
          class = "btn-primary"
        ),
        shinyjs::hidden(
          actionButton(
            inputId = ns("submit_answer"),
            label = "Submit Answer",
            width = "100%",
            class = "btn-success"
          ) 
        ),
        actionButton(
          inputId = ns("stay_silent"),
          label = "Stay Silent",
          width = "47%",
          class = "btn-danger"
        ),
        style = "width:300px;margin:0 auto;"
      ),
      footer = NULL,
      size = "l"
      # footer = modalButton("Cancel")
    )
    
    showModal(dialog)
  })
  
  
  observeEvent(input$stay_silent, {
    removeModal()
    
    # for some reason these button clicks happen twice everytime 
    # they are clicked
    rv$n <- rv$n + 1
  })
  
  observeEvent(input$buzz_in, {
    shinyjs::hide("buzz_in")
    shinyjs::hide("stay_silent")
    shinyjs::show("user_answer")
    shinyjs::show("submit_answer")
  })
  
  observeEvent(input$submit_answer, {
    # browser()
    
    value <- get_value(selected_row, selected_round)
    correct_answer <- question()$answer
    
    answer_stringdist <- stringdist::stringdist(correct_answer, input$user_answer)
    
    if (stringr::str_detect(stringr::str_to_lower(correct_answer), stringr::str_to_lower(input$user_answer))){
      is_correct <- TRUE
    } else if (answer_stringdist < 3) {
      is_correct <- TRUE
    } else {
      is_correct <- FALSE
    }
    
    if (is_correct){
      dialog <- modalDialog(
        div(
          "You have answered correctly!"
        ),
        div(
          "Correct Answer:", correct_answer
        ),
        div(
          "Your Answer:", 
          span(input$user_answer, style = "color:green")
        ),
        div(
          actionButton(
            "close_confirm",
            "Close",
            width = "100%"
          ),
          style = "width:150px;margin:0 auto;"
        ),
        title = "Correct!",
        fade = FALSE,
        size = "l"
      )
      
      rv$score <- rv$score + (value/2)
    } else {
      dialog <- modalDialog(
        div(
          "You have answered incorrectly!"
        ),
        div(
          "Correct Answer:", correct_answer
        ),
        div(
          "Your Answer:", 
          span(input$user_answer, style = "color:red")
        ),
        div(
          actionButton(
            ns("close_confirm"),
            "Close",
            width = "100%"
          ),
          style = "width:150px;margin:10px auto;"
        ),
        title = "Oh Sorry!",
        fade = FALSE,
        size = "l"
      )
      
      rv$score <- rv$score - (value/2)
    }
    
    rv$n <- rv$n + 1
    
    removeModal()
    showModal(dialog)
    # for some reason these button clicks happen twice everytime 
    # they are clicked
    
  })
  
  
  observeEvent(input$close_confirm, {
    removeModal()
  })
  
  
}

get_value <- function(row, round){
  out <- switch(
    row,
    `1` = 200,
    `2` = 400, 
    `3` = 600, 
    `4` = 800,
    `5` = 1000
  )
  
  out <- out*round
  return(out)
} 
    
## To be copied in the UI
# mod_ind_box_ui("ind_box_ui_1")
    
## To be copied in the server
# callModule(mod_ind_box_server, "ind_box_ui_1")
 
