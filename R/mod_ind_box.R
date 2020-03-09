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
  
  
  initial_n <- reactive({rv$n})
  isolate(initial_n)
  
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
    value <- get_value(selected_row, selected_round)
    
    if (question()$double){
      max_value <- max(1000*selected_round, rv$score)
      dialog <- modalDialog(
        h1("Daily Double!"),
        numericInput(
          inputId = ns("wager"),
          label = "What's your wager",
          value = max_value,
          max = max_value,
          min = 0,
          step = 100,
          width = "100%"
        ),
        actionButton(
          inputId = ns("submit_wager"),
          label = "Make Wager!"
        ),
        footer = NULL,
        size = "l"
      )
      
      showModal(dialog)
    } else {
      rv$q_value <- value
      dialog <- modalDialog(
        div(
          toupper(question()$clue),
          class = "clue-text"
        ),
        br(),br(),br(),
        div(
          class = "answer-div",
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
          shinyjs::disabled(
            shinyjs::hidden(
              actionButton(
                inputId = ns("submit_answer"),
                label = "Submit Answer",
                width = "100%",
                class = "btn-success"
              ) 
            )),
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
      
    }
    
    
  })
  
  observeEvent(input$submit_wager, {
    rv$q_value <- input$wager
    
    dialog <- modalDialog(
      h1(
        question()$clue
      ),
      br(),br(),br(),
      div(
        class = "answer-div",
        textInput(
          inputId = ns("user_answer"),
          label = "Answer"
        ),
        style = "width:300px;margin:0 auto;"
      ),
      div(
        actionButton(
          inputId = ns("submit_answer"),
          label = "Submit Answer",
          width = "100%",
          class = "btn-success"
        ) ,
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
    correct_answer <- question()$answer
    
    dialog <- modalDialog(
      h3(
        "Correct Answer:", correct_answer
      ),
      div(
        actionButton(
          ns("close_confirm"),
          "Close",
          width = "100%"
        ),
        style = "width:150px;margin:0 auto;"
      ),
      footer = NULL,
      fade = FALSE,
      size = "l"
    )
    
    showModal(dialog)
    # for some reason these button clicks happen twice everytime 
    # they are clicked
  })
  
  observeEvent(input$buzz_in, {
    shinyjs::hide("buzz_in")
    shinyjs::disable("buzz_in")
    shinyjs::hide("stay_silent")
    shinyjs::disable("stay_silent")
    shinyjs::show("user_answer")
    shinyjs::show("submit_answer")
    shinyjs::enable("submit_answer")
    session$sendCustomMessage(type="refocus",message=list(NULL))
  })
  
  observeEvent(input$submit_answer, {
    value_label <- paste0("value_", selected_row, "_", selected_col, "_", selected_round)
    
    value <- rv$q_value
    correct_answer <- question()$answer
    
    answer_stringdist <- stringdist::stringdist(tolower(correct_answer), tolower(input$user_answer))
    
    if (stringr::str_detect(tolower(correct_answer), tolower(input$user_answer))){
      if (nchar(input$user_answer) <= 2){
        is_correct <- FALSE
      } else is_correct <- TRUE
    } else if (answer_stringdist < 3) {
      is_correct <- TRUE
    } else {
      is_correct <- FALSE
    }
    
    if (is_correct){
      dialog <- modalDialog(
        h1(
          "You have answered correctly!"
        ),
        h3(
          "Correct Answer:", correct_answer
        ),
        h3(
          "Your Answer:", 
          span(input$user_answer, style = "color:green")
        ),
        div(
          actionButton(
            ns("close_confirm"),
            "Close",
            width = "100%"
          ),
          style = "width:150px;margin:0 auto;"
        ),
        title = "Correct!",
        footer = NULL,
        fade = FALSE,
        size = "l"
      )
      
      if (is.null(rv[[value_label]])){
        rv$score <- rv$score + (value)
        rv[[value_label]] <- TRUE
      }
      
    } else {
      dialog <- modalDialog(
        h1(
          "You have answered incorrectly!"
        ),
        h3(
          "Correct Answer:", correct_answer
        ),
        h3(
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
        footer = NULL,
        fade = FALSE,
        size = "l"
      )
      
      if (is.null(rv[[value_label]])){
        rv$score <- rv$score - (value)
        rv[[value_label]] <- TRUE
      }
    }
    
    
    removeModal()
    showModal(dialog)
    # for some reason these button clicks happen twice everytime 
    # they are clicked
    
  })
  
  
  observeEvent(input$close_confirm, {
    question_label <- paste0("question_", selected_row, "_", selected_col, "_", selected_round)
    
    if (is.null(rv[[question_label]])){
      rv$n <- rv$n + 1
      rv[[question_label]] <- TRUE
    }
    
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
 
