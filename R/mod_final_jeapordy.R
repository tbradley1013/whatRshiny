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
    uiOutput(ns("final_cat"))
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
      dplyr::filter(round == 3)
  })
  
  output$final_cat <- renderUI({
    category <- final_q()$category
    div(
      class = "final-j-div",
      div("Final Jeapordy!", style = "color:white;font-size:60px"),
      actionButton(
        inputId = ns("start_final"),
        label = category,
        class = "final-j-box"
      ),
      style = "width:50%;margin:0 auto;"
    )
  })
  
  observeEvent(input$start_final, {
    user_score <- rv$score
    dialog <- modalDialog(
      div(
        h3("Make your wager"),
        helpText("Max wager: ", user_score),
        numericInput(
          inputId = ns("final_wager"),
          label = "Wager",
          value = 0,
          min = 0, 
          max = user_score,
          step = 100,
          width = "100%"
        ),
        actionButton(
          inputId = ns("make_wager"),
          label = "Make Wager!"
        )
      ),
      footer = NULL,
      size = "l"
    )
    
    showModal(dialog)
  })
  
  observeEvent(input$make_wager, {
    removeModal()
    rv$q_value <- input$wager
    dialog <- modalDialog(
      h1(
        final_q()$clue
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
      size = "l",
      fade = FALSE
    )
    showModal(dialog)
  })
  
  observeEvent(input$submit_answer, {
    browser()
    
    value <- rv$q_value
    correct_answer <- final_q()$answer
    
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
        footer = NULL,
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
    
    # if (rv$score < 0){
    #   style = "color:red;"
    # } else if (rv$score > 0){
    #   style = "color:green;"
    # } else {
    #   style = "color:white;"
    # }
    
    dialog <- modalDialog(
      h1("Game Over!"),
      h3("Final Score:", span(rv$score)),
      actionButton(
        inputId = ns("replay"),
        label = "Play Again!",
        class = "btn-success"
      ),
      footer = NULL,
      size = "l",
      fade = FALSE
    )
    
    showModal(dialog)
  })
  
  observeEvent(input$replay, {
    removeModal()
    session$reload()
  })
}
    
## To be copied in the UI
# mod_final_jeapordy_ui("final_jeapordy_ui_1")
    
## To be copied in the server
# callModule(mod_final_jeapordy_server, "final_jeapordy_ui_1")
 
