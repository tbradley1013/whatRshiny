#' @import shiny
app_server <- function(input, output,session) {
  session$onSessionEnded(stopApp)
 
  # defining initial reactive values ----
  rv <- reactiveValues(
    game_number = sample(1:6000, 1),
    round = 1,
    n = 0,
    score = 0,
    loaded = FALSE
  )
  
  
  # loading the data ----
  observe({
    rv$game = whatr::whatr_html(x = rv$game_number, out = "showgame")
    rv$scores = whatr::whatr_html(x = rv$game_number, out = "showscores")
  })
  
  
  observe({
    isolate({
      rv$game_board <- whatr::whatr_board(game = rv$game)
      # rv$clue_seq <- whatr_clues(html = rv$game)
      rv$game_info <- whatr::whatr_info(game = rv$game)
      rv$doubles <- whatr::whatr_doubles(game = rv$scores)
    })
  })
  
  output$game_info <- renderUI({
    req(rv$game_info)
    tagList(
      h5("Show Number:", rv$game_info$show),
      h5("Air Date:", format(rv$game_info$date, "%b %d, %Y"))
    )
  })
  
  shiny::outputOptions(output, "game_info", suspendWhenHidden = FALSE)
  
  # remove loading screen when board is ready ---
  observe({
    req(game_info(), rv$loaded)

    msgs <- c("THIS", "IS", "JEOPARDY!")

    for (i in 1:3){
      msg <- msgs[i]
      session$sendCustomMessage("update_intro_text", msg)
      Sys.sleep(1)
    }


    waiter::waiter_hide()
  })

  # Calculating round counts and current round -----
  observe({
    req(rv$game_board)
    # round_counts <- dplyr::count(rv$clue_seq, round)
    
    rv$n_round_1 <- max(rv$game_board$n[rv$game_board$round == 1])
    rv$n_round_2 <- max(rv$game_board$n[rv$game_board$round == 2] - (rv$n_round_1))
    cat("Round 1 Total =", rv$n_round_1, "\n")
    cat("Round 2 Total =", rv$n_round_2, "\n")
  })
  
  observe({
    req(rv$n_round_1)

    total <- rv$n_round_1 + rv$n_round_2
    cat("Total:", total, "\n")

    if (rv$n < rv$n_round_1){
      rv$round <- 1
    } else if (rv$n < total){
      rv$round <- 2
    } else if (rv$n >= total){
      rv$round <- 3
    }

  })
  
  observe({
    cat("Current n =", rv$n, "and round =", rv$round, "\n")
  })
  
  observe({
    if (rv$n == 15){
      dialog <- modalDialog(
        h3("Tell us a little about yourself!"),
        textInput(
          inputId = "user_name",
          label = "What's your name?",
          width = "100%"
        ),
        textInput(
          inputId = "user_fact",
          label = "I hear you have an interesting hobby...",
          width = "100%"
        ),
        actionButton(
          inputId = "user_info_submit",
          label = "Thanks Alex!",
          class = "btn-primary btn-lg"
        ),
        footer = NULL,
        size = "l"
      )
      
      showModal(dialog)
    }
  })
  
  observeEvent(input$user_info_submit, {
    removeModal()
  })
  
  # putting together all of the game info ----
  game_info <- reactive({
    req(rv$game_board)
    doubles <- rv$doubles$n
    
    blank_board <- tidyr::crossing(row = 1:5, col = 1:6) %>% 
      dplyr::mutate(round = 1) %>% 
      dplyr::bind_rows(
        tidyr::crossing(row = 1:5, col = 1:6) %>% 
          dplyr::mutate(round = 2)
      ) %>% 
      dplyr::add_row(row = 1, col = 1, round = 3)
    
    out <- blank_board %>% 
      # dplyr::left_join(rv$clue_seq, by = c("round", "row", "col")) %>% 
      dplyr::left_join(
        rv$game_board,
        by = c("round", "row", "col")
      ) %>% 
      dplyr::mutate(double = ifelse(n %in% doubles, TRUE, FALSE))
    
    return(out)
  })
  
  
  # Creating a rendering teh categories ------
  categories <- reactive({
    req(game_info())
    
    cats <- game_info() %>% 
      dplyr::distinct(round, col, category) %>% 
      dplyr::filter(!is.na(category))
    
    return(cats)
  })
  
  output$categories_ui <- renderUI({
    req(categories())
    
    cats <- categories() %>% 
      dplyr::filter(round == rv$round)
    
    cat_ui <- purrr::map(1:6, ~{
      value <- cats$category[cats$col == .x]
      
      tagList(
        shinyjs::disabled(
          actionButton(
            inputId = paste0("category_", .x),
            label = toupper(value),
            class = "cat-box"
          )
          # span(
          #   # inputId = paste0("category_", .x),
          #   id = paste0("category_", .x),
          #   class = "cat-box",
          #   # label = value
          #   value
          # )
        )
      )
    })
    
    return(tagList(cat_ui))
  })
  
  
  # Calling the box info for each question ----
  observe({
    req(game_info())
    
    isolate({
      purrr::map2(game_info()$row, game_info()$col, ~{
        id <- paste("ind_box_ui_1", .x, .y, sep = "_")
        
        callModule(
          mod_ind_box_server,
          id = id,
          session = session,
          game_info = game_info,
          selected_row = .x,
          selected_col = .y,
          selected_round = 1,
          rv = rv
        )
      })
      
      purrr::map2(game_info()$row, game_info()$col, ~{
        id <- paste("ind_box_ui_2", .x, .y, sep = "_")
        
        callModule(
          mod_ind_box_server,
          id = id,
          session = session,
          game_info = game_info,
          selected_row = .x,
          selected_col = .y,
          selected_round = 2,
          rv = rv
        )
      })
      
      callModule(mod_final_jeapordy_server, "final_jeapordy_ui_1", 
                 game_info = game_info, rv = rv)
    })
   
    
    rv$loaded <- TRUE
  })
  
  
  # changing what is shown as the game progresses and rounds change ----
  observe({
    if (rv$round == 1){
      shinyjs::show("round_1")
      shinyjs::hide("round_2")
      shinyjs::show("categories-row")
      shinyjs::hide("final-jeapordy")
    } else if (rv$round == 2){
      shinyjs::hide("round_1")
      shinyjs::show("round_2")
      shinyjs::show("categories-row")
      shinyjs::hide("final-jeapordy")
    } else if (rv$round == 3){
      shinyjs::hide("round_1")
      shinyjs::hide("round_2")
      shinyjs::hide("categories-row")
      shinyjs::show("final-jeapordy")
    }
  })
  
  # rendering the user score ----
  output$score <- renderUI({
    req(rv$score)
    
    if (rv$score < 0){
      style = "color:red;"
    } else if (rv$score > 0){
      style = "color:green;"
    } else {
      style = "color:white;"
    }
    
    tagList(
      div(
        "Score: ", 
        span(paste0("$", format(rv$score, big.mark = ",")), style = style),
        style = "position:absolute;top:10px;left:10px;font-size:2em"
      )
    )
  })
  
  # restart after final jeapordy
  # observe({
  #   req(rv$game_done)
  #   waiter::waiter_show(custom_loading_spinner())
  #   rv$game_number <- sample(1:6000, 1)
  #   rv$game_done <- FALSE
  #   rv$n <- 0
  #   rv$score <- 0
  #   rv$round <- 1
  #   isolate({
  #     rv$game = read_game(game = rv$game_number)
  #     rv$scores = read_game(game = rv$game_number)
  #     rv$game_board <- whatr_board(html = rv$game)
  #     rv$clue_seq <- whatr_clues(html = rv$game)
  #     rv$game_info <- whatr_info(html = rv$game, game = rv$game_number)
  #     rv$doubles <- whatr_doubles(html = rv$scores)
  #   })
  # })
  
}
