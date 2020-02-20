#' @import shiny
app_server <- function(input, output,session) {
  session$onSessionEnded(stopApp)
  # List the first level callModules here
  rv <- reactiveValues(
    game = sample(1:6000, 1),
    round = 1,
    n = 0,
    score = 0
  )
  
  observe({
    isolate({
      rv$game_board <- whatr::whatr_board(game = rv$game)
      rv$clue_seq <- whatr::whatr_clues(game = rv$game)
    })
  })
  
  observe({
    req(rv$clue_seq)
    round_counts <- dplyr::count(rv$clue_seq, round)
    
    rv$n_round_1 <- round_counts$n[round_counts$round == 1]*2
    rv$n_round_2 <- round_counts$n[round_counts$round == 2]*2
  })
  
  observe({
    req(rv$n_round_1)
    
    if (rv$n >= rv$n_round_1){
      rv$round <- 2
    } else if (rv$n >= (rv$n_round_1 + rv$n_round_2)){
      rv$round <- 3
    }
    
  })
  
  
  game_info <- reactive({
    req(rv$game_board, rv$clue_seq)
    
    blank_board <- tidyr::crossing(row = 1:5, col = 1:6) %>% 
      dplyr::mutate(round = 1) %>% 
      dplyr::bind_rows(
        tidyr::crossing(row = 1:5, col = 1:6) %>% 
          dplyr::mutate(round = 2)
      ) %>% 
      dplyr::add_row(row = 0, col = 0, round = 3)
    
    out <- blank_board %>% 
      dplyr::left_join(rv$clue_seq, by = c("round", "row", "col")) %>% 
      dplyr::left_join(
        dplyr::select(rv$game_board, -n),
        by = "clue"
      )
    
    return(out)
  })
  
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
            label = value,
            class = "cat-box"
          )
        )
      )
    })
    
    return(tagList(cat_ui))
  })
  
  
  observe({
    req(game_info())
    
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
  })
  
  observe({
    if (rv$round == 1){
      shinyjs::show("round_1")
      shinyjs::hide("round_2")
    } else if (rv$round == 2){
      shinyjs::hide("round_1")
      shinyjs::show("round_2")
    } else {
      shinyjs::hide("round_1")
      shinyjs::hide("round_2")
    }
  })
  
  
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
  
}
