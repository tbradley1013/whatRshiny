#' @import shiny
app_server <- function(input, output,session) {
  session$onSessionEnded(stopApp)
  # List the first level callModules here
  rv <- reactiveValues(
    game = sample(1:6000, 1)
  )
  
  observe({
    rv$game_board <- whatr::whatr_board(game = rv$game)
    rv$clue_seq <- whatr::whatr_clues(game = rv$game)
  })
  
  categories <- reactive({
    req(rv$game_board)
    
    cats <- rv$game_board %>% 
      distinct(round, col, category)
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
      dplyr::filter(round == 1)
    
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
    # browser()
    # ids <- paste(rv$clue_seq$row, rv$clue_seq$col, sep = "_")
    # ids <- ids[ids != "0_0"]
    req(game_info())
    
    purrr::map2(game_info()$row, game_info()$col, ~{
      id <- paste("ind_box_ui", .x, .y, sep = "_")

      callModule(
        mod_ind_box_server,
        id = id,
        session = session,
        game_info = game_info,
        selected_row = .x,
        selected_col = .y,
        selected_round = 1
      )
    })
  })
  
  # callModule(mod_ind_box_server, "ind_box_ui_1", session = session,
  #            r = rv, row = 1, col = 1, round = 1)
  
  
  
}
