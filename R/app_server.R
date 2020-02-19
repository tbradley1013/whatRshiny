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
  
  observe({
    # browser()
    # ids <- paste(rv$clue_seq$row, rv$clue_seq$col, sep = "_")
    # ids <- ids[ids != "0_0"]
    
    purrr::map2(rv$clue_seq$row, rv$clue_seq$col, ~{
      id <- paste("ind_box_ui", .x, .y, sep = "_")

      callModule(
        mod_ind_box_server,
        id = id,
        session = session,
        r = rv,
        row = .x,
        col = .y,
        round = 1
      )
    })
  })
  
  # callModule(mod_ind_box_server, "ind_box_ui_1", session = session,
  #            r = rv, row = 1, col = 1, round = 1)
  
  
  
}
