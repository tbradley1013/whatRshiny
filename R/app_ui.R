#' @import shiny
app_ui <- function() {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    shinyjs::useShinyjs(),
    # List the first level UI elements here 
    fluidPage(
      h1("whatRshiny"),
      div(
        class = "board",
        div(
          class = "board-row",
          uiOutput("categories_ui", inline = TRUE)
        ),
        div(
          id = "round_1",
          class = "round-div",
          purrr::map(1:5, function(z){
            div(
              class = "board-row",
              # display = "table-row",
              purrr::map(1:6, ~{
                mod_ind_box_ui(id = paste("ind_box_ui_1", z, .x, sep = "_"))
              })
            )
          })
        ),
        shinyjs::hidden(
          div(
            id = "round_2",
            class = "round-div",
            purrr::map(1:5, function(z){
              div(
                class = "board-row",
                # display = "table-row",
                purrr::map(1:6, ~{
                  mod_ind_box_ui(id = paste("ind_box_ui_2", z, .x, sep = "_"))
                })
              )
            })
          )
        )
        # display = "table",
        
        
        # mod_ind_box_ui("ind_box_ui_1")
      ),
      uiOutput("score")
    )
  )
}

#' @import shiny
golem_add_external_resources <- function(){
  
  addResourcePath(
    'www', system.file('app/www', package = 'whatRshiny')
  )
 
  tags$head(
    golem::activate_js(),
    golem::favicon(),
    # Add here all the external resources
    # If you have a custom.css in the inst/app/www
    # Or for example, you can add shinyalert::useShinyalert() here
    tags$link(rel="stylesheet", type="text/css", href="www/styles.css")
  )
}
