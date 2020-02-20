#' @import shiny
app_ui <- function() {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    shinyjs::useShinyjs(),
    waiter::use_waiter(),
    waiter::waiter_show_on_load(custom_loading_spinner(), color = "#0C00A8"),
    # List the first level UI elements here 
    fluidPage(
      h1("Shiny Jeopardy!"),
      h3("Replay past Jeopardy games using the whatR and whatRshiny packages!"),
      div(
        id = "links-div",
        tags$a(
          icon("github"),
          href = "https://github.com/tbradley1013/whatRshiny"
        )
      ),
      div(
        id = "copyright",
        helpText('© "JEOPARDY!" is a registered trademark of Jeopardy Productions, Inc. License: All Rights Reserved.')
      ),
      div(
        class = "board",
        div(
          id = "categories-row",
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
        ),
        shinyjs::hidden(
          div(
            id = "final-jeapordy"
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
    tags$link(rel="stylesheet", type="text/css", href="www/styles.css"),
    tags$script(src = "www/button-click.js")
  )
}


custom_loading_spinner <- function(){
  tagList(
    h1("Loading you Jeapordy! game...", style = "margin-bottom:20px"),
    # waiter::spin_fading_circles()
    # waiter::spin_solar()
    waiter::spin_loaders(4, color = "#E5A561", style = "font-size:2em")
  )
}