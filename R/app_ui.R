#' @import shiny
app_ui <- function() {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    shinyjs::useShinyjs(),
    waiter::use_waiter(),
    waiter::waiter_show_on_load(custom_loading_spinner(), color = "#0C00A8"),
    fluidPage(
      h1("SHINY JEOPARDY!"),
      h3("Replay past JEOPARDY! games using the whatR and whatRshiny packages!"),
      uiOutput("game_info"),
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
          uiOutput("categories_ui", inline = TRUE, class = "categories")
        ),
        div(
          id = "round_1",
          class = "round-div",
          purrr::map(1:5, function(z){
            div(
              class = "board-row",
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
                purrr::map(1:6, ~{
                  mod_ind_box_ui(id = paste("ind_box_ui_2", z, .x, sep = "_"))
                })
              )
            })
          )
        ),
        shinyjs::hidden(
          div(
            id = "final-jeapordy",
            mod_final_jeapordy_ui("final_jeapordy_ui_1")
          )
        )
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
    tags$link(href="https://fonts.googleapis.com/css?family=Questrial&display=swap", rel="stylesheet"),
    tags$link(href="https://fonts.googleapis.com/css?family=Libre+Baskerville&display=swap", rel="stylesheet"),
    tags$link(rel="stylesheet", type="text/css", href="www/styles.css"),
    tags$script(src = "www/button-click.js"),
    tags$script(src = "www/update-intro-text.js")
  )
}


custom_loading_spinner <- function(){
  tagList(
    h1(
      "Loading your JEOPARDY! game...", 
      style = "margin-bottom:20px;margin-top:-50px;", 
      class = "intro-text"
    ),
    waiter::spin_loaders(4, color = "#E5A561", style = "font-size:2em")
  )
}