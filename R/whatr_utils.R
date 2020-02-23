
whatr_doubles <- function(html = NULL, game = NULL){
  if (!is.null(html)){
    data <- html 
  } else data <- whatr::read_scores(game = game)
  
  single_doubles <- data %>%
    rvest::html_node("#jeopardy_round > table td.ddred") %>%
    rvest::html_text() %>%
    base::as.integer()
  
  double_doubles <- data %>%
    rvest::html_nodes("#double_jeopardy_round > table td.ddred") %>%
    rvest::html_text() %>%
    base::as.integer() %>%
    base::unique()
  
  return(list(single = single_doubles, double = double_doubles))
}
