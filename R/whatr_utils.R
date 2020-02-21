whatr_id <- function(game = NULL, date = NULL, show = NULL) {
  if (is.null(game)) {
    else_arg <- c(!is.null(date), !is.null(show))
    type <- c("date", "show")[else_arg]
    input <- c(date, show)
    response <- httr::HEAD(
      url = "http://www.j-archive.com/search.php",
      query = list(search = paste(type, input, sep = ":"))
    )
    game <- stringr::str_extract(response$url, "\\d+$")
  }
  return(as.character(game))
}

read_game <- function(game = NULL, date = NULL, show = NULL) {
  response <- httr::GET(
    url = "http://www.j-archive.com/showgame.php",
    query = list(game_id = whatr_id(game, date, show))
  )
  showgame <- httr::content(response)
  return(showgame)
}

whatr_board <- function (game = NULL, date = NULL, show = NULL){
  data <- read_game(game, date, show)
  categories <- data %>% 
    rvest::html_nodes("table td.category_name") %>% 
    rvest::html_text(trim = TRUE) %>% 
    stringr::str_to_title() %>% 
    stringr::str_replace_all("\"", "'") %>%
    tibble::enframe(name = NULL, value = "category") %>% 
    dplyr::mutate(round = c(rep(1L, 6), rep(2L, 6), 3L), col = c(1L:6L, 1L:6L, 0L)) %>% 
    dplyr::select(round, col, category)
  single_order <- data %>% 
    rvest::html_nodes("#jeopardy_round > table td.clue_order_number") %>% 
    rvest::html_text() %>% 
    base::as.integer()
  double_order <- data %>% 
    rvest::html_nodes("#double_jeopardy_round > table td.clue_order_number") %>% 
    rvest::html_text() %>% 
    base::as.integer()
  order <- data %>% 
    rvest::html_nodes("table tr td div") %>% 
    rvest::html_attr("onmouseover") %>% 
    stringr::str_extract("(?<=clue_)(.*)(?=_stuck)") %>% 
    tibble::enframe(name = NULL) %>%
    tidyr::separate(col = value, sep = "', '", into = c("one", "two")) %>%
    dplyr::select(one) %>% 
    tidyr::separate(col = one, sep = "_", into = c("round", "col", "row"), convert = TRUE, fill = "right") %>% 
    dplyr::mutate(round = as.integer(round %>% dplyr::recode(J = "1", DJ = "2", FJ = "3")), n = c(single_order, double_order + 
                                                                                           max(single_order), max(double_order) + 1L))
  order$row[length(order$row)] <- 0
  order$col[length(order$col)] <- 0
  extract_answer <- function(node) {
    node %>% 
      rvest::html_attr("onmouseover") %>% 
      xml2::read_html() %>% 
      rvest::html_nodes("em.correct_response") %>% 
      rvest::html_text()
  }
  clues <- data %>% 
    rvest::html_nodes("table td.clue_text") %>% 
    rvest::html_text() %>% 
    stringr::str_to_title() %>% 
    stringr::str_replace_all("\"", "'") %>% 
    tibble::enframe(name = NULL, value = "clue") %>% 
    dplyr::bind_cols(order) %>% 
    dplyr::select(round, col, row, n, clue)
  final_answer <- data %>% 
    rvest::html_node(".final_round") %>% 
    base::as.character() %>% 
    stringr::str_split("class") %>% 
    base::unlist() %>% 
    magrittr::extract(stringr::str_which(., "correct_response")) %>% 
    stringr::str_extract(";&gt;(.*)&lt;/") %>% 
    stringr::str_remove(";&gt;") %>% 
    stringr::str_remove("&lt;/") %>% 
    stringr::str_to_title()
  answers <- data %>%
    rvest::html_nodes("table tr td div") %>% 
    purrr::map(extract_answer) %>%
    base::unlist() %>% 
    stringr::str_to_title() %>% 
    stringr::str_replace_all("\"", "'") %>% 
    stringr::str_remove_all(stringr::fixed("\\")) %>% 
    tibble::enframe(name = NULL, value = "answer") %>%
    dplyr::add_row(answer = final_answer) %>% 
    dplyr::bind_cols(order) %>% 
    dplyr::select(round, col, row, n, answer)
  categories %>% 
    dplyr::left_join(clues, by = c("round", "col")) %>% 
    dplyr::left_join(answers, by = c("round", "col", "row", "n")) %>% 
    dplyr::select(n, category, clue, answer)
}

whatr_clues <- function(game = NULL, date = NULL, show = NULL) {
  # data <- showgame(game, date, show)
  data <- read_game(game, date, show)
  data %>%
    rvest::html_nodes("table td.clue_text") %>%
    rvest::html_text() %>%
    stringr::str_to_title() %>%
    stringr::str_replace_all("\"", "\'") %>%
    tibble::enframe(name = NULL, value = "clue") %>%
    dplyr::bind_cols(whatr_order(game)) %>%
    dplyr::select(round, col, row, n, clue)
}

whatr_order <- function(game = NULL, date = NULL, show = NULL) {
  # data <- showgame(game, date, show)
  data <- read_game(game, date, show)
  single_order <- data %>%
    rvest::html_nodes("#jeopardy_round > table td.clue_order_number") %>%
    rvest::html_text() %>%
    base::as.integer()
  double_order <- data %>%
    rvest::html_nodes("#double_jeopardy_round > table td.clue_order_number") %>%
    rvest::html_text() %>%
    base::as.integer()
  order <- data %>%
    rvest::html_nodes("table tr td div") %>%
    rvest::html_attr("onmouseover") %>%
    stringr::str_extract("(?<=clue_)(.*)(?=_stuck)") %>%
    tibble::enframe(name = NULL) %>%
    tidyr::separate(
      col = value,
      sep = "', '",
      into = c("one", "two")
    ) %>%
    dplyr::select(one) %>%
    tidyr::separate(
      col = one,
      sep = "_",
      into = c("round", "col", "row"),
      convert = TRUE,
      fill = "right"
    ) %>%
    dplyr::mutate(
      round = as.integer(
        round %>%
          dplyr::recode(
            "J"  = "1",
            "DJ" = "2",
            "FJ" = "3"
          )
      ),
      n = c(
        single_order,
        double_order + max(single_order),
        max(single_order) + max(double_order) + 1L
      )
    )
  order$row[length(order$row)] <- 0
  order$col[length(order$col)] <- 0
  return(order)
}
