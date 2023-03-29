#' Using quantedas kwic to search for the keyword in context!
#'
#' @param tokens tokens object created with convert_to_tokens
#' @param pattern regex to search for
#' @param ... additional parameters passed to kwic!
#' @param window amount of surrounding tokens to get as context
#'
#' @return searching for a single keyword
search_for_kwic <- function(tokens, pattern, window = 10, ...) {
  matches <- quanteda::kwic(tokens, pattern, window = window, ...)
}


#' Search for multiple patterns
#'
#' @param tokens - quanteda token object - created with convert_to_quanteda
#' @param patterns - list of regexes to search for
#' @param window - amount of surrounding tokens to get as context
#' @param ... additional parameters passed to quanteda::kwic
#'
#' @return kwic match dataframe
search_for_kws <- function(tokens, patterns, window = 10, ...) {
  results <- list()

  for (pattern in patterns) {
    result <- search_for_kwic(tokens, pattern, window = window, ...)
    results <- append(results, list(result))
  }

  results <- dplyr::bind_rows(results)
}


#' simplify kwic dataframe for the end user
#'
#' @param kwic - kwic dataframe created by search_for_kws
#'
#' @return a simplified data frame with a docname-page, the matched sentence and the search pattern!
#'
#' @importFrom rlang .data
simplify_kwic <- function(kwic) {
  kwic <- dplyr::as_tibble(kwic)

  kwic <- dplyr::mutate(kwic,
                        sentence = paste(kwic$pre, kwic$keyword, kwic$post),
                        docname = stringr::str_remove(kwic$docname, "\\.\\w?\\w?\\d?\\d?$"),
                        # docname = stringr::str_extract(kwic$docname, "(=?\\.\\w?\\w?\\d\\d?$)"),
                        page = stringr::str_extract(kwic$docname, "\\w?\\w?\\d\\d?$"))

  kwic <- dplyr::select(kwic,
                        "docname",
                        "page",
                        "sentence",
                        "pattern"
                        )
}
