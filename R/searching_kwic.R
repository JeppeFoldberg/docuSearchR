#' Using quantedas kwic to search for the keyword in context!
#'
#' @param tokens tokens object created with convert_to_corpus
#' @param pattern regex to search for
#' @param ... additional parameters passed to kwic!
#' @param window amount of surrounding tokens to get as context
#'
#' @return
#' @export
#'
#' @examples
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
#' @export
#'
#' @examples
search_for_kws <- function(tokens, patterns, window = 10, ...) {
  results <- list()

  for (pattern in patterns) {
    result <- search_for_kwic(tokens, pattern, window = window, ...)
    results <- append(results, list(result))
  }

  results <- dplyr::bind_rows(results)
}
