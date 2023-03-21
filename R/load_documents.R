#' A test function
#'
#' @param path character vector with the path for the location of word document(s)
#'
#' @return readtext object(s) of the loaded files
#' @export
#'
#' @examples
#' load_word()
load_word <- function(path) {
  file <- readtext::readtext(path)
}
