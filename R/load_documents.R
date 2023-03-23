#' load_documents - loading word and pdf files for easy reading!
#'
#' @param path character vector with the path for the location of documents
#'
#' @return readtext objects of the loaded files
#' @export
#'
#' @examples
#' load_documents
load_documents <- function(path) {
  word_files <- readtext::readtext(paste0(path, ".docx"))
  pdf_files <- readtext::readtext(paste0(path, ".pdf"))

  docs <- rbind(word_files, pdf_files)
}
