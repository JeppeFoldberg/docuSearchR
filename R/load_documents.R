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
  if (any(stringr::str_detect(list.files(path), ".pptx"))) {
    # convert_ppts() # does not do anything yet - docxtractr might be the best package
    # however, it relies on libre-office!
  }

  word_files <- readtext::readtext(paste0(path, "/*.docx"))
  pdf_files <- readtext::readtext(paste0(path, "/*.pdf"))

  docs <- rbind(word_files, pdf_files)

  return(docs)
}

#' Convert to corpus for easy searching
#'
#' @param readtext_object a readtext s3 class created by load_documents
#'
#' @return quanteda corpus
#' @export
#'
#' @examples
convert_to_corpus <- function(readtext_object) {
  corpus <- quanteda::corpus(readtext_object, unique_docnames = F)
}
