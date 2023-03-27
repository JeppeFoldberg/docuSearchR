#' Adds a filetype column to the readtext object!
#'
#' @param readtext_object a readtext s3 class created by load_documents
#'
#' @return readtext object with new filetype column
#' @export
#'
#' @examples
add_filetype <- function(readtext_object) {
  readtext_object$filetype <- stringr::str_extract(readtext_object$doc_id, "\\..{3,4}")
  return(readtext_object)
}

#' Adds word pages as their own document
#'
#' @description takes a readtext object and explodes each word document to be their
#' own document.
#' @param readtext_object a readtext s3 class created by load_documents
#'
#' @return the readtext object
#' @export
#'
#' @examples
#'
#' @importFrom rlang .data
pivot_pages_longer <- function(readtext_object) {
  doc_docs <- dplyr::filter(readtext_object, .data$filetype == ".docx")
  other_docs <- dplyr::filter(readtext_object, .data$filetype != ".docx")

  # extracting each page to new column!
  doc_docs <- tidyr::separate_longer_delim(doc_docs,
                                           .data$text,
                                           delim = "\n")
  # adding id to each page!
  doc_docs$page <- with(doc_docs, ave(rep(1, nrow(doc_docs)), doc_id, FUN = seq_along))

  # coercing back to readtext object!
  class(doc_docs) <- c("readtext", "data.frame")

  # adding NA to page for other types of objects for now - later we will probably
  # want a way to estimate their page location!
  other_docs$page <- NA
  all_docs <- rbind(doc_docs, other_docs)

  return(all_docs)
}

