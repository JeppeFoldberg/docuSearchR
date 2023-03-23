#' Adds a filetype column to the readtext object!
#'
#' @param readtext_object
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
#' @param readtext_object
#'
#' @return the readtext object
#' @export
#'
#' @examples
pivot_pages_longer <- function(readtext_object) {
  doc_docs <- dplyr::filter(readtext_object, filetype == ".docx")
  other_docs <- dplyr::filter(readtext_object, filetype != ".docx")

  # extracting each page to new column!
  doc_docs <- tidyr::separate_wider_delim(doc_docs, text, names_sep = "_p_", delim = "\n")
  # pivot longer so each text page gets a variable that is the page number!
  doc_docs <- tidyr::pivot_longer(doc_docs, !c(doc_id, filetype), names_to = "page", values_to = "text")
  # cleaning up the page variable
  doc_docs <- dplyr::mutate(doc_docs,
                            page = as.integer(stringr::str_extract(page, "\\d+")))

  # coercing back to readtext object!
  class(doc_docs) <- c("readtext", "data.frame")

  other_docs$page <- NA
  all_docs <- rbind(doc_docs, other_docs)
  return(all_docs)
}

