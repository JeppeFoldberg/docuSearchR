

try_catch_missing_files <- function(call) {
  # function to call a file reader and return NULL when no file is present!
}


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

  # replace these long wrappings of trycatch with one unified wrapper above!
  word_files <- tryCatch(
    readtext::readtext(paste0(path, "/*.docx")),
    error=function(cond) {
      # message(paste("No doc file in: ", path))
      # message("Here's the original error message:")
      # message(cond)
      return(NULL)
    })

  pdf_files <- tryCatch(
    readtext::readtext(paste0(path, "/*.pdf")),
    error=function(cond) {
      # message(paste("No doc file in: ", path))
      # message("Here's the original error message:")
      # message(cond)
      return(NULL)
    })

  docs <- dplyr::bind_rows(word_files, pdf_files)

  return(docs)
}

load_documents_recursive <- function(path) {
  dirs <- list.dirs(path, recursive = T)

  results <- list()
  for (dir in dirs) {
    result <- load_documents(dir)
    results <- append(results, list(result))
  }
  results <- dplyr::bind_rows(results)
}

#' Convert to corpus for easy searching
#'
#' @param readtext_object a readtext s3 class created by load_documents
#'
#' @return quanteda tokenized corpus
#' @export
#'
#' @examples
convert_to_corpus <- function(readtext_object) {
  corpus <- quanteda::corpus(readtext_object, unique_docnames = F)
  corpus <- quanteda::tokens(corpus)
}
