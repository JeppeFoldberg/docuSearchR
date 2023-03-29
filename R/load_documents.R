#' function to read in a single excel sheet
#'
#' @param path path to the excel file!
#'
#' @return readtextobject with the charactercells from the sheet!
#'
#' @importFrom rlang .data
read_excel <- function(path) {
  cells <- tidyxl::xlsx_cells(path,
                              sheets = c(1L)) # change this to na later and implement combining sheets and adress into one page varible

  character_cells <- dplyr::filter(cells, !is.na(character))


  character_cells <- dplyr::mutate(character_cells,
                                   doc_id = path)


  character_cells <- dplyr::rename(character_cells,
    page = "address",
    text = "character"
  )

  character_cells <- dplyr::select(character_cells,
                                   "doc_id",
                                   "text",
                                   "page")

  class(character_cells) <- c("readtext", "data.frame")

  return(character_cells)
}

try_catch_missing_files <- function(call) {
  # function to call a file reader and return NULL when no file is present!
}


#' load_documents - loading word and pdf files for easy reading!
#'
#' @param path character vector with the path for the location of documents
#'
#' @return readtext objects of the loaded files
#'
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

  excel_file_paths <- list.files(path)[stringr::str_detect(list.files(path), ".*\\.xlsx")]

  excel_files <- list()
  for (excel_file_path in excel_file_paths) {
    excel_file_path <- paste0(path, "/", excel_file_path)
    excel_file <- read_excel(excel_file_path)
    excel_files <- append(excel_files, list(excel_file))
  }

  excel_files <- dplyr::bind_rows(excel_files)

  docs <- dplyr::bind_rows(word_files, pdf_files, excel_files)

  return(docs)
}

#' Loading all supported documents in folders and subfolders
#'
#' @param path - path to the folder that contains all documents
#'
#' @return the loaded readtext object already with page added
#' @export
#'
#' @examples
#' # not run
#' # readtext_object <- load_documents_recursive()
load_documents_recursive <- function(path) {
  dirs <- list.dirs(path, recursive = T)

  results <- list()
  for (dir in dirs) {
    result <- load_documents(dir)
    results <- append(results, list(result))
  }

  results <- dplyr::bind_rows(results)

  docs <- add_filetype(results)
  docs <- pivot_pages_longer(docs)

  # collapsing page_number into the doc_id - later we will extract it to its own column
  docs <- dplyr::mutate(docs,
                        "doc_id" =  paste(docs$doc_id, docs$page, sep = "."))
}

#' Convert to tokens for easy searching
#'
#' @param readtext_object a readtext s3 class created by load_documents
#'
#' @return quanteda tokenized corpus
convert_to_tokens <- function(readtext_object) {
  # # simply having a column named doc_id is not enough for quanteda...
  # rownames(readtext_object) <- readtext_object$doc_id
  corpus <- quanteda::corpus(readtext_object)
  corpus <- quanteda::tokens(corpus)
}

#' main function to interface with docuSearchR
#'
#' @param path path to the folder that contains documents - also reads files in nested folders
#' @param list_of_regexes list of searchterms - regexcompatible
#' @param window amount of context-words to get around the matches!
#'
#' @return the simplified search results!
#' @export
#'
#' @examples
#' # not run
#' # main("path to folder", c("regex"), window = 5)
main <- function(path, list_of_regexes, window = 10) {
  docs <- load_documents_recursive(path)

  docs_tokens <- convert_to_tokens(docs)

  kwic <- search_for_kws(docs_tokens,
                         list_of_regexes,
                         window = window)

  simplified_kwic <- simplify_kwic(kwic)
}
