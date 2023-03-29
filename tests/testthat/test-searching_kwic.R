test_that("Searching works", {
  docs <- load_documents_recursive("../data")

  docs <- convert_to_tokens(docs)

  result <- search_for_kws(docs, c("kodeord*"))

  expect_s3_class(result, "kwic")
  # expects five matches because thats how many times i wrote kodeord in the test data
  expect_equal(NROW(result), 5)
})

test_that("Simplifying kwic dataframe for easy reading", {
  kwic <- dplyr::tibble(
    docname = c("ja.NA", "nej.A2"),
    from = c(1L, 2L),
    to = c(1L, 2L),
    pre = c("sentence before", "still the sentence before"),
    keyword = c("keyword", "keyword"),
    post = c("sentence after", "still the sentence after"),
    pattern = c("regex1", "regex2")
  )

  goal <- dplyr::tibble(
    docname = c("ja.NA", "nej.A2"),
    page = c("NA", "A2"),
    sentence = c("sentence before keyword sentence after",
                 "still the sentence before keyword still the sentence after"),
    pattern = c("regex1", "regex2")
  )

  simplified_kwic <- simplify_kwic(kwic)

  expect_equal(simplified_kwic, goal)
})
