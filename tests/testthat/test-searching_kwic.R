test_that("Searching works", {
  docs <- load_documents_recursive("../data")

  docs <- convert_to_corpus(docs)

  result <- search_for_kws(docs, c("kodeord*"))

  expect_s3_class(result, "kwic")
  expect_equal(NROW(result), 3)
})
