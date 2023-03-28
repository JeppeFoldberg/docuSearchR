test_that("Adding filetype and exploding pages works!", {
  docs <- load_documents_recursive("../data")
  docs <- add_filetype(docs)
  col_names <- names(docs)

  expect_true("filetype" %in% col_names)

  expect_equal(docs$filetype, c(".docx", ".pdf", ".docx"))

  docs_after <- pivot_pages_longer(docs)

  # now we get each word page as a document!
  expect_equal(nrow(docs_after), nrow(docs) + 5)

  # test that it is still a readtext object
  expect_s3_class(docs_after, "readtext")
})
