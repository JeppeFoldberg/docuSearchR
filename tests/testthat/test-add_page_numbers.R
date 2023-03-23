test_that("Adding filetype and exploding pages works!", {
  docs <- load_documents("../data")
  docs <- add_filetype(docs)
  col_names <- names(docs)

  expect_true("filetype" %in% col_names)

  expect_equal(docs$filetype, c(".docx", ".pdf"))

  docs_after <- pivot_pages_longer(docs)

  expect_equal(nrow(docs_after), nrow(docs) + 2)
})
