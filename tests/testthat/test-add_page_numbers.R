test_that("Adding filetype and exploding pages works!", {
  docs <- load_documents("../data")
  docs <- add_filetype(docs)
  # col_names <- names(docs)

  expect_true("filetype" %in% names(docs))

  docs_after <- pivot_pages_longer(docs)

  # now we get each word page as a document! (we add two because its 3 pages long)
  expect_equal(nrow(docs_after), nrow(docs) + 2)

  # test that it is still a readtext object
  expect_s3_class(docs_after, "readtext")
})
