test_that("loading loads a readtext object of correct length", {
  docs <- load_documents("../data")

  # test that it loads a readtext object
  expect_s3_class(docs, "readtext")

  # For now it should read in three files!
  expect_equal(nrow(docs), 3)
})
