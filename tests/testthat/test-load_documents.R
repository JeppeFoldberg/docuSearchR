test_that("loading loads a readtext object of correct length", {
  docs <- load_documents("../data")

  # test that it loads a readtext object
  expect_s3_class(docs, "readtext")

  # read in the word doc, the pdf and 6 cells from text-excel!
  expect_equal(nrow(docs), 8)
})

test_that("converting to tokens works", {
  docs <- load_documents_recursive("../data")

  # test that it loads a readtext object
  expect_s3_class(docs, "readtext")

  docs <- convert_to_tokens(docs)

  expect_s3_class(docs, "tokens")

})

test_that("Reading excel works", {
  excel <- read_excel("../data/test_excel.xlsx")

  # test that it loads a readtext object
  expect_s3_class(excel, "readtext")

  expect_equal(nrow(excel), 6)
})

test_that("Recursive loading works!", {
  docs <- load_documents_recursive("../data")

  # test that it loads a readtext object
  expect_s3_class(docs, "readtext")

  # For now it should read in three files with 14 pages...
  expect_equal(nrow(docs), 14)
})
