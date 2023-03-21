test_that("loading loads a readtext object", {
  expect_s3_class(load_word("../data/test_kodeord.docx"), "readtext")
})
