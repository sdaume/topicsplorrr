# simple test to check that unigrams and bigrams are parsed

test_that("unigrams parsed from text source", {
  # use quanteda's inaugural presidential speeches as sampel data
  sample_docs <- quanteda::convert(quanteda::data_corpus_inaugural,
                                   to = "data.frame")

  unigrams_df <- expect_silent(unigrams_by_date(textData = sample_docs,
                                                textColumn = "text",
                                                dateColumn = "Year"))
  expect_gt(nrow(unigrams_df), 0)
})


test_that("unigrams parsed with correct format", {
  # use quanteda's inaugural presidential speeches as sampel data
  sample_docs <- quanteda::convert(quanteda::data_corpus_inaugural,
                                   to = "data.frame")

  unigrams_df <- unigrams_by_date(textData = sample_docs,
                                  textColumn = "text",
                                  dateColumn = "Year")

  expect_s3_class(unigrams_df, "data.frame")
  expect_equal(ncol(unigrams_df), 3, label = "number of columns")
  expect_gt(nrow(unigrams_df), 0, label = "number of rows")
})



test_that("bigrams parsed from text source", {
  # use quanteda's inaugural presidential speeches as sampel data
  sample_docs <- quanteda::convert(quanteda::data_corpus_inaugural,
                                   to = "data.frame")

  bigrams_df <- expect_silent(bigrams_by_date(textData = sample_docs,
                                              textColumn = "text",
                                              dateColumn = "Year"))
  expect_gt(nrow(bigrams_df), 0)
})


test_that("bigrams parsed with correct format", {
  # use quanteda's inaugural presidential speeches as sampel data
  sample_docs <- quanteda::convert(quanteda::data_corpus_inaugural,
                                   to = "data.frame")

  bigrams_df <- bigrams_by_date(textData = sample_docs,
                                textColumn = "text",
                                dateColumn = "Year")

  expect_s3_class(bigrams_df, "data.frame")
  expect_equal(ncol(bigrams_df), 3, label = "number of columns")
  expect_gt(nrow(bigrams_df), 0, label = "number of rows")
})

