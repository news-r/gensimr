test_that("test and data works", {
  data("corpus")
  expect_length(corpus, 9)
  text <- common_texts(TRUE)
  expect_length(text, length(corpus))
  
  docs <- prepare_documents(corpus) %>% 
    purrr::map(unlist)
  expect_equal(docs, text)

  crps <- common_corpus(TRUE)
  expect_length(crps, 9)

  dict <- common_dictionary(TRUE)
  expect_length(dict, 2)
})
