test_that("api works", {
  library(gensimr)

  dataset <- "glove-twitter-25"

  # model description
  recs <- api_info(dataset)$num_records
  expect_length(api_info(), 2)
  expect_equal(recs, 1193514L)
  
  model <- api_load(dataset)

  # find words most similar to "cat"
  sim <- model$most_similar("cat") %>% 
    reticulate::py_to_r()

  expect_equal(sim[[1]][[1]], "dog")
})
