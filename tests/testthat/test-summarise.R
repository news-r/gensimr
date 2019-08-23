test_that("summariser works", {
  # set seed
  seed <- 42L
  set.seed(seed)
  reticulate::py_set_seed(seed)

  # preprocess
  data(corpus, package = "gensimr")
  docs <- prepare_documents(corpus)
  dictionary <- corpora_dictionary(docs)
  corpus_bow <- doc2bow(dictionary, docs)
  corpus_mm <- serialize_mmcorpus(corpus_bow, auto_delete = FALSE)
  mm <- read_serialized_mmcorpus(corpus_mm)

  # weights
  weights <- get_bm25_weights(corpus_mm)
  expect_length(weights, 3)
  weights <- get_bm25_weights(mm)
  expect_length(weights, 9)

  # keywords
  kw <- keywords(corpus[[1]]) 
  expect_equal(kw[[1]], "machine")

  crps <- paste0(corpus, collapse = ". ")
  sums <- summarize(crps)
  expect_length(sums, 1)

  # cleanup
  delete_mmcorpus(corpus_mm)
})
