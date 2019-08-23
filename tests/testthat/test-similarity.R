test_that("multiplication works", {
  lsi <- model_lsi(common_corpus(), id2word = common_dictionary())

  mm <- read_serialized_mmcorpus(corpus_mm)

  new_document <- "A human and computer interaction"
  preprocessed_new_document <- preprocess(new_document, min_freq = 0)
  vec_bow <- doc2bow(dictionary, preprocessed_new_document)
  vec_lsi <- wrap(lsi, vec_bow)

  wrapped_lsi <- wrap(lsi, mm)
  index <- similarity_matrix(wrapped_lsi)
  expect_type(index, "environment")

  sims <- wrap(index, vec_lsi)

  similarity <- get_similarity(sims)
  expect_length(similarity, 2)

  index2 <- similarity(corpus_mm, num_features = reticulate::py_len(dictionary))

  # query all similarities
  sims <- wrap(index2, corpus_bow, to_r = TRUE)

  sims_long <- reshape2::melt(sims)
  expect_length(similarity, 2)
})
