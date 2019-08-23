test_that("similarity works", {
  data(corpus, package = "gensimr")
  docs <- prepare_documents(corpus)
  dictionary <- corpora_dictionary(docs)
  corpus_bow <- doc2bow(dictionary, docs)
  corpus_mm <- serialize_mmcorpus(corpus_bow, auto_delete = FALSE)
  mm <- read_serialized_mmcorpus(corpus_mm)

  lsi <- model_lsi(corpus_bow, id2word = dictionary)

  new_document <- "A human and computer interaction"
  preprocessed_new_document <- preprocess(new_document, min_freq = 0)
  vec_bow <- doc2bow(common_dictionary(), preprocessed_new_document)
  vec_lsi <- wrap(lsi, vec_bow)

  mm <- read_serialized_mmcorpus(corpus_mm)

  wrapped_lsi <- wrap(lsi, mm)
  index <- similarity_matrix(wrapped_lsi)
  expect_type(index, "environment")

  sims <- wrap(index, vec_lsi)

  similarity <- get_similarity(sims)
  expect_length(similarity, 2)

  index2 <- similarity(corpus_mm, num_features = reticulate::py_len(dictionary))

  # query all similarities
  sims <- wrap(index2, corpus_bow, to_r = TRUE)

  expect_length(similarity, 2)
})
