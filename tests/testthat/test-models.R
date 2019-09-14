test_that("models works", {

  # set seed
  seed <- 42L
  set.seed(seed)
  reticulate::py_set_seed(seed)

  data(corpus, package = "gensimr")
  docs <- prepare_documents(corpus)
  dictionary <- corpora_dictionary(docs)
  corpus_bow <- doc2bow(dictionary, docs)
  corpus_mm <- serialize_mmcorpus(corpus_bow, auto_delete = FALSE)
  mm <- read_serialized_mmcorpus(corpus_mm)
  expect_type(mm, "environment")

  # tfidf
  tfidf_mm <- model_tfidf(mm)
  tfidf_file <- model_tfidf(corpus_mm)
  tfidf_object <- model_tfidf(corpus_mm)
  expect_equal(tfidf_file, tfidf_object)

  # apply
  corpus_transformed_file <- wrap(tfidf_file, corpus_bow)
  corpus_transformed_object <- wrap(tfidf_object, corpus_bow)
  expect_equal(corpus_transformed_object, corpus_transformed_file)

  # lsi
  k <- 2L
  lsi <- model_lsi(corpus_transformed_file, id2word = dictionary, num_topics = k)
  expect_type(lsi, "environment")
  wrapped_corpus <- wrap(lsi, corpus_transformed_file)
  wrapped_corpus_docs <- get_docs_topics(wrapped_corpus)
  expect_length(wrapped_corpus_docs, k * 2)

  # fasttext
  ft <- model_fasttext(size = 4L, window = 3L, min_count = 1L)
  expect_type(ft, "environment")
  ft$build_vocab(sentences = docs)
  ft$train(sentences = docs, total_examples = length(docs), epochs = 10L)
  sim <- ft$wv$most_similar(positive = c('computer', 'human'), negative = c('interface')) %>% 
    reticulate::py_to_r()
  expect_equal(sim[[1]][[1]], "system")

  match <- ft$wv$doesnt_match(c("human", "computer", "interface", "tree")) %>% 
    reticulate::py_to_r()
  expect_equal(match, "tree")

  # random projections
  rp <- model_rp(corpus_transformed_file, id2word = dictionary, num_topics = k)
  expect_type(rp, "environment")
  wrapped_corpus <- wrap(rp, corpus_transformed_file)
  wrapped_corpus_docs <- get_docs_topics(wrapped_corpus)
  expect_length(wrapped_corpus_docs, k * 2)

  # lda
  lda <- model_lda(corpus_mm, id2word = dictionary, num_topics = k)
  expect_type(lda, "environment")
  lda <- model_lda(corpus_transformed_file, id2word = dictionary, num_topics = k)
  expect_type(lda, "environment")
  wrapped_corpus <- wrap(lda, corpus_transformed_file)
  wrapped_corpus_docs <- get_docs_topics(wrapped_corpus)
  expect_length(wrapped_corpus_docs, k * 2)

  # ldamc
  ldamc <- model_ldamc(mm, id2word = dictionary, num_topics = k)
  expect_type(ldamc, "environment")
  ldamc <- model_ldamc(corpus_mm, id2word = dictionary, num_topics = k)
  expect_type(ldamc, "environment")
  wrapped_corpus <- wrap(ldamc, corpus_transformed_file)
  wrapped_corpus_docs <- get_docs_topics(wrapped_corpus)
  expect_length(wrapped_corpus_docs, k * 2)

  # log entropy
  log_entropy <- model_logentropy(corpus_bow)
  expect_type(log_entropy, "environment")
  wrapped_corpus <- wrap(log_entropy, corpus_bow)

  # hdp
  hdp <- model_hdp(corpus_mm, id2word = dictionary)
  expect_type(hdp, "environment")
  hdp <- model_hdp(mm, id2word = dictionary)
  expect_type(hdp, "environment")
  topics <- reticulate::py_to_r(hdp$show_topic(topic_id = 1L, topn = 5L))
  expect_length(topics, 5L)

  # word2vec
  word2vec <- model_word2vec(size = 100L, window = 5L, min_count = 1L)
  expect_type(word2vec, "environment")
  word2vec$build_vocab(docs) 
  word2vec$train(docs, total_examples = word2vec$corpus_count, epochs = 20L)
  word2vec$init_sims(replace = TRUE)
  sim <- word2vec$wv$most_similar(positive = c("interface")) %>% 
    reticulate::py_to_r()

  match <- word2vec$wv$doesnt_match(c("human", "interface", "trees")) %>% 
    reticulate::py_to_r()

  sim1 <- word2vec$wv$similarity("eps", "survey") %>% 
    reticulate::py_to_r()
  sim2 <- word2vec$wv$similarity("human", "trees") %>% 
    reticulate::py_to_r()

  # author topic
  data("authors")

  auth2doc <- auth2doc(authors, name, document)
  expect_type(auth2doc, "environment")

  # create temp to hold serialized data
  temp <- tempfile("serialized")

  # build model
  atmodel <- model_at(
    corpus_mm, 
    id2word = dictionary, 
    author2doc = auth2doc, 
    num_topics = 2L, 
    serialized = TRUE,
    serialization_path = temp
  )
  expect_type(atmodel, "environment")

  # delete temp
  unlink(temp, recursive = TRUE)

  atopics <- get_author_topics(atmodel)
  expect_length(atopics, 5)

  # map models
  models <- map_model(
    corpus = corpus_transformed_file, 
    id2word = dictionary,
    num_topics = c(2, 4, 6)
  ) 

  # compute topic coherence of all models
  models <- map_coherence(
    models, corpus = corpus_transformed_file, 
    dictionary = dictionary, 
    coherence = 'u_mass'
  )

  plot(models)
  print(models)

  # coherence
  coh <- get_coherence_data(models)
  expect_length(coh, 3)

  # perplexity
  p <- get_perplexity_data(models)
  expect_length(p, 3)

  collect <- as_model_collection(list(lda, ldamc))
  expect_length(collect, 2)

  # cleanup
  delete_mmcorpus(corpus_mm)
})
