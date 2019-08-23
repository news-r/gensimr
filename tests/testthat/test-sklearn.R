test_that("sklearn works", {
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
  tfidf <- model_tfidf(corpus_mm)
  corpus_transformed <- wrap(tfidf, corpus_bow)

  data("authors", package = "gensimr")

  #  author topic
  auth2doc <- auth2doc(authors, name, document)
  expect_type(auth2doc, "environment")

  temp <- tempfile("serialized")
  atmodel <- sklearn_at(
    id2word = dictionary, 
    author2doc = auth2doc, 
    num_topics = 2L, 
    passes = 100L,
    serialized = TRUE,
    serialization_path = temp
  )
  unlink(temp, recursive = TRUE)
  expect_type(atmodel, "environment")

  fit <- atmodel$fit(corpus_bow)$transform("jack") %>% 
    reticulate::py_to_r()

  expect_length(fit, 2)

  # doc2vec
  d2v <- sklearn_doc2vec(min_count = 1, size = 5)
  expect_type(d2v, "environment")
  vectors <- d2v$fit_transform(docs) %>% 
    reticulate::py_to_r()
  expect_length(vectors, 45) # size = 5 * 9 docs

  # hdp
  hdp <- sklearn_hdp(id2word = dictionary)
  expect_type(d2v, "environment")
  vectors <- hdp$fit_transform(corpus_bow) %>% 
    reticulate::py_to_r()

  expect_length(vectors, 81) # 9 docs

  # lda
  lda <- sklearn_lda(
    num_topics = 2L, 
    id2word = dictionary, 
    iterations = 20L, 
    random_state = 1L
  )
  expect_type(lda, "environment")
  trans <- lda$fit_transform(corpus_bow) %>% 
    reticulate::py_to_r()
  expect_length(trans, 18) # 9 docs * 2 topics

  # lsi
  lsi <- sklearn_lsi(id2word = dictionary, num_topics = 15L)
  expect_type(lsi, "environment")

  # L2 reg classifier
  clf <- sklearn_logistic(penalty = "l2", C = 0.1, solver = "lbfgs")

  # sklearn pipepline
  pipe <- sklearn_pipeline(lsi, clf)

  # Create some random binary labels for our documents.
  labels <- sample(c(0L, 1L), 9, replace = TRUE)

  # How well does our pipeline perform on the training set?
  fit <- pipe$fit(corpus_bow, labels)$score(corpus_bow, labels) %>% 
    reticulate::py_to_r()
  expect_gt(fit, .7)

  # random projections
  rp_model <- sklearn_rp(id2word = dictionary)
  expect_type(rp_model, "environment")
  rp_fit <- rp_model$fit(corpus_bow)

  # Use the trained model to transform a document.
  result <- rp_fit$transform(corpus_bow) %>% 
    reticulate::py_to_r()

  expect_length(result, 2700)

  # phrase detection
  corpus_split <- corpus %>% 
    purrr::map(strsplit, " ") %>% 
    purrr::map(function(x){
      sentence <- x[[1]]
      tolower(sentence)
    })

  # Create the model. Make sure no term is ignored and combinations seen 2+ times are captured.
  pt_model <- sklearn_pt(min_count = 1, threshold = 2)

  # Use sklearn fit_transform to see the transformation.
  pt_trans <- pt_model$fit_transform(corpus_split)

  # Since graph and minors were seen together 2+ times they are considered a phrase.
  expect_true(c("graph_minors") %in% reticulate::py_to_r(pt_trans)[[9]])

  # word id mapping
  skbow_model <- sklearn_doc2bow()

  # fit
  corpus_skbow <- skbow_model$fit_transform(corpus) %>% 
    reticulate::py_to_r()

  expect_length(corpus_skbow, 9)

  # tfidf
  tfidf_model <- sklearn_tfidf(dictionary = dictionary)
  tfidf_w_sklearn <- tfidf_model$fit_transform(corpus_bow)

  # same as with gensim
  expect_true(corpus_transformed[[1]] == tfidf_w_sklearn[[1]])

  # cleanup
  delete_mmcorpus(corpus_mm)
})
