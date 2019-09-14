test_that("lda works", {
  docs <- prepare_documents(corpus)
  dict <- corpora_dictionary(docs)
  corpora <- doc2bow(dict, docs)
  
  # lda model
  model <- model_lda(
    corpus = corpora, 
    id2word = dict, 
    iterations = 50L, 
    num_topics = 2L
  )
  
  # visualise
  vis <- prepare_ldavis(model, corpora, dict)
  expect_type(vis, "environment")
  # plot(vis)

  html <- ldavis_as_html(vis)
  expect_type(html, "environment")

  # plot_ldavis(model, corpora, dict)
})
