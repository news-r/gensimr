#' Tf-idf Model
#' 
#' Initialise a model based on the document frequencies of all its features.
#' 
#' @param mm A matrix market as returned by \code{\link{mmcorpus_serialize}}.
#' @param normalize Whether to normalize the resulting vectors to (Euclidean) unit length.
#' 
#' @name model_tfidf
#' 
#' @export
model_tfidf <- function(mm, normalize = FALSE) UseMethod("model_tfidf")

#' @rdname model_tfidf
#' @method model_tfidf mm
#' @export
model_tfidf.mm <- function(mm, normalize = FALSE){
  assert_that(!missing(mm), msg = "Missing `mm`")
  corpus <- gensim$corpora$MmCorpus(mm$file)
  model <- gensim$models$TfidfModel(corpus, normalize = normalize)

  # unlink temp
  if(mm$temp) unlink(mm$file)

  invisible(model)
}

#' @rdname model_tfidf
#' @method model_tfidf gensim.corpora.mmcorpus.MmCorpus
#' @export
model_tfidf.gensim.corpora.mmcorpus.MmCorpus <- function(mm, normalize = FALSE){
  assert_that(!missing(mm), msg = "Missing `mm`")
  model <- gensim$models$TfidfModel(mm, normalize = normalize)
  invisible(model)
}

#' Latent Semantic Indexing
#' 
#' Transform into a latent n dimensional space via Latent Semantic Indexing.
#' 
#' @param corpus Model as returned by \code{\link{corpora_transform}}. A tf-idf/bag-of-words transformation is recommended for LSI.
#' @param dictionary Dictionary as returned by \code{\link{corpora_dictionary}}.
#' @param num_topics Number of topics (dimensions).
#' 
#' @details Target dimensionality (\code{num_topics}) of 200–500 is recommended as a “golden standard” \url{https://dl.acm.org/citation.cfm?id=1458105}.
#' 
#' @name model_lsi
#' 
#' @export
model_lsi <- function(corpus, dictionary, num_topics = 2) UseMethod("model_lsi")

#' @rdname model_lsi
#' @method model_lsi gensim.interfaces.TransformedCorpus
model_lsi.gensim.interfaces.TransformedCorpus <- function(corpus, dictionary, num_topics = 2){
  assert_that(!missing(corpus), msg = "Missing `corpus`")
  assert_that(!missing(dictionary), msg = "Missing `dictionary`")
  gensim$models$LsiModel(corpus, id2word = dictionary, num_topics = num_topics)
}

#' @rdname model_lsi
#' @method model_lsi mm
#' @export
model_lsi.mm <- function(corpus, dictionary, num_topics = 2){
  assert_that(!missing(corpus), msg = "Missing `corpus`")
  assert_that(!missing(dictionary), msg = "Missing `dictionary`")
  corpus_tfidf <- model_tfidf(corpus)
  gensim$models$LsiModel(corpus_tfidf, id2word = dictionary, num_topics = num_topics)
}

#' Transform Model
#'
#' Trasnform a corpus based on a model.
#' 
#' @param model A model as returned, for instance by \code{\link{model_lsi}}.
#' @param corpus A corpus as returned by \code{\link{corpora_transform}}.
#' @param wrapped_corpus Output of \code{\link{wrap_corpus}}.
#' 
#' @name wrap_corpus
#' @export
wrap_corpus <- function(model, corpus){
  model[corpus]
}

#' @rdname wrap_corpus
#' @export
wrap_corpus_docs <- function(wrapped_corpus){
  l <- reticulate::py_len(wrapped_corpus)

  docs <- list()
  for(i in 1:l){
    doc <- reticulate::py_to_r(wrapped_corpus[i - 1])
    docs <- append(docs, list(doc))
  }

  dimensions <- length(docs[[1]])
  vars <- c("x", "y")

  col_names <- paste0("dimension_", 1:dimensions) %>% 
    tidyr::crossing(vars) %>% 
    dplyr::mutate(name = paste0(., "_", vars)) %>% 
    dplyr::pull(name)

  docs %>% 
    purrr::map(unlist) %>% 
    purrr::map(t) %>% 
    purrr::map_dfr(tibble::as_tibble) %>% 
    purrr::set_names(col_names)
}
