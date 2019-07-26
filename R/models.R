#' Tf-idf Model
#' 
#' Initialise a model based on the document frequencies of all its features.
#' 
#' @param mm A matrix market as returned by \code{\link{mmcorpus_serialize}}.
#' @param normalize ormalize document vectors to unit euclidean length? You can also inject your own function into normalize.
#' @param smart SMART (System for the Mechanical Analysis and Retrieval of Text) 
#' Information Retrieval System, a mnemonic scheme for denoting tf-idf weighting variants in 
#' the vector space model. The mnemonic for representing a combination of weights takes the form XYZ, 
#' for example \code{nfc}, \code{bpn} and so on, where the letters represents the term weighting of the document vector.
#' See SMART section.
#' @param pivot You can either set the pivot by hand, or you can let Gensim figure it out automatically with the following two steps: 
#' 1) Set either the u or b document normalization in the smartirs parameter.
#' 2) Set either the corpus or dictionary parameter. The pivot will be automatically determined from the properties of the corpus or dictionary.
#' If pivot is \code{NULL} and you don’t follow steps 1 and 2, then pivoted document length normalization will be disabled.
#' @param slope Setting the slope to 0.0 uses only the pivot as the norm, and setting the slope to 1.0 effectively disables pivoted 
#' document length normalization. Singhal [2] suggests setting the slope between 0.2 and 0.3 for best results.
#' @param ... Any other options, from the \href{https://radimrehurek.com/gensim/models/tfidfmodel.html}{official documentation}.
#' 
#' @section SMART:
#' Term frequency weighing:
#' \itemize{
#'   \item{\code{b} - binary}  
#'   \item{\code{t} or \code{n} - raw}  
#'   \item{\code{a} - augmented}  
#'   \item{\code{l} - logarithm}  
#'   \item{\code{d} - double logarithm}  
#'   \item{\code{L} - Log average}  
#' }
#' 
#' Document frequency weighting:
#' \itemize{
#'   \item{\code{x} or \code{n} - none}  
#'   \item{\code{f} - idf}  
#'   \item{\code{t} - zero-corrected idf}  
#'   \item{\code{p} - probabilistic idf}  
#' }
#' 
#' Document normalization:
#' \itemize{
#'   \item{\code{x} or \code{n} - none}  
#'   \item{\code{c} - cosine}  
#'   \item{\code{u} - pivoted unique}  
#'   \item{\code{b} - pivoted character length}  
#' }
#' 
#' @name model_tfidf
#' 
#' @export
model_tfidf <- function(mm, normalize = FALSE, smart = "nfc", pivot = NULL, slope = .25, ...) UseMethod("model_tfidf")

#' @rdname model_tfidf
#' @method model_tfidf mm_file
#' @export
model_tfidf.mm_file <- function(mm, normalize = FALSE, smart = "nfc", pivot = NULL, slope = .25, ...){
  assert_that(!missing(mm), msg = "Missing `mm_file`")
  corpus <- gensim$corpora$MmCorpus(mm$file)
  model <- gensim$models$TfidfModel(
    corpus, 
    pivot = pivot,
    slope = slope,
    smartirs = smart,
    normalize = normalize,
    ...
  )

  # unlink temp
  if(mm$temp && mm$delete) unlink(mm$file)

  invisible(model)
}

#' @rdname model_tfidf
#' @method model_tfidf mm
#' @export
model_tfidf.mm <- function(mm, normalize = FALSE, smart = "nfc", pivot = NULL, slope = .25, ...){
  assert_that(!missing(mm), msg = "Missing `mm`")
  model <- gensim$models$TfidfModel(
    mm, 
    pivot = pivot,
    slope = slope,
    smartirs = smart,
    normalize = normalize,
    ...
  )
  invisible(model)
}

#' Latent Semantic Indexing Model
#' 
#' Transform into a latent n dimensional space via Latent Semantic Indexing.
#' 
#' @param corpus Model as returned by \code{\link{wrap}}. A tf-idf/bag-of-words transformation is recommended for LSI.
#' @param num_topics Number of requested factors (latent dimensions).
#' @param distributed If \code{TRUE} - distributed mode (parallel execution on several machines) will be used.
#' @param ... Any other options, from the \href{https://radimrehurek.com/gensim/models/lsimodel.html}{official documentation}.
#' 
#' @details Target dimensionality (\code{num_topics}) of 200–500 is recommended as a “golden standard” \url{https://dl.acm.org/citation.cfm?id=1458105}.
#' 
#' @name model_lsi
#' 
#' @export
model_lsi <- function(corpus, num_topics = 2L, distributed = FALSE, ...) UseMethod("model_lsi")

#' @rdname model_lsi
#' @method model_lsi wrapped
#' @export
model_lsi.wrapped <- function(corpus, num_topics = 2L, distributed = FALSE, ...){
  assert_that(!missing(corpus), msg = "Missing `corpus`")
  num_topics <- as.integer(num_topics) # enforce integer

  if(num_topics < 200)
    cat(crayon::yellow(cli::symbol$warning), "Low number of topics\n")
  gensim$models$LsiModel(corpus, num_topics = num_topics, distributed = distributed, ...)
}

#' Random Projections Model
#' 
#' Reduce vector space dimensionality. This is a very efficient (both memory- and CPU-friendly) 
#' approach to approximating TfIdf distances between documents, by throwing in a little randomness.
#' 
#' @param corpus Model as returned by \code{\link{wrap}}. A tf-idf/bag-of-words transformation is recommended for LSI.
#' @param num_topics Number of requested factors (latent dimensions).
#' @param ... Any other options, from the \href{https://radimrehurek.com/gensim/models/rpmodel.html}{official documentation}.
#' 
#' @details Target dimensionality (\code{num_topics}) of 200–500 is recommended as a “golden standard” \url{https://dl.acm.org/citation.cfm?id=1458105}.
#' 
#' @name model_rp
#' 
#' @export
model_rp <- function(corpus, num_topics = 2L, ...) UseMethod("model_rp")

#' @rdname model_rp
#' @method model_rp wrapped
#' @export
model_rp.wrapped <- function(corpus, num_topics = 2L, ...){
  assert_that(!missing(corpus), msg = "Missing `corpus`")
  num_topics <- as.integer(num_topics) # enforce integer

  if(num_topics < 200)
    cat(crayon::yellow(cli::symbol$warning), "Low number of topics\n")
  gensim$models$RpModel(corpus, num_topics = num_topics, ...)
}

#' Latent Dirichlet Allocation Model
#' 
#' Transformation from bag-of-words counts into a topic space of lower dimensionality. 
#' LDA is a probabilistic extension of LSA (also called multinomial PCA), so LDA’s topics 
#' can be interpreted as probability distributions over words. These distributions are, 
#' just like with LSA, inferred automatically from a training corpus. Documents are in turn 
#' interpreted as a (soft) mixture of these topics (again, just like with LSA).
#' 
#' @param corpus Model as returned by \code{\link{mmcorpus_serialize}}.
#' @param ... Any other options, from the \href{https://radimrehurek.com/gensim/models/ldamodel.html}{official documentation of \code{model_lda}} or
#' the \href{https://radimrehurek.com/gensim/models/ldamulticore.html}{official documentation of \code{model_ldamc}}.
#' 
#' @section Functions:
#' \itemize{
#'   \item{\code{model_lda} - Single-core implementation.}
#'   \item{\code{model_ldamc} - Multi-core implementation.}  
#' }
#' 
#' @details Target dimensionality (\code{num_topics}) of 200–500 is recommended as a “golden standard” \url{https://dl.acm.org/citation.cfm?id=1458105}.
#' 
#' @name model_lda
#' 
#' @export
model_lda <- function(corpus, ...) UseMethod("model_lda")

#' @rdname model_lda
#' @method model_lda mm_file
#' @export
model_lda.mm_file <- function(corpus, ...){
  assert_that(!missing(corpus), msg = "Missing `corpus`")
  
  corpus_read <- gensim$corpora$MmCorpus(corpus$file)
  model <- gensim$models$LdaModel(corpus_read, ...)

  # unlink temp
  if(corpus$temp && corpus$delete) unlink(corpus$file)
  
  invisible(model)
}

#' @rdname model_lda
#' @method model_lda mm
#' @export
model_lda.mm <- function(corpus, ...){
  assert_that(!missing(corpus), msg = "Missing `corpus`")
  
  model <- gensim$models$LdaModel(corpus, ...)
  
  invisible(model)
}

#' @rdname model_lda
#' @export
model_ldamc <- function(corpus, ...) UseMethod("model_ldamc")

#' @rdname model_lda
#' @method model_ldamc mm_file
#' @export
model_ldamc.mm_file <- function(corpus, ...){
  assert_that(!missing(corpus), msg = "Missing `corpus`")
  
  corpus_read <- gensim$corpora$MmCorpus(corpus$file)
  model <- gensim$models$LdaMulticore(corpus_read, ...)

  # unlink temp
  if(corpus$temp && corpus$delete) unlink(corpus$file)
  
  invisible(model)
}

#' @rdname model_lda
#' @method model_ldamc mm
#' @export
model_ldamc.mm <- function(corpus,...){
  assert_that(!missing(corpus), msg = "Missing `corpus`")
  
  model <- gensim$models$LdaMulticore(corpus, ...)
  
  invisible(model)
}

#' Log Entropy Model
#' 
#' This module allows simple Bag of Words (BoW) represented corpus 
#' to be transformed into log entropy space. It implements Log Entropy 
#' Model that produces entropy-weighted logarithmic term frequency representation.
#' 
#' @param corpus Model as returned by \code{\link{doc2bow}}.
#' @param normalize If \code{TRUE}, the resulted log entropy weighted 
#' vector will be normalized to length of 1, If \code{FALSE} - do nothing.
#' 
#' @name model_logentropy
#' 
#' @export
model_logentropy <- function(corpus, normalize = FALSE) UseMethod("model_logentropy")

#' @rdname model_logentropy
#' @method model_logentropy python.builtin.tuple
#' @export
model_logentropy.python.builtin.tuple <- function(corpus, normalize = FALSE){
  assert_that(!missing(corpus), msg = "Missing `corpus`")
  model <- gensim$models$LogEntropyModel(corpus, normalize = normalize)
  invisible(model)
}

#' Hierarchical Dirichlet Process Model
#' 
#' Hierarchical Dirichlet process (HDP) is a powerful mixed-membership model 
#' for the unsupervised analysis of grouped data. Unlike its finite counterpart, 
#' latent Dirichlet allocation, the HDP topic model infers the number of topics 
#' from the data. Here we have used Online HDP, which provides the speed of online 
#' variational Bayes with the modeling flexibility of the HDP. The idea behind Online 
#' variational Bayes in general is to optimize the variational objective function with 
#' stochastic optimization.The challenge we face is that the existing coordinate ascent
#' variational Bayes algorithms for the HDP require complicated approximation methods
#' or numerical optimization. This model utilises stick breaking construction of Hdp
#' which enables it to allow for coordinate-ascent variational Bayes without numerical
#' approximation.
#' 
#' @param corpus Model as returned by \code{\link{mmcorpus_serialize}}.
#' @param id2word Dictionary for the input corpus, as returned by \code{\link{corpora_dictionary}}.
#' @param ... Any other options, from the \href{https://radimrehurek.com/gensim/models/hdpmodel.html}{official documentation}.
#' 
#' @details This is a non-parametric bayesian method: notice the lack of \code{num_topics} argument.
#' 
#' @name model_hdp
#' 
#' @export
model_hdp <- function(corpus, id2word, ...) UseMethod("model_hdp")

#' @rdname model_hdp
#' @method model_hdp mm_file
#' @export
model_hdp.mm_file <- function(corpus, id2word, ...){
  assert_that(!missing(corpus), msg = "Missing `corpus`")
  assert_that(!missing(id2word), msg = "Missing `id2word`")
  
  corpus_read <- gensim$corpora$MmCorpus(corpus$file)
  model <- gensim$models$HdpModel(corpus_read, id2word, ...)

  # unlink temp
  if(corpus$temp && corpus$delete) unlink(corpus$file)
  
  invisible(model)
}

#' @rdname model_hdp
#' @method model_hdp mm
#' @export
model_hdp.mm <- function(corpus, id2word, ...){
  assert_that(!missing(corpus), msg = "Missing `corpus`")
  assert_that(!missing(id2word), msg = "Missing `id2word`")
  
  model <- gensim$models$HdpModel(corpus, id2word, ...)
  
  invisible(model)
}

#' Word2Vec Model
#' 
#' The word2vec algorithms include skip-gram and CBOW models, using either hierarchical softmax or negative sampling.
#' 
#' @param ... Any other options, from the \href{https://radimrehurek.com/gensim/models/word2vec.html}{official documentation}.
#' 
#' @details This is a non-parametric bayesian method: notice the lack of \code{num_topics} argument.
#' 
#' @name model_word2vec
#' 
#' @export
model_word2vec <- function(...){
  model <- gensim$models$Word2Vec(...)  
  invisible(model)
}


#' Get Document Topics
#' 
#' @param corpus Corpus.
#' 
#' @name get_docs_topics
#' @export
get_docs_topics <- function(corpus) UseMethod("get_docs_topics")

#' @rdname get_docs_topics
#' @method get_docs_topics gensim.interfaces.CorpusABC
#' @export
get_docs_topics.gensim.interfaces.CorpusABC <- function(corpus){
  l <- reticulate::py_len(corpus)

  docs <- list()
  for(i in 1:l){
    doc <- reticulate::py_to_r(corpus[i - 1])
    docs <- append(docs, list(doc))
  }

  dimensions <- length(docs[[1]])
  vars <- c("x", "y")

  col_names <- paste0("dimension_", 1:dimensions) %>% 
    tidyr::crossing(vars) %>% 
    dplyr::mutate(name = paste0(., "_", vars)) %>% 
    dplyr::pull(name)

  is_empty <- function(x){
    if(length(x) >= 1)
      TRUE
    else
      FALSE
  }

  docs %>% 
    purrr::map(unlist) %>% 
    purrr::keep(is_empty) %>% 
    purrr::map(t) %>% 
    purrr::map_dfr(tibble::as_tibble) %>% 
    purrr::set_names(col_names)
}
