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
#' @param file Path to a saved model.
#' 
#' @examples
#' docs <- prepare_documents(corpus)
#' dictionary <- corpora_dictionary(docs)
#' corpora <- doc2bow(dictionary, docs)
#' 
#' # fit model
#' tfidf <- model_tfidf(corpora)
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

#' @method model_tfidf mm_file
#' @export
model_tfidf.mm_file <- function(mm, normalize = FALSE, smart = "nfc", pivot = NULL, slope = .25, ...){
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

#' @export
model_tfidf.default <- function(mm, normalize = FALSE, smart = "nfc", pivot = NULL, slope = .25, ...){
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


#' @rdname model_tfidf
#' @export
load_tfidf <- function(file){
  assert_that(!missing(file), msg = "Missing `file`")
  gensim$models$TfidfModel$load(file)
}

#' Latent Semantic Indexing Model
#' 
#' Transform into a latent n dimensional space via Latent Semantic Indexing.
#' 
#' @param corpus Corpus as returned by \code{\link{wrap}}. A tf-idf/bag-of-words transformation is recommended for LSI.
#' @param distributed If \code{TRUE} - distributed mode (parallel execution on several machines) will be used.
#' @param ... Any other options, from the \href{https://radimrehurek.com/gensim/models/lsimodel.html}{official documentation}.
#' @param file Path to a saved model.
#' 
#' @examples
#' docs <- prepare_documents(corpus)
#' dictionary <- corpora_dictionary(docs)
#' corpora <- doc2bow(dictionary, docs)
#' 
#' # fit model
#' lsi <- model_lsi(corpora, id2word = dictionary, num_topics = 2L)
#' lsi$print_topics()
#' 
#' @details Target dimensionality (\code{num_topics}) of 200–500 is recommended as a “golden standard” \url{https://dl.acm.org/citation.cfm?id=1458105}.
#' 
#' @name model_lsi
#' 
#' @export
model_lsi <- function(corpus, distributed = FALSE, ...) UseMethod("model_lsi")

#' @method model_lsi wrapped
#' @export
model_lsi.wrapped <- function(corpus, distributed = FALSE, ...){
  gensim$models$LsiModel(corpus, distributed = distributed, ...)
}

#' @method model_lsi list
#' @export
model_lsi.list <- function(corpus, distributed = FALSE, ...){
  gensim$models$LsiModel(corpus, distributed = distributed, ...)
}

#' @export
model_lsi.default <- model_lsi.list

#' @rdname model_lsi
#' @export
load_lsi <- function(file){
  assert_that(!missing(file), msg = "Missing `file`")
  gensim$models$LsiModel$load(file)
}

#' Normalization Model
#' 
#' Compute the l1 or l2 normalization by normalizing separately for each document in a corpus.
#' 
#' @param corpus Model as returned by \code{\link{wrap}}.
#' @param norm Norm used to normalize, defaults to \code{l2}.
#' @param file Path to a saved model.
#' 
#' @examples
#' docs <- prepare_documents(corpus)
#' dictionary <- corpora_dictionary(docs)
#' corpora <- doc2bow(dictionary, docs)
#' 
#' # fit model
#' norm <- model_norm(corpora)
#' 
#' @details Target dimensionality (\code{num_topics}) of 200–500 is recommended as a “golden standard” \url{https://dl.acm.org/citation.cfm?id=1458105}.
#' 
#' @name model_norm
#' 
#' @export
model_norm <- function(corpus, norm = c("l2", "l1")) UseMethod("model_norm")

#' @method model_norm wrapped
#' @export
model_norm.wrapped <- function(corpus, norm = c("l2", "l1")){
  nrom <- tolower(norm)
  norm <- match.arg(norm)
  gensim$models$normmodel$NormModel(corpus, norm)
}

#' @method model_norm list
#' @export
model_norm.list <- function(corpus, norm = c("l2", "l1")){
  nrom <- tolower(norm)
  norm <- match.arg(norm)
  gensim$models$normmodel$NormModel(corpus, norm)
}

#' @export
model_norm.default <- model_norm.list

#' @rdname model_norm
#' @export
load_norm <- function(file){
  assert_that(!missing(file), msg = "Missing `file`")
  gensim$models$normmodel$NormModel$load(file)
}

#' Fasttext Model
#' 
#' Train word embeddings from a training corpus with the additional 
#' ability to obtain word vectors for out-of-vocabulary words.
#' 
#' @param ... Any option, from the \href{https://radimrehurek.com/gensim/models/fasttext.html}{official documentation}.
#' @param file Path to a saved model.
#' 
#' @examples
#' docs <- prepare_documents(corpus)
#' 
#' # fit model
#' ft <- model_fasttext(size = 4L, window = 3L, min_count = 1L)
#' 
#' # build vocabulary
#' ft$build_vocab(sentences = unname(docs))
#' 
#' # train
#' ft$train(sentences = unname(docs), total_examples = length(docs), epochs = 10L)
#' 
#' # most similar
#' ft$wv$most_similar(positive = c('computer', 'human'), negative = c('interface'))
#' 
#' # odd one out
#' ft$wv$doesnt_match(c("human", "computer", "interface", "tree"))
#' 
#' # similarity score
#' ft$wv$similarity('computer', 'human')
#' 
#' @name model_fasttext
#' 
#' @export
model_fasttext <- function(...){
  gensim$models$fasttext$FastText(...)
}

#' @rdname model_fasttext
#' @export
load_fasttext <- function(file){
  assert_that(!missing(file), msg = "Missing `file`")
  gensim$models$fasttext$FastText$load(file)
}

#' Random Projections Model
#' 
#' Reduce vector space dimensionality. This is a very efficient (both memory- and CPU-friendly) 
#' approach to approximating TfIdf distances between documents, by throwing in a little randomness.
#' 
#' @param corpus Corpus as returned by \code{\link{wrap}}. A tf-idf/bag-of-words transformation is recommended for LSI.
#' @param ... Any other options, from the \href{https://radimrehurek.com/gensim/models/rpmodel.html}{official documentation}.
#' @param file Path to a saved model.
#' 
#' @details Target dimensionality (\code{num_topics}) of 200–500 is recommended as a “golden standard” \url{https://dl.acm.org/citation.cfm?id=1458105}.
#' 
#' @name model_rp
#' 
#' @export
model_rp <- function(corpus, ...) UseMethod("model_rp")

#' @method model_rp wrapped
#' @export
model_rp.wrapped <- function(corpus, ...){
  assert_that(!missing(corpus), msg = "Missing `corpus`")
  gensim$models$RpModel(corpus, ...)
}

#' @export
model_rp.default <- model_rp.wrapped

#' @rdname model_rp
#' @export
load_rp <- function(file){
  assert_that(!missing(file), msg = "Missing `file`")
  gensim$models$RpModel$load(file)
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
#' @param file Path to a saved model.
#' 
#' @section Functions:
#' \itemize{
#'   \item{\code{model_lda} - Single-core implementation.}
#'   \item{\code{model_ldamc} - Multi-core implementation.}  
#' }
#' 
#' @details Target dimensionality (\code{num_topics}) of 200–500 is recommended as a “golden standard” \url{https://dl.acm.org/citation.cfm?id=1458105}.
#' 
#' @examples
#' docs <- prepare_documents(corpus)
#' dictionary <- corpora_dictionary(docs)
#' corpora <- doc2bow(dictionary, docs)
#' corpus_mm <- serialize_mmcorpus(corpora, auto_delete = FALSE)
#' 
#' # fit model
#' lda <- model_lda(corpus_mm, id2word = dictionary, num_topics = 2L)
#' lda_topics <- lda$get_document_topics(corpora)
#' get_docs_topics(lda_topics)
#' 
#' @name model_lda
#' 
#' @export
model_lda <- function(corpus, ...) UseMethod("model_lda")

#' @method model_lda mm_file
#' @export
model_lda.mm_file <- function(corpus, ...){
  assert_that(!missing(corpus), msg = "Missing `corpus`")
  
  corpus_read <- gensim$corpora$MmCorpus(corpus$file)
  model <- gensim$models$LdaModel(corpus = corpus_read, ...)

  # unlink temp
  if(corpus$temp && corpus$delete) unlink(corpus$file)
  
  invisible(model)
}

#' @method model_lda mm
#' @export
model_lda.mm <- function(corpus, ...){
  assert_that(!missing(corpus), msg = "Missing `corpus`")
  model <- gensim$models$LdaModel(corpus = corpus, ...)
  invisible(model)
}

#' @export
model_lda.default <- model_lda.mm

#' @rdname model_lda
#' @export
load_lda <- function(file){
  assert_that(!missing(file), msg = "Missing `file`")
  gensim$models$LdaModel$load(file)
}

#' @rdname model_lda
#' @export
model_ldamc <- function(corpus, ...) UseMethod("model_ldamc")

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

#' @method model_ldamc mm
#' @export
model_ldamc.mm <- function(corpus,...){
  assert_that(!missing(corpus), msg = "Missing `corpus`")
  model <- gensim$models$LdaMulticore(corpus, ...)
  invisible(model)
}

#' @export
model_ldamc.default <- model_ldamc.mm

#' @rdname model_lda
#' @export
load_ldamc <- function(file){
  assert_that(!missing(file), msg = "Missing `file`")
  gensim$models$LdaMulticore$load(file)
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
#' @param file Path to a saved model.
#' 
#' @examples
#' docs <- prepare_documents(corpus)
#' dictionary <- corpora_dictionary(docs)
#' corpora <- doc2bow(dictionary, docs)
#' 
#' # fit model
#' log_entropy <- model_logentropy(corpora)
#' wrap(log_entropy, corpora)
#' 
#' @name model_logentropy
#' 
#' @export
model_logentropy <- function(corpus, normalize = FALSE) UseMethod("model_logentropy")

#' @export
model_logentropy.default <- function(corpus, normalize = FALSE){
  assert_that(!missing(corpus), msg = "Missing `corpus`")
  model <- gensim$models$LogEntropyModel(corpus, normalize = normalize)
  invisible(model)
}

#' @rdname model_logentropy
#' @export
load_logentropy <- function(file){
  assert_that(!missing(file), msg = "Missing `file`")
  gensim$models$LogEntropyModel$load(file)
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
#' @param file Path to a saved model.
#' 
#' @details This is a non-parametric bayesian method: notice the lack of \code{num_topics} argument.
#' 
#' @examples
#' docs <- prepare_documents(corpus)
#' dictionary <- corpora_dictionary(docs)
#' corpora <- doc2bow(dictionary, docs)
#' corpus_mm <- serialize_mmcorpus(corpora, auto_delete = FALSE)
#' 
#' # fit model
#' hdp <-  model_hdp(corpus_mm, id2word = dictionary)
#' reticulate::py_to_r(hdp$show_topic(topic_id = 1L, topn = 5L))
#' 
#' @name model_hdp
#' 
#' @export
model_hdp <- function(corpus, id2word, ...) UseMethod("model_hdp")

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

#' @method model_hdp mm
#' @export
model_hdp.mm <- function(corpus, id2word, ...){
  assert_that(!missing(corpus), msg = "Missing `corpus`")
  assert_that(!missing(id2word), msg = "Missing `id2word`")
  
  model <- gensim$models$HdpModel(corpus, id2word, ...)
  
  invisible(model)
}

#' @export
model_hdp.default <- model_hdp.mm

#' @rdname model_hdp
#' @export
load_hdp <- function(file){
  assert_that(!missing(file), msg = "Missing `file`")
  gensim$models$HdpModel$load(file)
}

#' Word2Vec Model
#' 
#' The word2vec algorithms include skip-gram and CBOW models, using either hierarchical softmax or negative sampling.
#' 
#' @param ... Any other options, from the \href{https://radimrehurek.com/gensim/models/word2vec.html}{official documentation}.
#' @param file Path to a saved model.
#' 
#' @examples
#' docs <- prepare_documents(corpus)
#' 
#' # initialise
#' word2vec <- model_word2vec(size = 100L, window = 5L, min_count = 1L)
#' word2vec$build_vocab(docs)
#' word2vec$train(docs, total_examples = word2vec$corpus_count, epochs = 20L)
#' word2vec$init_sims(replace = TRUE)
#' 
#' # use
#' word2vec$wv$most_similar(positive = c("interface"))
#' word2vec$wv$doesnt_match(c("human", "interface", "trees"))
#' word2vec$wv$similarity("human", "trees")
#' 
#' @name model_word2vec
#' 
#' @export
model_word2vec <- function(...){
  model <- gensim$models$Word2Vec(...)  
  invisible(model)
}

#' @rdname model_word2vec
#' @export
load_word2vec <- function(file){
  assert_that(!missing(file), msg = "Missing `file`")
  gensim$models$Word2Vec$load(file)
}

#' Get Document Topics
#' 
#' Get document-topics matrix.
#' 
#' @param corpus Corpus.
#' 
#' @examples
#' docs <- prepare_documents(corpus)
#' dictionary <- corpora_dictionary(docs)
#' corpora <- doc2bow(dictionary, docs)
#' corpus_mm <- serialize_mmcorpus(corpora)
#' tfidf <- model_tfidf(corpus_mm)
#' corpus_transformed <- wrap(tfidf, corpora)
#' 
#' # fit model
#' lsi <- model_lsi(corpus_transformed, id2word = dictionary, num_topics = 2L)
#' wrapped_corpus <- wrap(lsi, corpus_transformed)
#' get_docs_topics(wrapped_corpus)
#' 
#' @name get_documents_topics
#' @export
get_docs_topics <- function(corpus) UseMethod("get_docs_topics")

#' @rdname get_documents_topics
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

#' Author-topic Model
#' 
#' @param corpus Model as returned by \code{\link{mmcorpus_serialize}}.
#' @param ... Any other options, from the ¨
#' \href{https://radimrehurek.com/gensim/models/atmodel.html}{official documentation}.
#' @param file Path to a saved model.
#' 
#' @examples
#' docs <- prepare_documents(corpus)
#' dictionary <- corpora_dictionary(docs)
#' corpora <- doc2bow(dictionary, docs)
#' corpus_mm <- serialize_mmcorpus(corpora)
#' 
#' # fit model
#' auth2doc <- auth2doc(authors, name, document)
#' 
#' \dontrun{
#' # create temp to hold serialized data
#' temp <- tempfile("serialized")
#' 
# build model
#' atmodel <- model_at(
#'   corpus_mm, 
#'   id2word = dictionary, 
#'   author2doc = auth2doc, 
#'   num_topics = 2L, 
#'   serialized = TRUE,
#'   serialization_path = temp
#' )
#' 
#' # delete temp
#' unlink(temp, recursive = TRUE)
#' }
#' 
#' @name model_at
#' 
#' @seealso \code{\link{get_author_topics}}
#' 
#' @export
model_at <- function(corpus, ...) UseMethod("model_at")

#' @method model_at mm_file
#' @export
model_at.mm_file <- function(corpus, ...){
  assert_that(!missing(corpus), msg = "Missing `corpus`")
  
  corpus_read <- gensim$corpora$MmCorpus(corpus$file)
  model <- gensim$models$AuthorTopicModel(corpus_read, ...)

  # unlink temp
  if(corpus$temp && corpus$delete) unlink(corpus$file)
  
  invisible(model)
}

#' @method model_at mm
#' @export
model_at.mm <- function(corpus,...){
  assert_that(!missing(corpus), msg = "Missing `corpus`")
  model <- gensim$models$AuthorTopicModel(corpus, ...)
  invisible(model)
}

#' @rdname model_at
#' @export
load_at <- function(file){
  assert_that(!missing(file), msg = "Missing `file`")
  gensim$models$AuthorTopicModel$load(file)
}

#' Get Author topics
#' 
#' Construct vectors of topics for each author.
#' 
#' @param auth2doc Output of \code{\link{model_at}}.
#' 
#' @examples
#' docs <- prepare_documents(corpus)
#' dictionary <- corpora_dictionary(docs)
#' corpora <- doc2bow(dictionary, docs)
#' corpus_mm <- serialize_mmcorpus(corpora)
#' 
#' # fit model
#' auth2doc <- auth2doc(authors, name, document)
#' 
#' \dontrun{
#' # create temp to hold serialized data
#' temp <- tempfile("serialized")
#' 
# build model
#' atmodel <- model_at(
#'   corpus_mm, 
#'   id2word = dictionary, 
#'   author2doc = auth2doc, 
#'   num_topics = 2L, 
#'   serialized = TRUE,
#'   serialization_path = temp
#' )
#' 
#' # delete temp
#' unlink(temp, recursive = TRUE)
#' get_author_topics(atmodel)
#' }
#' 
#' @name get_author_topics
#' 
#' @export
get_author_topics <- function(auth2doc) UseMethod("get_author_topics")

#' @export
get_author_topics.default <- function(auth2doc){
  assert_that(!missing(auth2doc), msg = "Missing `auth2doc`")

  authors <- auth2doc$id2author %>% 
    reticulate::py_to_r() %>% 
    unname() %>% 
    unlist()

  dict <- purrr::map(authors, auth2doc$get_author_topics) 
  
  dimensions <- reticulate::py_len(dict[[1]])
  vars <- c("x", "y")

  col_names <- paste0("dimension_", 1:dimensions) %>% 
    tidyr::crossing(vars) %>% 
    dplyr::mutate(name = paste0(., "_", vars)) %>% 
    dplyr::pull(name)

  vectors <- dict %>% 
    purrr::map(reticulate::py_to_r) %>% 
    purrr::map(unlist) %>% 
    purrr::map(as.data.frame) %>% 
    purrr::map(t) %>% 
    purrr::map(purrr::set_names, col_names) %>% 
    purrr::map_dfr(dplyr::bind_rows)

  dplyr::bind_cols(
    tibble::tibble(authors = authors),
    vectors
  )
}

#' Map Models
#' 
#' Apply multiple models to given different number of topcis
#' and log the perplexity to assess best model.
#' 
#' @param models An object of class \code{model_collection} as returned by
#' \code{map_model}, or a list of models for \code{as_model_collection}.
#' @param corpus If passed perplexity is computed (recommended).
#' @param num_topics A vector or list indicating number of topics to use for each model.
#' @param model_function A gensimr model function name to use.
#' @param ... Any other argument that would normally be passed to \code{model_function}.
#' 
#' @details \code{model_collection} object inherits from list.
#' 
#' @name map_model
#' @export
map_model <- function(num_topics = seq(2, 98, by = 10), model_function = model_lda, ...){
  assert_that(min(num_topics) > 1, msg = "Minimum of `num_topics` must be greater than 1.")
  
  crps <- .get_corpus(...)

  purrr::map(num_topics, function(x, func, ...){
    func(num_topics = as.integer(x), ...)
  }, func = model_function, ...) %>% 
    purrr::map(function(model, c){
      list(
        model = model,
        perplexity = model$log_perplexity(c) %>% reticulate::py_to_r(),
        num_topics = model$num_topics %>% reticulate::py_to_r()
      )
    }, c = crps) %>% 
      .construct_model_collection()
}

#' @rdname map_model
#' @export
as_model_collection <- function(models, corpus = NULL){
  models %>% 
    purrr::map(function(x, c){
      list(
        model = x,
        perplexity = ifelse(!is.null(c), x$log_perplexity(c) %>% reticulate::py_to_r(), NA),
        num_topics = x$num_topics %>% reticulate::py_to_r()
      )
    }, c = corpus) %>% 
    .construct_model_collection()
}

#' @rdname map_model
#' @export
get_perplexity_data <- function(models) UseMethod("get_perplexity_data")

#' @method get_perplexity_data model_collection
#' @export
get_perplexity_data.model_collection <- function(models){
  data <- .get_perplexity_data(models)
  data$model <- purrr::map(models, "model") %>% unlist()
  return(data)
}

#' @export
print.model_collection <- function(x, ...){
  cat(
    crayon::blue(cli::symbol$info),
    "A collection of", length(x), "models.\n"
  )
}

#' @export
plot.model_collection <- function(x, y, ...){
  x %>% 
    .get_perplexity_data() %>% 
    plot(..., type = "l")
}

#' @export
plot.model_coherence <- function(x, y, ...){
  data <- .get_coherence_data(x)
  
  point <- dplyr::filter(data, coherence == min(coherence))

  plot(data,  type = "l")
  points(point, col = "red", pch = 19)
}