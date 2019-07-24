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
  if(mm$temp) unlink(mm$file)

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
#' @param corpus Model as returned by \code{\link{corpora_transform}}. A tf-idf/bag-of-words transformation is recommended for LSI.
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
#' @method model_lsi transformed_corpus
#' @export
model_lsi.transformed_corpus <- function(corpus, num_topics = 2L, distributed = FALSE, ...){
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
#' @param corpus Model as returned by \code{\link{corpora_transform}}. A tf-idf/bag-of-words transformation is recommended for LSI.
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
#' @method model_rp transformed_corpus
#' @export
model_rp.transformed_corpus <- function(corpus, num_topics = 2L, ...){
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
#' @param num_topics Number of requested factors (latent dimensions).
#' @param ... Any other options, from the \href{https://radimrehurek.com/gensim/models/rpmodel.html}{official documentation}.
#' 
#' @details Target dimensionality (\code{num_topics}) of 200–500 is recommended as a “golden standard” \url{https://dl.acm.org/citation.cfm?id=1458105}.
#' 
#' @name model_lda
#' 
#' @export
model_lda <- function(corpus, num_topics = 2L, ...) UseMethod("model_lda")

#' @rdname model_lda
#' @method model_lda mm_file
#' @export
model_lda.mm_file <- function(corpus, num_topics = 2L, ...){
  assert_that(!missing(corpus), msg = "Missing `corpus`")
  num_topics <- as.integer(num_topics) # enforce integer
  
  if(num_topics < 200)
    cat(crayon::yellow(cli::symbol$warning), "Low number of topics\n")
  
  corpus_read <- gensim$corpora$MmCorpus(corpus$file)
  model <- gensim$models$LdaModel(corpus_read, num_topics = num_topics, ...)

  # unlink temp
  if(corpus$temp) unlink(corpus$file)
  
  invisible(model)
}

#' @rdname model_lda
#' @method model_lda mm
#' @export
model_lda.mm <- function(corpus, num_topics = 2L, ...){
  assert_that(!missing(corpus), msg = "Missing `corpus`")
  num_topics <- as.integer(num_topics) # enforce integer
  
  if(num_topics < 200)
    cat(crayon::yellow(cli::symbol$warning), "Low number of topics\n")
  
  model <- gensim$models$RpModel(corpus, num_topics = num_topics, ...)
  
  invisible(model)
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
