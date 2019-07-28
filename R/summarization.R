#' BM 25
#' 
#' Function to compute rank scores for documents
#' in corpus and helper class BM25 used in calculations.
#' 
#' @param mm A corpus.
#' @param n_jobs The number of processes to use for computing bm25.
#' @param ... Any other options, from the \href{https://radimrehurek.com/gensim/summarization/bm25.html}{official documentation}.
#' 
#' @name get_bm25_weights
#' 
#' @export
get_bm25_weights <- function(mm, n_jobs = -1) UseMethod("get_bm25_weights")

#' @rdname get_bm25_weights
#' @method get_bm25_weights mm_file
#' @export
get_bm25_weights.mm_file <- function(mm, n_jobs = -1){
  assert_that(!missing(mm), msg = "Missing `mm_file`")
  corpus <- gensim$corpora$MmCorpus(mm$file)
  model <- gensim$summarization$bm25$get_bm25_weights(corpus, n_jobs = -1)

  # unlink temp
  if(mm$temp && mm$delete) unlink(mm$file)

  invisible(model)
}

#' @rdname get_bm25_weights
#' @method get_bm25_weights mm
#' @export
get_bm25_weights.mm <- function(mm, n_jobs = -1){
  assert_that(!missing(mm), msg = "Missing `mm`")
  model <- gensim$summarization$bm25$get_bm25_weights(mm, n_jobs = -1)

  invisible(model)
}

#' @rdname get_bm25_weights
#' @method get_bm25_weights list
#' @export
get_bm25_weights.list <- function(mm, n_jobs = -1){
  assert_that(!missing(mm), msg = "Missing `mm`")
  model <- gensim$summarization$bm25$get_bm25_weights(mm, n_jobs = -1)

  invisible(model)
}