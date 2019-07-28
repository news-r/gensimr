#' BM 25
#' 
#' Function to compute rank scores for documents
#' in corpus and helper class BM25 used in calculations.
#' 
#' @param mm A corpus.
#' @param n_jobs The number of processes to use for computing bm25.
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
  model <- gensim$summarization$bm25$get_bm25_weights(mm, n_jobs = -1) %>% 
    reticulate::py_to_r()
  invisible(model)
}

#' Keywords
#' 
#' Find keywords from text.
#' 
#' @param corpus A corpus.
#' @param ... Any other options, from the \href{https://radimrehurek.com/gensim/summarization/keywords.html}{official documentation}.
#' 
#' @name keywords
#' 
#' @export
keywords <- function(corpus, ...) UseMethod("keywords")

#' @rdname keywords
#' @method keywords character
#' @export
keywords.character <- function(corpus, ...){
  kw <- corpus %>% 
    purrr::map(gensim$summarization$keywords, ...) %>% 
    purrr::map(reticulate::py_to_r) %>% 
    purrr::map(strsplit, "\n") %>% 
    purrr::map(function(x) x[[1]])
  invisible(kw)
}

#' @rdname keywords
#' @method keywords character
#' @export
keywords.factor <- keywords.character

#' @rdname keywords
#' @method keywords list
#' @export
keywords.list <- function(corpus, ...){
  kw <- corpus %>% 
    purrr::map(gensim$summarization$keywords, ...) %>% 
    purrr::map(reticulate::py_to_r) %>% 
    purrr::map(strsplit, "\n") %>% 
    purrr::map(function(x) x[[1]])
  invisible(kw)
}

#' summarize
#' 
#' Text summarization.
#' 
#' @param corpus A corpus.
#' @param ... Any other options, from the \href{https://radimrehurek.com/gensim/summarization/summariser.html}{official documentation}.
#' 
#' @name summarize
#' 
#' @export
summarize <- function(corpus, ...) UseMethod("summarize")

#' @rdname summarize
#' @method summarize character
#' @export
summarize.character <- function(corpus, ...){
  kw <- corpus %>% 
    purrr::map(gensim$summarization$summarize, ...) %>% 
    purrr::map(reticulate::py_to_r) 
  invisible(kw)
}

#' @rdname summarize
#' @method summarize character
#' @export
summarize.factor <- summarize.character

#' @rdname summarize
#' @method summarize python.builtin.tuple
#' @export
summarize.python.builtin.tuple <- function(corpus, ...){
  kw <- corpus  %>% 
    gensim$summarization$summarize_corpus() %>% 
    purrr::map(reticulate::py_to_r) 
  invisible(kw)
}


#' @rdname summarize
#' @method summarize mm_file
#' @export
summarize.mm_file <- function(corpus, ...){
  corpus <- gensim$corpora$MmCorpus(corpus$file)
  model <- gensim$summarization$summarize_corpus(corpus, ...) %>% 
    purrr::map(reticulate::py_to_r) 

  # unlink temp
  if(corpus$temp && corpus$delete) unlink(corpus$file)

  invisible(model)
}

#' @rdname summarize
#' @method summarize mm
#' @export
summarize.mm <- function(corpus, ...){
  model <- gensim$summarization$summarize_corpus(corpus, ...) %>% 
    purrr::map(reticulate::py_to_r) 
  invisible(model)
}