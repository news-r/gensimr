#' Author-topic Model
#' 
#' @param corpus Model as returned by \code{\link{mmcorpus_serialize}}.
#' @param ... Any other options, from the ¨
#' \href{https://radimrehurek.com/gensim/models/atmodel.html}{official documentation}.
#' 
#' @name sklearn_atmodel
#' 
#' @seealso \code{\link{get_author_topics}}
#' 
#' @export
sklearn_atmodel <- function(corpus, ...) UseMethod("sklearn_atmodel")

#' @rdname sklearn_atmodel
#' @method sklearn_atmodel mm_file
#' @export
sklearn_atmodel.mm_file <- function(corpus, ...){
  assert_that(!missing(corpus), msg = "Missing `corpus`")
  
  corpus_read <- gensim$corpora$MmCorpus(corpus$file)
  model <- gensim$models$AuthorTopicModel(corpus_read, ...)

  # unlink temp
  if(corpus$temp && corpus$delete) unlink(corpus$file)
  
  invisible(model)
}

#' @rdname sklearn_atmodel
#' @method sklearn_atmodel mm
#' @export
sklearn_atmodel.mm <- function(corpus,...){
  assert_that(!missing(corpus), msg = "Missing `corpus`")
  model <- gensim$models$AuthorTopicModel(corpus, ...)
  invisible(model)
}

#' Get Author topics
#' 
#' Construct vectors of topics for each author.
#' 
#' @param auth2doc Output of \code{\link{sklearn_atmodel}}.
#' 
#' @name get_author_topics
#' 
#' @export
get_author_topics <- function(auth2doc) UseMethod("get_author_topics")

#' @rdname get_author_topics
#' @method get_author_topics gensim.models.atmodel.AuthorTopicModel
#' @export
get_author_topics.gensim.models.atmodel.AuthorTopicModel <- function(auth2doc){
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