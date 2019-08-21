#' Similarity Matrix
#' 
#' @param corpus Corpus.
#' @param ... Any other arguments.
#' 
#' @name similarity_matrix
#' 
#' @export
similarity_matrix <- function(corpus, ...) UseMethod("similarity_matrix")

#' @rdname similarity_matrix
#' @method similarity_matrix wrapped
#' @export
similarity_matrix.wrapped <- function(corpus, ...){
  gensim$similarities$MatrixSimilarity(corpus)
}

#' Get Similarity
#' 
#' Returns Documents by order of similarity.
#' 
#' @param sim A wrapped similarity, output of \code{\link{wrap}}.
#' 
#' @return Documents by order of similarity, 
#' the most similar at the top and the least similar at the bottom.
#' 
#' @name get_similarity
#' 
#' @export
get_similarity <- function(sim) UseMethod("get_similarity")

#' @rdname get_similarity
#' @method get_similarity wrapped
#' @export
get_similarity.wrapped <- function(sim){
  enum <- bi$enumerate(sim)
  lst <- bi$list(enum)
  lst <- unlist(lst[[1]][[2]])
  tibble::tibble( 
    doc = 1:length(lst),
    cosine = lst
  ) %>% 
    dplyr::mutate(doc = doc - 1) %>% 
    dplyr::arrange(-cosine)
}