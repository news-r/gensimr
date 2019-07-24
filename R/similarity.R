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