#' Preprocessed Text
#' 
#' gensim built-in preprocessed texts.
#' 
#' @examples
#' common_texts()
#' common_corpus()
#' common_dictionary()
#' 
#' @name utils
#' @export
common_texts <- function() {
  reticulate::py_to_r(gensim$test$utils$common_texts)
}

#' @rdname utils
#' @export
common_corpus <- function() {
  reticulate::py_to_r(gensim$test$utils$common_corpus)
}

#' @rdname utils
#' @export
common_dictionary <- function() {
  reticulate::py_to_r(gensim$test$utils$common_dictionary)
}