#' Preprocessed Text
#' 
#' gensim built-in preprocessed texts.
#' 
#' @examples
#' common_texts()
#' 
#' @export
common_texts <- function() {
  reticulate::py_to_r(gensim$test$utils$common_texts)
}