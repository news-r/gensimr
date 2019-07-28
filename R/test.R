#' Preprocessed Text
#' 
#' gensim built-in preprocessed texts.
#' 
#' @param to_r Whether to return R data scruture.
#' 
#' @examples
#' common_texts()
#' common_corpus()
#' common_dictionary()
#' 
#' @name utils
#' @export
common_texts <- function(to_r = TRUE) {
  data <- gensim$test$utils$common_texts
  
  if(to_r)
    data <- reticulate::py_to_r(data)
  
  return(data)
}

#' @rdname utils
#' @export
common_corpus <- function(to_r = TRUE) {
  data <- gensim$test$utils$common_corpus

  if(to_r)
    data <- reticulate::py_to_r(data)
  
  return(data)
}

#' @rdname utils
#' @export
common_dictionary <- function(to_r = TRUE) {
  data <- gensim$test$utils$common_dictionary

  if(to_r)
    data <- reticulate::py_to_r(data)
  
  return(data)
}