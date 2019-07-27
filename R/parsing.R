#' Porter Stemmer
#' 
#' The Porter stemming algorithm (or ‘Porter stemmer’) is a process 
#' for removing the commoner morphological and inflexional endings 
#' from words in English. Its main use is as part of a term normalisation 
#' process that is usually done when setting up Information Retrieval systems.
#' 
#' @name porter_stemmer
#' 
#' @export
porter_stemmer <- function() {
  model <- gensim$parsing$porter$PorterStemmer()
  invisible(model)
}

#' Remove stopwords
#' 
#' Remove stopwords from a character string.
#' 
#' @param s A Character string or data.frame.
#' @param text bare name of text column.
#' @param ... Any other options.
#' 
#' @name remove_stopwords
#' 
#' @export
remove_stopwords <- function(s, ...) UseMethod("remove_stopwords")

#' @rdname remove_stopwords
#' @method remove_stopwords character
#' @export
remove_stopwords.character <- function(s, ...){
  gensim$parsing$preprocessing$remove_stopwords(s)
}

#' @rdname remove_stopwords
#' @method remove_stopwords data.frame
#' @export
remove_stopwords.data.frame <- function(s, text, ...){
  assert_that(!missing(text), msg = "Missing `text`")
  txt <- dplyr::pull(!!dplyr::enquo(text))
  purrr::map(txt) %>% 
    gensim$parsing$preprocessing$remove_stopwords(s) %>% 
    unlist()
}