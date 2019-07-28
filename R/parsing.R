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
  s %>% 
    purrr::map(gensim$parsing$preprocessing$remove_stopwords) %>% 
    purrr::map(reticulate::py_to_r) %>% 
    unlist()
}

#' @rdname remove_stopwords
#' @method remove_stopwords list
#' @export
remove_stopwords.list <- function(s, ...){
  s %>% 
    purrr::map(gensim$parsing$preprocessing$remove_stopwords) %>% 
    purrr::map(reticulate::py_to_r) %>% 
    unlist()
}

#' @rdname remove_stopwords
#' @method remove_stopwords data.frame
#' @export
remove_stopwords.data.frame <- function(s, text, ...){
  assert_that(!missing(text), msg = "Missing `text`")
  s %>% 
    dplyr::pull(!!dplyr::enquo(text)) %>% 
    purrr::map(gensim$parsing$preprocessing$remove_stopwords) %>% 
    purrr::map(reticulate::py_to_r) %>% 
    unlist()
}

#' Strip Short Words
#' 
#' Remove words less than a certain length.
#' 
#' @param s A Character string or data.frame.
#' @param text bare name of text column.
#' @param min_len Minimum word length.
#' @param ... Any other options.
#' 
#' @name strip_short
#' 
#' @export
strip_short <- function(s, min_len = 5, ...) UseMethod("strip_short")

#' @rdname strip_short
#' @method strip_short character
#' @export
strip_short.character <- function(s, min_len = 5, ...){
  s %>% 
    purrr::map(gensim$parsing$preprocessing$strip_short, minsize = min_len) %>% 
    purrr::map(reticulate::py_to_r) %>% 
    unlist()
}

#' @rdname strip_short
#' @method strip_short list
#' @export
strip_short.list <- function(s, min_len = 5, ...){
  s %>% 
    purrr::map(gensim$parsing$preprocessing$strip_short, minsize = min_len) %>% 
    purrr::map(reticulate::py_to_r) %>% 
    unlist()
}

#' @rdname strip_short
#' @method strip_short data.frame
#' @export
strip_short.data.frame <- function(s, min_len = 5, text, ...){
  assert_that(!missing(text), msg = "Missing `text`")
  s %>% 
    dplyr::pull(!!dplyr::enquo(text)) %>% 
    purrr::map(gensim$parsing$preprocessing$strip_short, minsize = min_len) %>% 
    purrr::map(reticulate::py_to_r) %>% 
    unlist()
}

#' Preprocess text
#' 
#' Remove stopwords from a character string.
#' 
#' @param s A Character string or data.frame.
#' @param text bare name of text column.
#' @param ... Any other options.
#' 
#' @name preprocess
#' 
#' @export
preprocess <- function(s, ...) UseMethod("preprocess")

#' @rdname preprocess
#' @method preprocess character
#' @export
preprocess.character <- function(s, ...){
  gensim$parsing$preprocessing$preprocess_documents(s) %>% 
    reticulate::py_to_r()
}

#' @rdname preprocess
#' @method preprocess list
#' @export
preprocess.list <- function(s, ...){
  gensim$parsing$preprocessing$preprocess_documents(s) %>% 
    reticulate::py_to_r()
}

#' @rdname preprocess
#' @method preprocess data.frame
#' @export
preprocess.data.frame <- function(s, text, ...){
  assert_that(!missing(text), msg = "Missing `text`")
  s %>% 
    dplyr::pull(!!dplyr::enquo(text)) %>% 
    gensim$parsing$preprocessing$preprocess_documents() %>% 
    reticulate::py_to_r()
}

#' Split Alphanumerics
#' 
#' Split Alphanumerics from a character string.
#' 
#' @param s A Character string or data.frame.
#' @param text bare name of text column.
#' @param ... Any other options.
#' 
#' @name split_alphanum
#' 
#' @export
split_alphanum <- function(s, ...) UseMethod("split_alphanum")

#' @rdname split_alphanum
#' @method split_alphanum character
#' @export
split_alphanum.character <- function(s, ...){
  s %>% 
    purrr::map(gensim$parsing$preprocessing$split_alphanum) %>% 
    purrr::map(reticulate::py_to_r) %>% 
    unlist()
}

#' @rdname split_alphanum
#' @method split_alphanum list
#' @export
split_alphanum.list <- function(s, ...){
  s %>% 
    purrr::map(gensim$parsing$preprocessing$split_alphanum) %>% 
    purrr::map(reticulate::py_to_r) %>% 
    unlist()
}

#' @rdname split_alphanum
#' @method split_alphanum data.frame
#' @export
split_alphanum.data.frame <- function(s, text, ...){
  assert_that(!missing(text), msg = "Missing `text`")
  s %>% 
    dplyr::pull(!!dplyr::enquo(text)) %>% 
    purrr::map(gensim$parsing$preprocessing$split_alphanum) %>% 
    purrr::map(reticulate::py_to_r) %>% 
    unlist()
}