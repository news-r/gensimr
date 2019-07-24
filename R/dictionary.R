#' Create a dictionary
#' 
#' Create a dictionary from a list of documents.
#' 
#' @param docs A list of documents as returned by \code{\link{preprocess}}.
#' @param file Path to save \code{.dict}.
#' 
#' @export
corpora_dictionary <- function(docs, file = NULL){
  assert_that(!missing(docs), msg = "Missing `docs`")
  docs <- docs %>% 
    unname() %>% 
    reticulate::tuple()
  dictionary <- gensim$corpora$Dictionary(docs)
  if(!is.null(file))
    dictionary$save(file)
  invisible(dictionary)
}

#' Create a dictionary
#' 
#' Create a dictionary from a list of documents.
#' 
#' @param dictionary A dictionary as returned by \code{\link{corpora_dictionary}}.
#' @param docs A list of documents as returned by \code{\link{preprocess}}.
#' 
#' @details Counts the number of occurrences of each distinct word, 
#' converts the word to its integer word id and returns the result as a sparse vector. 
#' 
#' @export
doc2bow <- function(dictionary, docs){
  purrr::map(docs, dictionary$doc2bow) %>% 
    unname() %>% 
    reticulate::tuple()
}