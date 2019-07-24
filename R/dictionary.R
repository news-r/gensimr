#' Create a dictionary
#' 
#' Create a dictionary from a list of documents.
#' 
#' @param docs A list of documents, for instance as returned by \code{\link{preprocess}}.
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