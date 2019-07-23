#' Create a dictionary
#' 
#' Create a dictionarry from a list of documents.
#' 
#' @param docs A list of documents, for instance as returned by \code{\link{preprocess}}.
#' 
#' @export
corpora_dictionary <- function(docs){
  assert_that(!missing(docs), msg = "Missing `docs`")
  docs <- unname(docs)
  gensim$corpora$Dictionary(docs)
}