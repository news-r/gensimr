#' Author Topic Model
#' 
#' Scikit-learn wrapper for author topic model.
#' 
#' @param dict Dictionary as returned by \code{\link{corpora_dictionary}}.
#' @param ... Any other options, from the ¨
#' \href{https://radimrehurek.com/gensim/models/atmodel.html}{official documentation}.
#' 
#' @name sklearn_at
#' 
#' @seealso \code{\link{get_author_topics}}
#' 
#' @export
sklearn_at <- function(dict, ...) UseMethod("sklearn_at")

#' @rdname sklearn_at
#' @method sklearn_at gensim.corpora.dictionary.Dictionary
#' @export
sklearn_at.gensim.corpora.dictionary.Dictionary <- function(dict, ...){
  assert_that(!missing(dict), msg = "Missing `dict`")
  model <- gensim$sklearn_api$atmodel$AuthorTopicTransformer(id2word = dict, ...)
  invisible(model)
}