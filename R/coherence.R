#' Topic Coherence
#' 
#' Calculate topic coherence for topic models.
#' 
#' @param models A model, i.e.: LDA or LSI, or a \code{list} of the latter.
#' @param dictionary A dictionary as returned by \code{\link{corpora_dictionary}}.
#' @param ... Any other options, from the \href{https://radimrehurek.com/gensim/models/coherencemodel.html}{official documentation}.
#'  
#' @name topics
#' @export
topic_coherence <- function(models, ...) UseMethod("topic_coherence")

#' @rdname topics
#' @method topic_coherence gensim.models.basemodel.BaseTopicModel
#' @export
topic_coherence.gensim.models.basemodel.BaseTopicModel <- function(models, ...){
  gensim$models$coherencemodel$CoherenceModel(model = models, ...)
}

#' @rdname topics
#' @method topic_coherence list
#' @export
topic_coherence.list <- function(models, dictionary, ...){
  assert_that(!missing(dictionary), msg = "Missing `dictionary`")
  gensim$models$coherencemodel$CoherenceModel$for_models(models = models, dictionary = dictionary, ...)
}
