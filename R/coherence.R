#' Topic Coherence
#' 
#' Calculate topic coherence for topic models.
#' 
#' @param models A model, i.e.: LDA or LSI, or a \code{list} of the latter.
#' @param ... Any other options, from the \href{https://radimrehurek.com/gensim/models/coherencemodel.html}{official documentation}.
#'  
#' @name topics
#' @export
model_coherence <- function(models, ...) UseMethod("model_coherence")

#' @rdname topics
#' @method model_coherence gensim.models.basemodel.BaseTopicModel
#' @export
model_coherence.gensim.models.basemodel.BaseTopicModel <- function(models, ...){
  gensim$models$CoherenceModel(model = models, ...)
}

#' @rdname topics
#' @method model_coherence list
#' @export
model_coherence.list <- function(models, ...){
  gensim$models$CoherenceModel$for_models(models = models, ...)
}
