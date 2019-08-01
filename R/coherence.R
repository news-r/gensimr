#' Topic Coherence
#' 
#' Calculate topic coherence for topic models.
#' 
#' @param models A model, i.e.: LDA or LSI, or a \code{list} of the latter.
#' @param ... Any other options, from the \href{https://radimrehurek.com/gensim/models/coherencemodel.html}{official documentation}.
#' 
#' @details A greater coherence is preferred: a higher value on the \code{get_coherence} method, see example.
#' 
#' @examples
#' # preprocess the corpus
#' texts <- prepare_documents(corpus)
#' dictionary <- corpora_dictionary(texts)
#' corpus <- doc2bow(dictionary, texts)
#' 
#' # create 2 models to compare
#' good_lda_model <- model_lda(
#'   corpus = corpus, 
#'   id2word = dictionary, 
#'   iterations = 50L, 
#'   num_topics = 2L
#' )
#' bad_lda_model <- model_lda(
#'   corpus = corpus, 
#'   id2word = dictionary, 
#'   iterations = 1L, 
#'   num_topics = 5L
#' )
#' 
#' # create coherence models
#' good_cm <- model_coherence(
#'   model = good_lda_model, 
#'   corpus = corpus, 
#'   dictionary = dictionary, 
#'   coherence = 'u_mass'
#' )
#' bad_cm <- model_coherence(
#'   model = bad_lda_model, 
#'   corpus = corpus, 
#'   dictionary = dictionary, 
#'   coherence = 'u_mass'
#' )
#' 
#' # compare coherence
#' good_cm$get_coherence()
#' bad_cm$get_coherence()
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
