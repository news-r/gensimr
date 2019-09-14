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

#' @method model_coherence gensim.models.basemodel.BaseTopicModel
#' @export
model_coherence.gensim.models.basemodel.BaseTopicModel <- function(models, ...){
  gensim$models$CoherenceModel(model = models, ...)
}

#' @export
model_coherence.default <- function(models, ...){
  gensim$models$CoherenceModel$for_models(models = models, ...)
}

#' Map Coherence
#' 
#' Compute topic coherence on multiple models to assess best model.
#' 
#' @param models An object of class \code{model_collection} as returned by
#' \code{map_model}.
#' @param ... Any other argument that would normally be passed to 
#' \code{\link{model_coherence}}.
#' 
#' @name map_coherence
#' @export
map_coherence <- function(models, ...) UseMethod("map_coherence")

#' @method map_coherence model_collection
#' @export
map_coherence.model_collection <- function(models, ...){
  coherence <- models %>% 
    purrr::map("model") %>% 
    purrr::map(model_coherence, ...) %>% 
    purrr::map(function(x){
      list(
        coherence_model = x,
        coherence = x$get_coherence() %>% reticulate::py_to_r()
      )
    })
  
  purrr::map2(models, coherence, function(x, y){
    append(x, y)
  }) %>% 
    .construct_model_collection("model_coherence")
}


#' @rdname map_coherence
#' @export
get_coherence_data <- function(models) UseMethod("get_coherence_data")

#' @method get_coherence_data model_collection
#' @export
get_coherence_data.model_collection <- function(models){
  data <- .get_coherence_data(models)
  data$coherence_model <- purrr::map(models, "coherence_model") %>% unlist()
  return(data)
}