#' PoinCare Model
#' 
#' Implementation of Poincaré Embeddings.
#' 
#' @param relations Training data. Note that the relations are treated as 
#' ordered pairs, i.e. a relation (a, b) does not imply the opposite 
#' relation (b, a). In case the relations are symmetric, the data should 
#' contain both relations (a, b) and (b, a).
#' @param ... Any other options, from the \href{https://radimrehurek.com/gensim/models/poincare.html}{official documentation}.
#' 
#' @name model_poincare
#' 
#' @export
model_poincare <- function(relations, ...) UseMethod("model_poincare")

#' @rdname model_poincare
#' @method model_poincare list
#' @export
model_poincare.list <- function(relations, ...){
  model <- gensim$models$poincare$PoincareModel(train_data = relations, ...)
  invisible(model)
}

#' @rdname model_poincare
#' @method model_poincare python.builtin.tuple
#' @export
model_poincare.python.builtin.tuple <- model_poincare.list

#' @rdname model_poincare
#' @method model_poincare character
#' @export
model_poincare.character <- function(relations, ...){
  rel <- gensim$models$poincare$PoincareRelations(relations)
  model <- gensim$models$poincare$PoincareModel(train_data = rel, ...)
  invisible(model)
}

#' @rdname model_poincare
#' @method model_poincare python.builtin.str
#' @export
model_poincare.python.builtin.str <- model_poincare.character
