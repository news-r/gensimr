#' Callbacks
#' 
#' Callbacks can be used to observe the training process.
#' 
#' @param ... Any option from href{https://radimrehurek.com/gensim/models/callbacks.html#gensim.models.callbacks.CoherenceMetric}{official documentation}.
#' 
#' @name callbacks
#' @export
coherence_metric <- function(...) {
  cb <- gensim$models$callbacks$CoherenceMetric(...)
  invisible(cb)
}

#' @rdname callbacks
#' @export
perplexity_metric <- function(...) {
  cb <- gensim$models$callbacks$PerplexityMetric(...)
  invisible(cb)
}

#' @name callbacks
#' @export
diff_metric <- function(...) {
  cb <- gensim$models$callbacks$DiffMetric(...)
  invisible(cb)
}

#' @name callbacks
#' @export
convergence_metric <- function(...) {
  cb <- gensim$models$callbacks$ConvergenceMetric(...)
  invisible(cb)
}

#' @rdname callbacks
#' @export
combine_metrics <- function(...) {
  metrics <- reticulate::tuple(...)
  invisible(metrics)
}