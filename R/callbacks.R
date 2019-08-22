#' Callbacks
#' 
#' Callbacks can be used to observe the training process.
#' 
#' @param ... Any option from href{https://radimrehurek.com/gensim/models/callbacks.html#gensim.models.callbacks.CoherenceMetric}{official documentation}.
#' 
#' @name callbacks
#' @export
coherence_metric <- function(...) {
  cb <- gensim$models$callbacks$CoherenceMetric(...) %>% 
    .construct_callback()
  invisible(cb)
}

#' @rdname callbacks
#' @export
perplexity_metric <- function(...) {
  cb <- gensim$models$callbacks$PerplexityMetric(...) %>% 
    .construct_callback()
  invisible(cb)
}

#' @name callbacks
#' @export
diff_metric <- function(...) {
  cb <- gensim$models$callbacks$DiffMetric(...) %>% 
    .construct_callback()
  invisible(cb)
}

#' @name callbacks
#' @export
convergence_metric <- function(...) {
  cb <- gensim$models$callbacks$ConvergenceMetric(...) %>% 
    .construct_callback()
  invisible(cb)
}

#' @rdname callbacks
#' @export
combine_metrics <- function(...) {
  metrics <- reticulate::tuple(...) %>% 
    .construct_callback()
  invisible(metrics)
}

.construct_callback <- function(x){
  structure(x, class = c("callback", class(x)))
}

#' @export
print.callback <- function(x, ...){
  cat(
    crayon::blue(cli::symbol$info),
    "A callback function\n",
    ...
  )
}