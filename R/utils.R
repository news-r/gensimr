globalVariables(
  c(
    ".",
    "cosine",
    "doc",
    "name",
    "word",
    "text",
    "n",
    "id",
    "tokens",
    "data",
    "documents",
    "args",
    "func",
    "filters",
    "coherence"
  )
)

.construct_model_collection <- function(x, ...){
  structure(x, class = c(..., "model_collection", class(x)))
}

.get_corpus <- function(...){
  args <- list(...)
  corpus <- args[["corpus"]]
  assert_that(!is.null(corpus), msg = "`corpus` argument is required.")
  return(corpus)
}

.get_perplexity_data <- function(models){
  purrr::map_dfr(models, function(x){
    assert_that(!is.na(x$perplexity), msg = "Model perplexity was not computed.")
    tibble::tibble(
      num_topics = x$num_topics,
      perplexity = x$perplexity
    )
  })
}

.get_coherence_data <- function(models){
  purrr::map_dfr(models, function(x){
    assert_that(length(x$coherence) > 0, msg = "Topic coherence was not computed.")
    tibble::tibble(
      num_topics = x$num_topics,
      coherence = x$coherence
    )
  })
}