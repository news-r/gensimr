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
    "filters"
  )
)

.construct_model_collection <- function(x){
  structure(x, class = c("model_collection", class(x)))
}

.get_corpus <- function(...){
  args <- list(...)
  corpus <- args[["corpus"]]
  assert_that(!is.null(corpus), msg = "`corpus` argument is required.")
  return(corpus)
}

.get_perplexity_data <- function(x){
  purrr::map_dfr(x, function(x){
    tibble::tibble(
      topics = x$n_topics,
      perplexity = x$perplexity
    )
  })
}