gensim <- sklearn <- ldavis <- bi <- NULL

.onLoad <- function(...) {
  gensim <<- reticulate::import("gensim", delay_load = TRUE, convert = FALSE)
  sklearn <<- reticulate::import("sklearn", delay_load = TRUE, convert = FALSE)
  ldavis <<- reticulate::import("pyLDAvis", delay_load = TRUE, convert = FALSE)
  bi <<- reticulate::import_builtins()
}
