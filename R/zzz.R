gensim <- sklearn <- NULL

.onLoad <- function(libname, pkgname) {
  gensim <<- reticulate::import("gensim", delay_load = TRUE, convert = FALSE)
  sklearn <<- reticulate::import("sklearn", delay_load = TRUE, convert = FALSE)
}