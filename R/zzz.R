gensim <- NULL

.onLoad <- function(libname, pkgname) {
  gensim <<- reticulate::import("gensim", delay_load = TRUE, convert = FALSE)
}