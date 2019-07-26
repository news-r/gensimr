#' Install Dependencies
#' 
#' Install Python dependencies. Arguments are passed to \link[reticulate]{py_install}.
#' 
#' @param envname Name of environment to install packages into
#' @param method Installation method. By default, "auto" automatically 
#' finds a method that will work in the local environment. 
#' Change the default to force a specific installation method. 
#' Note that the "virtualenv" method is not available on Windows.
#' @param conda Path to conda executable (or "auto" to find conda using the
#' \code{PATH} and other conventional install locations).
#' 
#' @examples
#' \dontrun{install_gensim()}
#' 
#' @import assertthat
#' 
#' @name dependencies
#' @export
install_gensim <- function(envname = NULL, method = "auto", conda = "auto") {
  reticulate::py_install("gensim", envname = envname, method = method, conda = conda)
}

#' @name dependencies
#' @export
install_sklearn <- function(envname = NULL, method = "auto", conda = "auto") {
  reticulate::py_install("sklearn", envname = envname, method = method, conda = conda)
}