#' Wrap
#' 
#' Wraps \code{y} with \code{x}
#' 
#' @param x,y Python objects.
#' @param to_r Applies \link[reticulate]{py_to_r} on output.
#' 
#' @importFrom graphics plot
#' 
#' @export
wrap <- function(x, y, to_r = FALSE) {
  wrap <- x[y]
  if(to_r)
    return(reticulate::py_to_r(wrap))
  structure(wrap, class = c(class(wrap), "wrapped"))
}
