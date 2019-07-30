#' Wrap
#' 
#' Wraps \code{y} with \code{x}
#' 
#' @param x,y Python objects.
#' 
#' @export
wrap <- function(x, y) {
  wrap <- x[y]
  structure(wrap, class = c(class(wrap), "wrapped"))
}
