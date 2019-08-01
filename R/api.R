#' API
#' 
#' This is an API for downloading, getting information and loading datasets/models. 
#' See [RaRe-Technologies/gensim-data](https://github.com/RaRe-Technologies/gensim-data) 
#' repo for more information about models/datasets/how-to-add-new/etc.
#' 
#' @param name A specific dataset, i.e.: \code{text8}.
#' @param to_r Whether to return the information in R data structure (\code{list}).
#' @param ... Any other argument from the \href{https://radimrehurek.com/gensim/downloader.html}{official documentation}.
#' 
#' @examples
#' \dontrun{
#' # available models to use with `api_load`
#' (models <- api_info() %>% names())
#' 
#' # load random model
#' api_load(sample(models, 1))
#' }
#' 
#' @name api
#' 
#' @export
api_info <- function(name = NULL, ..., to_r = TRUE){
  info <- gensim$downloader$info(name = name, ...)
  if(to_r)
    info <- reticulate::py_to_r(info)
  return(info)
}

#' @rdname api
#' @export
api_load <- function(name, ...){
  assert_that(!missing(name), msg = "Missing `name`")
  gensim$downloader$load(name = name, ...)
}