#' Author Topic Model
#' 
#' Scikit-learn wrapper for author topic model.
#' 
#' @param ... Any other options, from the
#' \href{https://radimrehurek.com/gensim/sklearn_api/atmodel.html}{official documentation}.
#' 
#' @name sklearn_at
#' 
#' @seealso \code{\link{get_author_topics}}
#' 
#' @export
sklearn_at <- function(...){
  model <- gensim$sklearn_api$atmodel$AuthorTopicTransformer(...)
  invisible(model)
}

#' Doc2vec Model
#' 
#' Doc2vec transformer.
#' 
#' @param ... Any other options, from the
#' \href{https://radimrehurek.com/gensim/sklearn_api/d2vmodel.html}{official documentation}.
#' 
#' @name sklearn_doc2vec
#' 
#' @export
sklearn_doc2vec <- function(...){
  model <- gensim$sklearn_api$D2VTransformer(...)
  invisible(model)
}

#' Hierarchical Dirichlet Process Model
#' 
#' Hierarchical Dirichlet Process Model with scikit-learn.
#' 
#' @param ... Any other options, from the
#' \href{https://radimrehurek.com/gensim/sklearn_api/hdp.html}{official documentation}.
#' 
#' @name sklearn_hdp
#' 
#' @export
sklearn_hdp <- function(...){
  model <- gensim$sklearn_api$HdpTransformer(...)
  invisible(model)
}

#' Latent Dirichlet Allocation Model
#' 
#' Latent Dirichlet Allocation Model with scikit-learn.
#' 
#' @param ... Any other options, from the
#' \href{https://radimrehurek.com/gensim/sklearn_api/ldamodel.html}{official documentation}.
#' 
#' @name sklearn_lda
#' 
#' @export
sklearn_lda <- function(...){
  model <- gensim$sklearn_api$LdaTransformer(...)
  invisible(model)
}

#' Latent Semantic Indexing Model
#' 
#' Latent Semantic Indexing Model with scikit-learn.
#' 
#' @param ... Any other options, from the
#' \href{https://radimrehurek.com/gensim/sklearn_api/ldamodel.html}{official documentation}.
#' 
#' @name sklearn_lsi
#' 
#' @export
sklearn_lsi <- function(...){
  model <- gensim$sklearn_api$LdaTransformer(...)
  invisible(model)
}