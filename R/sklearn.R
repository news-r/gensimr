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
#' \href{https://radimrehurek.com/gensim/sklearn_api/lsimodel.html}{official documentation}.
#' 
#' @name sklearn_lsi
#' 
#' @export
sklearn_lsi <- function(...){
  model <- gensim$sklearn_api$LsiTransformer(...)
  invisible(model)
}

#' Scikit-learn Logistic Regression
#' 
#' Initialise Scikit-learn Logistic Regression models.
#' 
#' @param ... Any other options, from the
#' \href{https://scikit-learn.org/stable/modules/generated/sklearn.linear_model.LogisticRegression.html}{official documentation}.
#' 
#' @name sklearn_logistic
#' 
#' @export
sklearn_logistic <- function(...){
  model <- sklearn$linear_model$LogisticRegression(...)
  invisible(model)
}

#' Scikit-learn Pipeline
#' 
#' @param model Model as returned by \code{\link{sklearn_lsi}}.
#' @param clf A classifier as returned by \code{\link{sklearn_logistic}}.
#'
#' @name sklearn_pipeline
#' 
#' @export
sklearn_pipeline <- function(model, clf){
  assert_that(!missing(model), msg = "Missing `model`")
  assert_that(!missing(clf), msg = "Missing `clf`")

  model <- sklearn$pipeline$Pipeline(
    list(
      list(
        "features",
        model
      ),
      list(
        "classifier",
        clf 
      )
    )
  )

  invisible(model)
}

#' Phrase (Colocation) Detection
#' 
#' Phrase Detection with scikit-learn phrase transformer.
#' 
#' @param ... Any other options, from the
#' \href{https://radimrehurek.com/gensim/sklearn_api/phrases.html}{official documentation}.
#' 
#' @name sklearn_pt
#' 
#' @export
sklearn_pt <- function(...){
  model <- gensim$sklearn_api$phrases$PhrasesTransformer(...)
  invisible(model)
}

#' Random Project Model
#' 
#' Initialize random project models with scikit-learn's transformer.
#' 
#' @param ... Any other options, from the
#' \href{https://radimrehurek.com/gensim/sklearn_api/rpmodel.html}{official documentation}.
#' 
#' @name sklearn_rp
#' 
#' @export
sklearn_rp <- function(...){
  model <- gensim$sklearn_api$rpmodel$RpTransformer(...)
  invisible(model)
}

#' Word ID Mapper
#' 
#' Initialize a \code{\link{doc2bow}} mapper in scikit-learn.
#' 
#' @param ... Any other options, from the
#' \href{https://radimrehurek.com/gensim/sklearn_api/text2bow.html}{official documentation}.
#' 
#' @name sklearn_doc2bow
#' 
#' @export
sklearn_doc2bow <- function(...){
  model <- gensim$sklearn_api$Text2BowTransformer(...)
  invisible(model)
}

#' @rdname sklearn_doc2bow
#' @export
sklearn_text2bow <- sklearn_doc2bow

#' Tf-idf Model
#' 
#' Initialize a term frequncy inverse document frequency matrix with scikit-learn.
#' 
#' @param ... Any other options, from the
#' \href{https://radimrehurek.com/gensim/sklearn_api/tfidf.html}{official documentation}.
#' 
#' @name sklearn_tfidf
#' 
#' @export
sklearn_tfidf <- function(...){
  model <- gensim$sklearn_api$TfIdfTransformer(...)
  invisible(model)
}
