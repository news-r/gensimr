#' Visualise Latent Dirichlet Allocation Models
#' 
#' Visualise Latent Dirichlet Allocation models with pyLDAvis.
#' 
#' @param model A model as returned by \code{\link{model_lda}}.
#' @param corpus A corpus as returned by \code{\link{doc2bow}}.
#' @param dictionary A dictionary as returned by \code{\link{corpora_dictionary}}.
#' @param prepared_vis Prepapred vis as returned by \code{prepapre_ldavis}.
#' @param ... Additional arguments from \href{https://pyldavis.readthedocs.io/en/latest/modules/API.html}{the official documentation}.
#' 
#' @details \code{\link{plot_ldavis}} is a wrapper around \code{prepapre_ldavis} 
#' and \code{\link{show_ldavis}}. Note that a \code{plot} method that can be used 
#' instead of \code{\link{show_ldavis}}.
#' 
#' @examples
#' docs <- prepare_documents(corpus)
#' dict <- corpora_dictionary(docs)
#' corpora <- doc2bow(dict, docs)
#' 
#' # lda model
#' model <- model_lda(
#'   corpus = corpora, 
#'   id2word = dict, 
#'   iterations = 50L, 
#'   num_topics = 2L
#' )
#' 
#' # visualise
#' vis <- prepare_ldavis(model, corpora, dict)
#' \dontrun{plot(vis)}
#' 
#' @name ldavis
#' @export
prepare_ldavis <- function(model, corpus, dictionary, ...){
  assert_that(!missing(model), msg = "Missing `model`")
  assert_that(!missing(corpus), msg = "Missing `corpus`")
  assert_that(!missing(dictionary), msg = "Missing `dictionary`")
  ldavis$gensim$prepare(model, corpus, dictionary, ...)
}

#' @rdname ldavis
#' @export
show_ldavis <- function(prepared_vis) UseMethod("show_ldavis")

#' @rdname ldavis
#' @method show_ldavis pyLDAvis._prepare.PreparedData
#' @export
show_ldavis.pyLDAvis._prepare.PreparedData <- function(prepared_vis){
  ldavis$show(prepared_vis)
}

#' @name ldavis
#' @export
plot_ldavis <- function(model, corpus, dictionary){
  assert_that(!missing(model), msg = "Missing `model`")
  assert_that(!missing(corpus), msg = "Missing `corpus`")
  assert_that(!missing(dictionary), msg = "Missing `dictionary`")
  vis <- ldavis$gensim$prepare(model, corpus, dictionary)
  ldavis$show(vis)
}

#' @export
plot.pyLDAvis._prepare.PreparedData <- function(x, ...){
  ldavis$show(x)
}

#' @rdname ldavis
#' @export
ldavis_as_html <- function(prepared_vis) UseMethod("ldavis_as_html")

#' @method ldavis_as_html pyLDAvis._prepare.PreparedData
#' @export
ldavis_as_html.pyLDAvis._prepare.PreparedData <- function(prepared_vis){
  ldavis$prepared_data_to_html(prepared_vis)
}

#' Save Visualisation
#' 
#' Save Latent Dirichlet Allocation visualisation.
#' 
#' @param prepared_vis Prepapred vis as returned by \code{\link{ldavis}}.
#' @param file A file name to save the html or json.
#' @param ... Additional arguments from \href{https://pyldavis.readthedocs.io/en/latest/modules/API.html}{the official documentation}.
#' 
#' @examples
#' docs <- prepare_documents(corpus)
#' dict <- corpora_dictionary(docs)
#' corpora <- doc2bow(dict, docs)
#' 
#' # lda model
#' model <- model_lda(
#'   corpus = corpora, 
#'   id2word = dict, 
#'   iterations = 50L, 
#'   num_topics = 2L
#' )
#' 
#' # visualise
#' vis <- prepare_ldavis(model, corpora, dict)
#' \dontrun{save_ldavis_html(vis, "lda.html")}
#' 
#' @return Invisibly returns \code{file}.
#' 
#' @name ldavis_save
#' @export
save_ldavis_html <- function(prepared_vis, file, ...) UseMethod("save_ldavis_html")

#' @method save_ldavis_html pyLDAvis._prepare.PreparedData
#' @export
save_ldavis_html.pyLDAvis._prepare.PreparedData <- function(prepared_vis, file, ...){
  assert_that(!missing(file), msg = "Missing `file`")
  ldavis$save_html(prepared_vis, file)
  invisible(file)
}

#' @rdname ldavis_save
#' @export
save_ldavis_json <- function(prepared_vis, file) UseMethod("save_ldavis_json")

#' @method save_ldavis_json pyLDAvis._prepare.PreparedData
#' @export
save_ldavis_json.pyLDAvis._prepare.PreparedData <- function(prepared_vis, file){
  assert_that(!missing(file), msg = "Missing `file`")
  ldavis$save_json(prepared_vis, file)
  invisible(file)
}