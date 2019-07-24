#' Create a dictionary
#' 
#' Create a dictionary from a list of documents.
#' 
#' @param docs A list of documents as returned by \code{\link{preprocess}}.
#' @param file Path to save \code{.dict}, If \code{NULL} the dictionary is \emph{not} saved.
#' 
#' @export
corpora_dictionary <- function(docs, file = NULL){
  assert_that(!missing(docs), msg = "Missing `docs`")
  docs <- docs %>% 
    unname() %>% 
    reticulate::tuple()
  dictionary <- gensim$corpora$Dictionary(docs)
  if(!is.null(file))
    dictionary$save(file)
  invisible(dictionary)
}

#' Create a dictionary
#' 
#' Create a dictionary from a list of documents.
#' 
#' @param dictionary A dictionary as returned by \code{\link{corpora_dictionary}}.
#' @param docs A list of documents as returned by \code{\link{preprocess}}.
#' 
#' @details Counts the number of occurrences of each distinct word, 
#' converts the word to its integer word id and returns the result as a sparse vector. 
#' 
#' @return A sparse matrix in the form, \link[reticulate]{tuple}.
#' 
#' @export
doc2bow <- function(dictionary, docs){
  assert_that(!missing(dictionary), msg = "Missing `dictionary`")
  assert_that(!missing(docs), msg = "Missing `docs`")
  docs %>% 
    purrr::map(dictionary$doc2bow) %>% 
    unname() %>% 
    reticulate::tuple() %>% 
    invisible()
}

#' Serialise Matrix Market Corpus
#' 
#' Wrap a term-document matrix on disk.
#'
#' @param corpus A corpus as returned by \code{\link{doc2bow}}.
#' @param file Path to a \code{.mm} file (recommended), if \code{NULL} it is saved to a temp file.
#' 
#' @return An object of class \code{mm} which holds the path to the \code{file} and metadata.
#' 
#' @name mmcorpus_serialize
#' 
#' @export
mmcorpus_serialize <- function(corpus, file = NULL){
  assert_that(!missing(corpus), msg = "Missing `corpus`")

  temp <- FALSE
  if(is.null(file)){
    temp <- TRUE
    file <- tempfile(fileext = ".mm")
  }

  gensim$corpora$MmCorpus$serialize(file, corpus)

  .as_mm(file, temp = temp)
}

#' @rdname mmcorpus_serialize
#' @export
as_serialized_mmcorpus <- function(file){
  .as_mm(file, temp = FALSE)
}

.as_mm <- function(file, temp = FALSE){
  obj <- list(file = file, temp = temp)
  structure(obj, class = c(class(obj), "mm"))
}

#' @export
print.mm <- function(x, ...){
  tick_cross <- ifelse(x$temp, crayon::green(cli::symbol$tick), crayon::red(cli::symbol$cross))
  cat(
    crayon::blue(cli::symbol$info), "Path:", x$file, "\n",
    tick_cross, "Temp file", "\n"
  )
}

#' Read Serialized Matrix Market
#' 
#' Read Serialized Matrix Market from path or \code{\link{as_serialized_mmcorpus}}.
#' 
#' @param file path or return value of \code{\link{as_serialized_mmcorpus}}.
#' 
#' @name read_serialized_mmcorpus
#' @export
read_serialized_mmcorpus <- function(file) UseMethod("read_serialized_mmcorpus")

#' @rdname read_serialized_mmcorpus
#' @method read_serialized_mmcorpus mm
#' @export
read_serialized_mmcorpus.mm <- function(file){
  assert_that(!missing(file), msg = "Missing `file`")
  gensim$corpora$MmCorpus(file$file)
}

#' @rdname read_serialized_mmcorpus
#' @method read_serialized_mmcorpus character
#' @export
read_serialized_mmcorpus.character <- function(file){
  assert_that(!missing(file), msg = "Missing `file`")
  gensim$corpora$MmCorpus(file)
}

#' Transform Corpora
#'
#' Trasnform a corpus based on a model.
#' 
#' @param model A model as returned, for instance by \code{\link{model_tfidf}}.
#' @param corpus A corpus as returned by \code{\link{doc2bow}}.
#' 
#' @export
corpora_transform <- function(model, corpus){
  model[corpus]
}
