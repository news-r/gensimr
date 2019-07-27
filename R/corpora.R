#' Create a dictionary
#' 
#' Create a dictionary from a list of documents.
#' 
#' @param docs A list of documents as returned by \code{\link{prepare_documents}}.
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
#' @param docs A list of documents as returned by \code{\link{prepare_documents}}.
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
#' Serialise a term-document matrix on disk.
#'
#' @param corpus A corpus as returned by \code{\link{doc2bow}}.
#' @param file Path to a \code{.mm} file (recommended), if \code{NULL} it is saved to a temp file.
#' @param auto_delete Wether to automatically delete the temp file after first use.
#' 
#' @section Functions:
#' \itemize{
#'   \item{\code{serialize_mmcorpus} - Serialize the corpus}  
#'   \item{\code{as_serialized_mmcorpus} - Create an \code{mm_file} from an already created corpus file.}  
#'   \item{\code{delete_mmcorpus} - Delete temp corpus.}  
#' }
#' 
#' @return An object of class \code{mm_file} which holds the path to the \code{file} and metadata.
#' 
#' @name mmcorpus_serialize
#' 
#' @export
serialize_mmcorpus <- function(corpus, file = NULL, auto_delete = TRUE){
  assert_that(!missing(corpus), msg = "Missing `corpus`")

  if(!is.null(file) && auto_delete)
    cat(crayon::yellow(cli::symbol$warning), "`auto_delete` only works when `file` is NULL\n")

  # initialise
  temp <- delete <- FALSE
  if(is.null(file)){
    temp <- TRUE

    if(auto_delete) delete <- TRUE
    file <- tempfile(fileext = ".mm")
  }

  gensim$corpora$MmCorpus$serialize(file, corpus)

  .as_mm_file(file, temp = temp, delete = delete)
}

#' @rdname mmcorpus_serialize
#' @export
as_serialized_mmcorpus <- function(file){
  .as_mm_file(file, temp = FALSE, delete = FALSE)
}

#' @rdname mmcorpus_serialize
#' @export
delete_mmcorpus <- function(file){
  if(!file$temp)
    cat(crayon::yellow(cli::symbol$warning), "Not a temp file\n")
  else {
    cat(crayon::green(cli::symbol$tick), "Temp unlinked\n")
    unlink(file$file, recursive = TRUE, force = TRUE)
  } 
}

.as_mm_file <- function(file, temp = FALSE, delete = FALSE){
  obj <- list(file = file, temp = temp, delete = delete)
  structure(obj, class = c(class(obj), "mm_file"))
}

.as_mm <- function(mm){
  structure(mm, class = c(class(mm), "mm"))
}

#' @export
print.mm_file <- function(x, ...){
  tick_cross <- ifelse(x$temp, crayon::green(cli::symbol$tick), crayon::red(cli::symbol$cross))
  delete_use <- ifelse(x$delete, crayon::green(cli::symbol$tick), crayon::red(cli::symbol$cross))
  cat(
    crayon::blue(cli::symbol$info), "Path:", x$file, "\n",
    tick_cross, "Temp file\n",
    delete_use, "Delete after use\n"
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
#' @method read_serialized_mmcorpus mm_file
#' @export
read_serialized_mmcorpus.mm_file <- function(file){
  assert_that(!missing(file), msg = "Missing `file`")
  mm <- .as_mm(gensim$corpora$MmCorpus(file$file))
  invisible(mm)
}

#' @rdname read_serialized_mmcorpus
#' @method read_serialized_mmcorpus character
#' @export
read_serialized_mmcorpus.character <- function(file){
  assert_that(!missing(file), msg = "Missing `file`")
  mm <- .as_mm(gensim$corpora$MmCorpus(file))
  invisible(mm)
}
