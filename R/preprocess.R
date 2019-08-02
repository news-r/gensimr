#' Prepare Documents
#' 
#' Simple text preprocessor for, namely for example purposes.
#' 
#' @param data A \code{data.frame} containing \code{text} and \code{id} 
#' where each row represent a document or a \code{character} vector of text containing documents.
#' @param text A bare column name or a vector of documents.
#' @param doc_id Id of documents, if omitted they are created dynamically 
#' assuming each element of \code{text}.
#' @param min_freq Minimum term frequency to keep terms in. 
#' @param lexicon Name of a lexicon of stopwords, borrowed from \link[tidytext]{stop_words}.
#' @param ... Any other parameters.
#' @param return_doc_id Whether to return document id (named list).
#' 
#' @return A named \code{list} of documents where the names are the documents \code{id}.
#' 
#' @details Simply tokenises each document, removes punctuation, stop words, digits,
#' and keeps only terms that appear more than \code{min_freq} \emph{across documents.} 
#' 
#' @name prepare_documents
#' @export
prepare_documents <- function(data, ...) UseMethod("prepare_documents")

#' @rdname prepare_documents
#' @method prepare_documents data.frame
#' @export
prepare_documents.data.frame <- function(data, text, doc_id = NULL, min_freq = 1, 
  lexicon = c("SMART", "snowball", "onix"), ..., return_doc_id = FALSE){

  assert_that(!missing(data), msg = "Missing `data`")
  assert_that(!missing(text), msg = "Missing `text`")

  text_enquo <- dplyr::enquo(text)
  id_enquo <- dplyr::enquo(doc_id)

  tokens <- data %>% 
    dplyr::select(
      text = !!text_enquo, 
      id = !!id_enquo
    ) 

  cat(crayon::yellow(cli::symbol$arrow_right), "Preprocessing", crayon::blue(nrow(tokens)), "documents\n")

  if(!"id" %in% names(tokens))
    tokens <- dplyr::mutate(tokens, id = 1:dplyr::n())

  .get_tokens(tokens, min_freq, lexicon = lexicon, return_doc_id = return_doc_id)
}

#' @rdname prepare_documents
#' @method prepare_documents character
#' @export
prepare_documents.character <- function(data, doc_id = NULL, min_freq = 1, 
  lexicon = c("SMART", "snowball", "onix"), ..., return_doc_id = FALSE){

  cat(crayon::yellow(cli::symbol$arrow_right), "Preprocessing", crayon::blue(length(data)), "documents\n")

  if(is.null(doc_id))
    doc_id <- 1:length(data)
  
  tokens <- tibble::tibble(
    text = data,
    id = doc_id
  )

  .get_tokens(tokens, min_freq, lexicon = lexicon, return_doc_id = return_doc_id)
}

#' @rdname prepare_documents
#' @method prepare_documents factor
#' @export
prepare_documents.factor <- prepare_documents.character

.get_tokens <- function(tokens, min_freq = 1, lexicon = c("SMART", "snowball", "onix"), return_doc_id = return_doc_id){

  assert_that(min_freq >= 0)
  lexicon <- match.arg(lexicon)

  sw <- tidytext::stop_words %>% 
    dplyr::filter(lexicon == lexicon)

  tokens <- tidytext::unnest_tokens(tokens, word, text) %>% 
    dplyr::anti_join(sw, by = "word")

  token_count <- dplyr::count(tokens, word) %>% 
    dplyr::filter(n > min_freq)

  filtered <- tokens %>% 
    dplyr::inner_join(token_count, by = "word") %>% 
    dplyr::group_by(id) %>% 
    tidyr::nest(word)

  filtered$data <- filtered$data %>% 
    purrr::map(unlist) %>% 
    purrr::map(unname)
  
  if(return_doc_id)
    ids <- dplyr::pull(filtered, id) %>% unique()

  filtered <- filtered %>% 
    dplyr::pull(data) %>% 
    lapply(as.list)

  if(return_doc_id)
    filtered <- purrr::set_names(filtered, ids)

  cat(crayon::yellow(cli::symbol$arrow_left), crayon::blue(length(filtered)), "documents after perprocessing\n")

  invisible(filtered)
}

#' Author Document Preprocess
#' 
#' Simply reshapes an authors to document data.frame to a format gensim expects.  
#' 
#' @param data A data.frame.
#' @param author Bare column name containing author names.
#' @param doc Bare column name of document ids. 
#' 
#' @return A Python \code{dict}.
#' 
#' @export
auth2doc <- function(data, author, doc){
  assert_that(!missing(data), msg = "Missing `data`")
  assert_that(!missing(author), msg = "Missing `author`")
  assert_that(!missing(doc), msg = "Missing `doc`")

  author_enquo <- dplyr::enquo(author)
  doc_enquo <- dplyr::enquo(doc)

  data <- data %>% 
    dplyr::select(
      authors = !!author_enquo, 
      documents = !!doc_enquo
    ) %>% 
    dplyr::mutate(documents = as.integer(documents)) %>% 
    tidyr::nest(documents)

  data$data <- data$data %>% 
    purrr::map(unlist) %>% 
    purrr::map(unname) 

  docs <- dplyr::pull(data, "data")
  names(docs) <- dplyr::pull(data, "authors")

  docs <- docs %>% 
    reticulate::dict()

  structure(docs, class = c(class(docs), "auth2doc"))
}