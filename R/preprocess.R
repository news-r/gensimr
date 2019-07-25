#' Preprocess
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
#' 
#' @return A named \code{list} of documents where the names are the documents \code{id}.
#' 
#' @details Simply tokenises each document, removes punctuation, stop words, digits,
#' and keeps only terms that appear more than \code{min_freq} \emph{across documents.} 
#' 
#' @name preprocess
#' @export
preprocess <- function(data, ...) UseMethod("preprocess")

#' @rdname preprocess
#' @method preprocess data.frame
#' @export
preprocess.data.frame <- function(data, text, doc_id = NULL, min_freq = 1, 
  lexicon = c("SMART", "snowball", "onix"), ...){

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

  .get_tokens(tokens, min_freq)
}

#' @rdname preprocess
#' @method preprocess character
#' @export
preprocess.character <- function(data, doc_id = NULL, min_freq = 1, 
  lexicon = c("SMART", "snowball", "onix"), ...){

  cat(crayon::yellow(cli::symbol$arrow_right), "Preprocessing", crayon::blue(length(data)), "documents\n")

  if(is.null(doc_id))
    doc_id <- 1:length(data)
  
  tokens <- tibble::tibble(
    text = data,
    id = doc_id
  )

  .get_tokens(tokens, min_freq)
}

#' @rdname preprocess
#' @method preprocess factor
#' @export
preprocess.factor <- preprocess.character

.get_tokens <- function(tokens, min_freq = 1, lexicon = c("SMART", "snowball", "onix")){

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
  
  ids <- dplyr::pull(filtered, id) %>% unique()

  filtered <- filtered %>% 
    dplyr::pull(data) %>% 
    lapply(as.list) %>% 
    purrr::set_names(ids)

  cat(crayon::yellow(cli::symbol$arrow_left), crayon::blue(length(filtered)), "documents after perprocessing\n")

  invisible(filtered)
}