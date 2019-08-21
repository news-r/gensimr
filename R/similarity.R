#' Similarity Matrix
#' 
#' @param corpus Corpus.
#' @param ... Any other arguments.
#' 
#' @name similarity_matrix
#' 
#' @export
similarity_matrix <- function(corpus, ...) UseMethod("similarity_matrix")

#' @rdname similarity_matrix
#' @method similarity_matrix wrapped
#' @export
similarity_matrix.wrapped <- function(corpus, ...){
  gensim$similarities$MatrixSimilarity(corpus)
}

#' Get Similarity
#' 
#' Returns Documents by order of similarity.
#' 
#' @param sim A wrapped similarity, output of \code{\link{wrap}}.
#' 
#' @return A \link[tibble]{tibble} Documents by order of similarity, 
#' the most similar at the top and the least similar at the bottom.
#' 
#' @name get_similarity
#' 
#' @export
get_similarity <- function(sim) UseMethod("get_similarity")

#' @rdname get_similarity
#' @method get_similarity wrapped
#' @export
get_similarity.wrapped <- function(sim){
  enum <- bi$enumerate(sim)
  lst <- bi$list(enum)
  lst <- unlist(lst[[1]][[2]])
  tibble::tibble( 
    doc = 1:length(lst),
    cosine = lst
  ) %>% 
    dplyr::mutate(doc = doc - 1) %>% 
    dplyr::arrange(-cosine)
}

#' Similarity
#' 
#' Splits the index into several smaller sub-indexes (“shards”), 
#' which are disk-based. If your entire index fits in memory 
#' (~one million documents per 1GB of RAM), you can also use 
#' the \code{\link{similarity_matrix}}. It is more simple but
#' does not scale as well: it keeps the entire index in RAM, 
#' no sharding. It also do not support adding new document 
#' to the index dynamically.
#' 
#' @param corpus A corpus.
#' @param num_features Size of the dictionary i.e.:\code{reticulate::py_len(dictionary)}.
#' @param ... Any other parameters to pass to the Python function, see 
#' \href{https://radimrehurek.com/gensim/similarities/docsim.html#gensim.similarities.docsim.Similarity}{official documentation}.
#' 
#' @name similarity
#' @export
similarity <- function(corpus, ...) UseMethod("similarity")

#' @rdname similarity
#' @method similarity gensim.corpora.mmcorpus.MmCorpus
#' @export
similarity.gensim.corpora.mmcorpus.MmCorpus <- function(corpus, num_features, ...){
  file <- tempdir()
  num_features <- as.integer(num_features)
  gensim$similarities$Similarity(file, corpus, num_features, ...)
}

#' @rdname similarity
#' @method similarity mm_file
#' @export
similarity.mm_file <- function(corpus, num_features, ...){
  file <- tempdir()
  num_features <- as.integer(num_features)
  corpus <- read_serialized_mmcorpus(corpus)
  gensim$similarities$Similarity(file, corpus, num_features, ...)
}

#' @rdname similarity
#' @method similarity python.builtin.tuple
#' @export
similarity.python.builtin.tuple <- function(corpus, num_features, ...){
  file <- tempdir()
  num_features <- as.integer(num_features)
  gensim$similarities$Similarity(file, corpus, num_features, ...)
}
