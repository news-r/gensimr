test_that("prepare_documents works", {
  # --------------- vector method
  expect_error(prepare_documents())
  texts <- prepare_documents(corpus)
  expect_length(texts, 9)

  # min_freq
  texts <- prepare_documents(corpus, min_freq = 3)
  expect_length(texts, 3)

  # doc_id
  texts <- prepare_documents(corpus, doc_id = 1:9)
  expect_length(texts, 9)
  texts <- prepare_documents(corpus, doc_id = 1:9, return_doc_id = TRUE)
  expect_equal(names(texts), as.character(1:9))

  # --------------- data.frame method
  corpus_df <- tibble::tibble(txt = corpus, id = 1:9)
  expect_error(prepare_documents(corpus_df))

  # min_freq
  texts <- prepare_documents(corpus_df, text = txt, doc_id = id, min_freq = 4)
  expect_length(texts, 0)

  # doc_id
  texts <- prepare_documents(corpus_df, text = txt, doc_id = id)
  expect_length(texts, 9)
  texts <- prepare_documents(corpus_df, text = txt, doc_id = id, return_doc_id = TRUE)
  expect_equal(names(texts), as.character(1:9))
})

test_that("parsing works", {

  # data.frame for test
  corpus_df <- tibble::tibble(txt = corpus, id = 1:9)

  # errors
  expect_error(strip_short())
  expect_error(strip_multiple_spaces())
  expect_error(strip_non_alphanum())
  expect_error(strip_numeric())
  expect_error(strip_punctuation())
  expect_error(strip_tags())
  expect_error(split_alphanum())
  expect_error(preprocess())
  expect_error(remove_stopwords())

  # strip short
  stripped <- strip_short(corpus, min_len = 10)
  stripped_list <- strip_short(as.list(corpus), min_len = 10)
  stripped_df <- strip_short(corpus_df, min_len = 10, txt)
  expect_equal(stripped[[1]], "applications")
  expect_equal(stripped_df[[1]], "applications")
  expect_equal(stripped_list[[1]], "applications")

  # strip multiple spaces
  stripped <- strip_multiple_spaces(c("x  "))
  stripped_list <- strip_multiple_spaces(list("x  "))
  stripped_df <- strip_multiple_spaces(tibble::tibble(txt = c("x  ")), txt)
  expect_equal(stripped, "x ")
  expect_equal(stripped_df, "x ")
  expect_equal(stripped_list, "x ")

  # strip non alphanumerics
  stripped <- strip_non_alphanum(c("x."))
  stripped_list <- strip_non_alphanum(list("x."))
  stripped_df <- strip_non_alphanum(tibble::tibble(txt = c("x.")), txt)
  expect_equal(stripped, "x ")
  expect_equal(stripped_df, "x ")
  expect_equal(stripped_list, "x ")

  # strip numeric
  stripped <- strip_numeric(c("x1"))
  stripped_list <- strip_numeric(list("x1"))
  stripped_df <- strip_numeric(tibble::tibble(txt = c("x1")), txt)
  expect_equal(stripped, "x")
  expect_equal(stripped_df, "x")
  expect_equal(stripped_list, "x")

  # strip punctuation
  stripped <- strip_punctuation(c("x."))
  stripped_list <- strip_punctuation(list("x."))
  stripped_df <- strip_punctuation(tibble::tibble(txt = c("x.")), txt)
  expect_equal(stripped, "x ")
  expect_equal(stripped_df, "x ")
  expect_equal(stripped_list, "x ")

  # strip tags
  stripped <- strip_tags(c("<span>x</span>"))
  stripped_list <- strip_tags(list("<span>x</span>"))
  stripped_df <- strip_tags(tibble::tibble(txt = c("<span>x</span>")), txt)
  expect_equal(stripped, "x")
  expect_equal(stripped_df, "x")
  expect_equal(stripped_list, "x")

  # split alphanum
  stripped <- split_alphanum(c("x1"))
  stripped_list <- split_alphanum(list("x1"))
  stripped_df <- split_alphanum(tibble::tibble(txt = c("x1")), txt)
  expect_equal(stripped, "x 1")
  expect_equal(stripped_df, "x 1")
  expect_equal(stripped_list, "x 1")

  # remove stopwords
  stripped <- remove_stopwords(c("a dog"))
  stripped_list <- remove_stopwords(list("a dog"))
  stripped_df <- remove_stopwords(tibble::tibble(txt = c("as dog")), txt)
  expect_equal(stripped, "dog")
  expect_equal(stripped_df, "dog")
  expect_equal(stripped_list, "dog")

  # filter rare
  docs <- prepare_documents(corpus)
  stripped <- filter_rare(docs, 2)
  expect_equal(stripped[[9]], list("graph"))
  expect_equal(length(docs), length(stripped))

  # preprocess
  expected <- c("graph", "minor", "survei")
  stripped <- preprocess(corpus)
  stripped_df <- preprocess(corpus_df, txt)
  expect_equal(stripped[[9]], expected)
  expect_equal(stripped_df[[9]], expected)
})

test_that("porter_stemmer and stem_words words", {
  
  # word to stem
  word <- "survey"
  expected <- "survei"

  # stemmer
  stemmer <- porter_stemmer()
  stemmed_porter <- stemmer$stem(word) %>% 
    reticulate::py_to_r()
  expect_equal(stemmed_porter, expected)

  # convenienve function
  stemmed_conv <- stem_porter(stemmer, word)
  expect_equal(stemmed_porter, stemmed_conv)

  # stem
  stemmed <- stem_text(word)
  expect_equal(stemmed, expected)
  expect_equal(stemmed, stemmed_porter)

  # test other methods
  corpus_df <- tibble::tibble(txt = corpus)
  stemmed_list <- stem_text(as.list(corpus))
  stemmed_df <- stem_text(corpus_df, text = txt)
  expect_equal(stemmed_list, stemmed_df)
})

test_that("dictionary-related funcs work works", {
  # ensure we have correct texts
  texts <- prepare_documents(corpus)

  # basic
  expect_error(corpora_dictionary())
  dictionary <- corpora_dictionary(texts)
  expect_equal(reticulate::py_len(dictionary), 12)

  # file
  temp <- tempfile()
  dictionary <- corpora_dictionary(texts, file = temp)
  expect_equal(reticulate::py_len(dictionary), 12)
  unlink(temp, force = TRUE)

  # doc2bow
  doc_bow <- doc2bow(dictionary, texts)
  expect_equal(reticulate::py_len(doc_bow), 9)

  #' serialie corpus
  (corpus_mm <- serialize_mmcorpus(doc_bow, auto_delete = FALSE))
  expect_type(corpus_mm, "list")
  read <- read_serialized_mmcorpus(corpus_mm)
  expect_type(read, "environment")
  delete_mmcorpus(corpus_mm)

  # with file
  temp <- tempfile()
  corpus_mm <- serialize_mmcorpus(doc_bow, file = temp)
  expect_type(corpus_mm, "list")
  as <- as_serialized_mmcorpus(corpus_mm$file)
  expect_type(as, "list")
  unlink(temp, force = TRUE)
})

test_that("text8 works", {
  fl <- datapath("testcorpus.txt")
  t8 <- text8corpus(fl)
  expect_type(t8, "environment")
})

test_that("test auth2doc words", {
  data("authors")

  authors_docs <- auth2doc(authors, name, document)
  expect_equal(reticulate::py_len(authors_docs), 3)
})