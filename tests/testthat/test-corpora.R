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
  stripped_df <- strip_short(corpus, min_len = 10, txt)
  expect_equal(stripped[[1]], "applications")
  expect_equal(stripped_df[[1]], "applications")

  # strip multiple spaces
  stripped <- strip_multiple_spaces(c("x  "))
  stripped_df <- strip_multiple_spaces(tibble::tibble(txt = c("x  ")), txt)
  expect_equal(stripped, "x ")
  expect_equal(stripped_df, "x ")

  # strip non alphanumerics
  stripped <- strip_non_alphanum(c("x."))
  stripped_df <- strip_non_alphanum(tibble::tibble(txt = c("x.")), txt)
  expect_equal(stripped, "x ")
  expect_equal(stripped_df, "x ")

  # strip numeric
  stripped <- strip_numeric(c("x1"))
  stripped_df <- strip_numeric(tibble::tibble(txt = c("x1")), txt)
  expect_equal(stripped, "x")
  expect_equal(stripped_df, "x")

  # strip punctuation
  stripped <- strip_punctuation(c("x."))
  stripped_df <- strip_punctuation(tibble::tibble(txt = c("x.")), txt)
  expect_equal(stripped, "x ")
  expect_equal(stripped_df, "x ")

  # strip tags
  stripped <- strip_tags(c("<span>x</span>"))
  stripped_df <- strip_tags(tibble::tibble(txt = c("<span>x</span>")), txt)
  expect_equal(stripped, "x")
  expect_equal(stripped_df, "x")

  # split alphanum
  stripped <- split_alphanum(c("x1"))
  stripped_df <- split_alphanum(tibble::tibble(txt = c("x1")), txt)
  expect_equal(stripped, "x 1")
  expect_equal(stripped_df, "x 1")

  # remove stopwords
  stripped <- remove_stopwords(c("a dog"))
  stripped_df <- remove_stopwords(tibble::tibble(txt = c("as dog")), txt)
  expect_equal(stripped, "dog")
  expect_equal(stripped_df, "dog")

  # preprocess
  expected <- c("graph", "minor", "survei")
  stripped <- preprocess(corpus)
  stripped_df <- preprocess(corpus_df, txt)
  expect_equal(stripped[[9]], expected)
  expect_equal(stripped_df[[9]], expected)
})

test_that("corpora_dictionary works", {
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
})