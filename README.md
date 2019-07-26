
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Travis build
status](https://travis-ci.org/news-r/gensimr.svg?branch=master)](https://travis-ci.org/news-r/gensimr)
<!-- badges: end -->

![gensim official
logo](https://radimrehurek.com/gensim/_static/images/gensim.png)

Brings [gensim](https://radimrehurek.com/gensim) to R: efficient
large-scale topic modeling.

⚠️ Notice the “Experimental” lifecycle badge: things won’t work, stuff
will break.

  - [Installation](#installation)
  - [Preprocessing](#preprocessing)
  - [Topic Modeling](#topic-modeling)
  - [Document Similarity](#document-similarity)
  - [External Data & Models](#external-data-models)
  - [Word Vectors](#word-vectors)

## Installation

Install the package.

``` r
# install.packages("remotes")
remotes::install_github("news-r/gensimr")
```

Install the python dependency.

*Make sure you have a C compiler before installing Gensim, to use the
optimized word2vec routines (70x speedup compared to plain NumPy
implementation).*

``` r
gensimr::install_gensim()
```

Ideally one should use a virtual environment and pass it to
`install_gensim`, only do this once.

``` r
# replace with path of your choice
my_env <- "./env"

# run this (works on unix)
args <- paste("-m venv", env)
system2("python3", args) # create environment
reticulate::use_virtualenv(my_env) # force reticulate to use env
gensimr::install_gensim(my_env) # install gensim in environment
gensimr::install_sklearn(my_env) # install if you want to use the sklearn API
```

## Preprocessing

First we preprocess the corpus using example data, a tiny corpus of 9
documents. Reproducing the tutorial on [corpora and vector
spaces](https://radimrehurek.com/gensim/tut1.html).

``` r
library(gensimr)

data(corpus, package = "gensimr")

print(corpus)
#> [1] "Human machine interface for lab abc computer applications"    
#> [2] "A survey of user opinion of computer system response time"    
#> [3] "The EPS user interface management system"                     
#> [4] "System and human system engineering testing of EPS"           
#> [5] "Relation of user perceived response time to error measurement"
#> [6] "The generation of random binary unordered trees"              
#> [7] "The intersection graph of paths in trees"                     
#> [8] "Graph minors IV Widths of trees and well quasi ordering"      
#> [9] "Graph minors A survey"

docs <- preprocess(corpus)
#> → Preprocessing 9 documents
#> ← 9 documents after perprocessing

docs[[1]] # print first preprocessed document 
#> [[1]]
#> [1] "human"
#> 
#> [[2]]
#> [1] "interface"
#> 
#> [[3]]
#> [1] "computer"
```

Once preprocessed we can build a dictionary.

``` r
dictionary <- corpora_dictionary(docs)
```

A dictionary essentially assigns an integer to each term.

`doc2bow` simply applies the method of the same name to every documents
(see example below); it counts the number of occurrences of each
distinct word, converts the word to its integer word id and returns the
result as a sparse vector.

``` r
# native method to a single document
dictionary$doc2bow(docs[[1]])
#> [(0, 1), (1, 1), (2, 1)]

# apply to all documents
corpus_bow <- doc2bow(dictionary, docs)
```

Then serialise to matrix market format, the function returns the path to
the file (this is saved on disk for efficiency), if no path is passed
then a temp file is created. Here we set `auto_delete` to `FALSE`
otherwise the corpus is deleted after first use. Note this means you
should manually delete it with `delete_mmcorpus`.

``` r
(corpus_mm <- serialize_mmcorpus(corpus_bow, auto_delete = FALSE))
#> ℹ Path: /var/folders/n9/ys9t1h091jq80g4hww24v8g0n7v578/T//RtmplCE2Ir/file20fe727c38a2.mm 
#>  ✔ Temp file
#>  ✖ Delete after use
```

Then initialise a model, we’re going to use a Latent Similarity Indexing
method later on (`model_lsi`) which requires td-idf.

``` r
tfidf <- model_tfidf(corpus_mm)
```

We can then use the model to transform our original corpus.

``` r
corpus_transformed <- wrap(tfidf, corpus_bow)
```

## Topic Modeling

Finally, we can build models, the number of topics of `model_*`
functions defautls to 2, which is too low for what we generally would do
with gensimr but works for the low number of documents we have. Below we
reproduce bits and bobs of the [topics and
transformation](https://radimrehurek.com/gensim/tut2.html).

### Latent Similarity Index

Note that we use the transformed corpus.

``` r
lsi <- model_lsi(corpus_transformed, id2word = dictionary)
#> ⚠ Low number of topics
lsi$print_topics()
#> [(0, '0.703*"trees" + 0.538*"graph" + 0.402*"minors" + 0.187*"survey" + 0.061*"system" + 0.060*"time" + 0.060*"response" + 0.058*"user" + 0.049*"computer" + 0.035*"interface"'), (1, '0.460*"system" + 0.373*"user" + 0.332*"eps" + 0.328*"interface" + 0.320*"time" + 0.320*"response" + 0.293*"computer" + 0.280*"human" + 0.171*"survey" + -0.161*"trees"')]
```

We can then wrap the model around the corpus to extract further
information, below we extract how each document contribute to each
dimension (topic).

``` r
wrapped_corpus <- wrap(lsi, corpus_transformed)
(wrapped_corpus_docs <- get_docs_topics(wrapped_corpus))
#> # A tibble: 9 x 4
#>   dimension_1_x dimension_1_y dimension_2_x dimension_2_y
#>           <dbl>         <dbl>         <dbl>         <dbl>
#> 1             0        0.0660             1        0.520 
#> 2             0        0.197              1        0.761 
#> 3             0        0.0899             1        0.724 
#> 4             0        0.0759             1        0.632 
#> 5             0        0.102              1        0.574 
#> 6             0        0.703              1       -0.161 
#> 7             0        0.877              1       -0.168 
#> 8             0        0.910              1       -0.141 
#> 9             0        0.617              1        0.0539
plot(wrapped_corpus_docs$dimension_1_y, wrapped_corpus_docs$dimension_2_y)
```

<img src="man/figures/README-unnamed-chunk-9-1.png" width="100%" />

### Random Projections

Note that we use the transformed corpus.

``` r
rp <- model_rp(corpus_transformed, id2word = dictionary)
#> ⚠ Low number of topics

wrapped_corpus <- wrap(rp, corpus_transformed)
wrapped_corpus_docs <- get_docs_topics(wrapped_corpus)
plot(wrapped_corpus_docs$dimension_1_y, wrapped_corpus_docs$dimension_2_y)
```

<img src="man/figures/README-unnamed-chunk-10-1.png" width="100%" />

### Latent Dirichlet Allocation

Note that we use the original, non-transformed corpus.

``` r
lda <- model_lda(corpus_mm, id2word = dictionary, num_topics = 2)
lda_topics <- lda$get_document_topics(corpus_bow)
wrapped_corpus_docs <- get_docs_topics(lda_topics)
plot(wrapped_corpus_docs$dimension_1_y, wrapped_corpus_docs$dimension_2_y)
```

<img src="man/figures/README-unnamed-chunk-11-1.png" width="100%" />

### Hierarchical Dirichlet Process

``` r
hdp <- model_hdp(corpus_mm, id2word = dictionary)
reticulate::py_to_r(hdp$show_topic(topic_id = 1L, topn = 5L))
#> [[1]]
#> [[1]][[1]]
#> [1] "time"
#> 
#> [[1]][[2]]
#> [1] 0.2154603
#> 
#> 
#> [[2]]
#> [[2]][[1]]
#> [1] "computer"
#> 
#> [[2]][[2]]
#> [1] 0.1712531
#> 
#> 
#> [[3]]
#> [[3]][[1]]
#> [1] "system"
#> 
#> [[3]][[2]]
#> [1] 0.1402072
#> 
#> 
#> [[4]]
#> [[4]][[1]]
#> [1] "graph"
#> 
#> [[4]][[2]]
#> [1] 0.1334292
#> 
#> 
#> [[5]]
#> [[5]][[1]]
#> [1] "user"
#> 
#> [[5]][[2]]
#> [1] 0.1169253
```

### Log Entropy

``` r
log_entropy <- model_logentropy(corpus_bow)
vector <- wrap(log_entropy, corpus_bow)
```

## Document Similarity

Reproducing [tutorial on
similarity](https://radimrehurek.com/gensim/tut3.html#similarity-interface).

``` r
mm <- read_serialized_mmcorpus(corpus_mm)

new_document <- "A human and computer interaction"
preprocessed_new_document <- preprocess(new_document, min_freq = 0)
#> → Preprocessing 1 documents
#> ← 1 documents after perprocessing
vec_bow <- doc2bow(dictionary, preprocessed_new_document)
vec_lsi <- wrap(lsi, vec_bow)

wrapped_lsi <- wrap(lsi, mm)
index <- similarity_matrix(wrapped_lsi)

sims <- wrap(index, vec_lsi)

get_similarity(sims)
#> # A tibble: 9 x 2
#>     doc  cosine
#>   <dbl>   <dbl>
#> 1     2  1.000 
#> 2     0  1.000 
#> 3     3  1.000 
#> 4     4  0.999 
#> 5     1  0.995 
#> 6     8  0.194 
#> 7     7 -0.0237
#> 8     6 -0.0516
#> 9     5 -0.0880
```

## External Data & Models

You can download external datasets to easily build models. External
dataset can be found on
[RaRe-Technologies/gensim-data](https://github.com/RaRe-Technologies/gensim-data).

``` r
dataset <- "glove-twitter-25"

# model description
downloader_info(dataset) %>% 
  reticulate::py_to_r() %>% 
  .[["description"]]
#> [1] "Pre-trained vectors based on 2B tweets, 27B tokens, 1.2M vocab, uncased (https://nlp.stanford.edu/projects/glove/)."

# download the model
model <- downloader_load(dataset)

# find words most similar to "cat"
model$most_similar("cat") %>% 
  reticulate::py_to_r()
#> [[1]]
#> [[1]][[1]]
#> [1] "dog"
#> 
#> [[1]][[2]]
#> [1] 0.9590819
#> 
#> 
#> [[2]]
#> [[2]][[1]]
#> [1] "monkey"
#> 
#> [[2]][[2]]
#> [1] 0.9203578
#> 
#> 
#> [[3]]
#> [[3]][[1]]
#> [1] "bear"
#> 
#> [[3]][[2]]
#> [1] 0.9143137
#> 
#> 
#> [[4]]
#> [[4]][[1]]
#> [1] "pet"
#> 
#> [[4]][[2]]
#> [1] 0.9108031
#> 
#> 
#> [[5]]
#> [[5]][[1]]
#> [1] "girl"
#> 
#> [[5]][[2]]
#> [1] 0.888063
#> 
#> 
#> [[6]]
#> [[6]][[1]]
#> [1] "horse"
#> 
#> [[6]][[2]]
#> [1] 0.8872727
#> 
#> 
#> [[7]]
#> [[7]][[1]]
#> [1] "kitty"
#> 
#> [[7]][[2]]
#> [1] 0.8870542
#> 
#> 
#> [[8]]
#> [[8]][[1]]
#> [1] "puppy"
#> 
#> [[8]][[2]]
#> [1] 0.8867697
#> 
#> 
#> [[9]]
#> [[9]][[1]]
#> [1] "hot"
#> 
#> [[9]][[2]]
#> [1] 0.8865255
#> 
#> 
#> [[10]]
#> [[10]][[1]]
#> [1] "lady"
#> 
#> [[10]][[2]]
#> [1] 0.8845519
```

### Word Vectors

Word2vec works somewhat differently. The example below is a reproduction
of the Kaggle [Gensim Word2Vec
Tutorial](https://www.kaggle.com/pierremegret/gensim-word2vec-tutorial#Training-the-model).

``` r
# initialise
word2vec <- model_word2vec(size = 100L, window = 5L, min_count = 1L)
word2vec$build_vocab(docs) 
#> None
word2vec$train(docs, total_examples = word2vec$corpus_count, epochs = 20L)
#> (76, 580)
word2vec$init_sims(replace = TRUE)
#> None
```

Now we can explore the model.

``` r
word2vec$wv$most_similar(positive = c("interface"))
#> [('trees', 0.2601749300956726), ('minors', 0.12357524037361145), ('computer', 0.074931301176548), ('response', 0.07039420306682587), ('time', 0.05693456530570984), ('system', -0.041648197919130325), ('user', -0.048062846064567566), ('human', -0.09219866991043091), ('eps', -0.15730395913124084), ('survey', -0.169086754322052)]
```

We expect “trees” to be the odd one out, it is a term that was in a
different topic (\#2) whereas other terms were in topics \#1.

``` r
word2vec$wv$doesnt_match(c("human", "interface", "trees"))
#> human
```

Test similarity between words.

``` r
word2vec$wv$similarity("human", "trees")
#> -0.063984536
word2vec$wv$similarity("eps", "system")
#> -0.14588615
```

## Scikit-learn

Scikit learn API.

### Author-topic model

``` r
# authors of corpus
data("authors", package = "gensimr")

auth2doc <- auth2doc(authors, name, document)

temp <- tempfile("serialized")
atmodel <- sklearn_atmodel(
  corpus_mm, 
  id2word = dictionary, 
  author2doc = auth2doc, 
  num_topics = 2L, 
  serialized = TRUE,
  serialization_path = temp
)
unlink(temp, recursive = TRUE)

atmodel$get_author_topics("jack") # single author 
#> [(0, 0.19926858269337291), (1, 0.8007314173066271)]

# apply to all authors
get_author_topics(atmodel)
#> # A tibble: 3 x 5
#>   authors dimension_1_x dimension_1_y dimension_2_x dimension_2_y
#>   <chr>           <dbl>         <dbl>         <dbl>         <dbl>
#> 1 jack                0         0.199             1         0.801
#> 2 jane                0         0.143             1         0.857
#> 3 john                0         0.142             1         0.858
```

Clean up, delete the corpus.

``` r
delete_mmcorpus(corpus_mm)
```
