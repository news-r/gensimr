
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental) [![Travis build status](https://travis-ci.org/news-r/gensimr.svg?branch=master)](https://travis-ci.org/news-r/gensimr) <!-- badges: end -->

gensimr
=======

Brings [gensim](https://radimrehurek.com/gensim) to R: efficient large-scale topic modeling.

Installation
------------

Install the package.

``` r
# install.packages("remotes")
remotes::install_github("news-r/gensimr")
```

Install the python dependency.

``` r
gensimr::install_gensim()
```

Ideally one should use a virtual environment and pass it to `install_gensim`, only do this once.

``` r
my_env <- "./env"
args <- paste("-m venv", env)
system2("python3", args) # create environment
reticulate::use_virtualenv(my_env) # force reticulate to use env
gensimr::install_gensim(my_env) # install gensim in environment
```

Example
-------

First we preprocess the corpus using example data, a tiny corpus of 9 documents.

``` r
library(gensimr)

data(corpus, package = "gensimr")

docs <- preprocess(corpus)
#> → Preprocessing 9 documents
#> ← 9 documents after perprocessing
```

Once preprocessed we can build a dictionary.

``` r
dictionary <- corpora_dictionary(docs)
```

A dictionary essentially assigns an integer to each term.

``` r
reticulate::py_to_r(dictionary$token2id)
#> $computer
#> [1] 0
#> 
#> $human
#> [1] 1
#> 
#> $interface
#> [1] 2
#> 
#> $response
#> [1] 3
#> 
#> $survey
#> [1] 4
#> 
#> $system
#> [1] 5
#> 
#> $time
#> [1] 6
#> 
#> $user
#> [1] 7
#> 
#> $eps
#> [1] 8
#> 
#> $trees
#> [1] 9
#> 
#> $graph
#> [1] 10
#> 
#> $minors
#> [1] 11
```

`doc2bow` simply applies the method of the same name to every documents (see example below); it counts the number of occurrences of each distinct word, converts the word to its integer word id and returns the result as a sparse vector.

``` r
# native method to a single document
dictionary$doc2bow(docs[[1]])
#> [(0, 1), (1, 1), (2, 1)]

# apply to all documents
corpus <- doc2bow(dictionary, docs)
```

Then convert to matrix market format and serialise, the function returns the path to the file (this is saved on disk for efficiency), if no path is passed then a temp file is created.

``` r
(mm_corpus <- mmcorpus_serialize(corpus))
#> ℹ Path: /tmp/RtmpHPdgNZ/file208f30891be5.mm 
#>  ✔ Temp file
```

Then initialise a model, we're going to use a Latent Similarity Indexing method later on (`model_lsi`) which requires td-idf.

``` r
model <- model_tfidf(mm_corpus)
```

We can then use the model to transform our original corpus.

``` r
corpus_transformed <- corpora_transform(model, corpus)
```

Finally, we build the model, the number of topics defautls to 2.

``` r
lsi <- model_lsi(corpus_transformed, dictionary)
lsi$print_topics()
#> [(0, '0.703*"trees" + 0.538*"graph" + 0.402*"minors" + 0.187*"survey" + 0.061*"system" + 0.060*"response" + 0.060*"time" + 0.058*"user" + 0.049*"computer" + 0.035*"interface"'), (1, '0.460*"system" + 0.373*"user" + 0.332*"eps" + 0.328*"interface" + 0.320*"response" + 0.320*"time" + 0.293*"computer" + 0.280*"human" + 0.171*"survey" + -0.161*"trees"')]
```

We can then wrap the model around the corpus to extract further information, below we extract how each document contribute to each dimension (topic).

``` r
wrapped_corpus <- wrap_corpus(lsi, corpus_transformed)
(wrapped_corpus_docs <- wrap_corpus_docs(wrapped_corpus))
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
```
