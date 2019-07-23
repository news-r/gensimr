
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Travis build
status](https://travis-ci.org/news-r/gensimr.svg?branch=master)](https://travis-ci.org/news-r/gensimr)
<!-- badges: end -->

# gensimr

Brings [gensim](https://radimrehurek.com/gensim) to R.

## Installation

Install the package.

``` r
install.packages("gensimr")
```

Install the python dependency.

``` r
gensimr::install_gensim()
```

Ideally one should use a virtual environment and pass it to
`install_gensim`.

``` r
system2("python3", "-m venv ./env") # create environment
reticulate::use_virtualenv("./env") # force reticulate to use env
gensimr::install_gensim("./env") # install gensim in environment
```

## Example

Use data from another [news-r](https://news-r.org) package.

``` r
# remotes::install_github("news-r/nethoser")
data("webhoser", package = "nethoser")
```

Preprocess.

``` r
library(gensimr)

preprocess(webhoser, text)
#> ✔ 272 documents preprocessed
```
