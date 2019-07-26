corpus <- c(
  "Human machine interface for lab abc computer applications",
  "A survey of user opinion of computer system response time",
  "The EPS user interface management system",
  "System and human system engineering testing of EPS",
  "Relation of user perceived response time to error measurement",
  "The generation of random binary unordered trees",
  "The intersection graph of paths in trees",
  "Graph minors IV Widths of trees and well quasi ordering",
  "Graph minors A survey"
)

authors <- tibble::tibble(
  name = c(
    rep("john", 7),
    rep("jane", 7),
    rep("jack", 5)
  ),
  document = c(
    0, 1, 2, 3, 4, 5, 6,
    2, 3, 4, 5, 6, 7, 8,
    0, 2, 4, 6, 8
  )
) %>% 
  dplyr::mutate(document = as.integer(document))

usethis::use_data(corpus, authors, overwrite = TRUE)
