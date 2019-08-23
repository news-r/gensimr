test_that("poincare works", {
  path <- datapath('poincare_hypernyms_large.tsv')
  poincare <- model_poincare(path)
  expect_type(poincare, "environment")
  poincare$train(epochs = 50L)

  dist <- poincare$kv$distance('mammal.n.01', 'carnivore.n.01')
  expect_gt(reticulate::py_to_r(dist), 2.97)
})
